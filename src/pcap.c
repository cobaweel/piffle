/*
 * Piffle, Copyright (C) 2007, Jaap Weel. This program is free
 * software; you can redistribute it and/or modify it under the terms
 * of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option)
 * any later version.  This program is distributed in the hope that it
 * will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.  You should
 * have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA. As a special
 * exception, when this file is copied by pfc into a .c output file,
 * you may use that output file without restriction. This special
 * exception was added by the Jaap Weel, and is modeled after the
 * special exception in GNU Bison. 
 */

/*
 * This is the pcap boilerplate, which lets you interface your Piffle
 * programs with the libpcap library.
 */

#define _POSIX
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <getopt.h>
#include <errno.h>
#include <assert.h>
#include <ctype.h>
#include <pcap.h>

/*
 * This struct simply gathers a pcap capture handle and a pcap dump
 * file handle.
 */
struct pcap_plumbing {
    pcap_t *source;
    pcap_dumper_t *sink;
};

/*
 * The filter function is supposed to be defined in Piffle.
 */
uint32_t filter(uint8_t inp[], uint32_t in_sz, uint8_t out[],
		uint32_t out_sz);

/*
 * Local forward declarations...
 */
static void do_filter(void);
static void filter_loop(struct pcap_plumbing *plumbing);
static void do_pkt(u_char *user, 
                   const struct pcap_pkthdr *hdr, 
                   const u_char *pkt);
static void pp_pkt(struct timeval t, unsigned pkt_sz, const u_char * pkt);
static void die(int err, char *fmt, ...);

/*
 * Command line configuration options (these get set in main()).
 */
static int opt_verbose = 0;	        /* -v */
static char *opt_device = NULL;	        /* -d  */
static char *opt_infile = NULL;	        /* -f */
static char *opt_outfile = NULL;	/* -o */
static char *opt_filter = NULL;	        /* -F */

/*
 * Pcap configuration options
 */
#define PCAP_MAX_PKT_SZ 65536	/* man page says this is enough */
#define PCAP_TIMEOUT    0	/* no timeout */
#define PCAP_PROMISC    0	/* don't force promiscuity */

/*
 * Return codes for the program.
 */
#define STATUS_OK           0
#define STATUS_COMMANDLINE  1
#define STATUS_IO           2
#define STATUS_PCAP         3



/****************************************************************************
 *                                  MAIN
 ****************************************************************************/

/*
 * Main. Read the command line and call do_filter().
 */
int main(int argc, char **argv)
{
    struct option long_options[] = {
	{"verbose", no_argument, 0, 'v'},
	{"output", required_argument, 0, 'o'},
	{"input", required_argument, 0, 'f'},
	{"device", required_argument, 0, 'd'},
	{"filter", required_argument, 0, 'F'}
    };

    while (1) {
	switch (getopt_long(argc, argv, "-vo:f:d:F:", long_options, 0)) {
	case '?':
	case 1:
	    die(STATUS_COMMANDLINE, "error parsing command line");
	    break;
	case -1:
	    goto end_options;
	    break;
	case 'v':
	    opt_verbose = 1;
	    break;
	case 'f':
	    opt_infile = optarg;
	    break;
	case 'o':
	    opt_outfile = optarg;
	    break;
	case 'd':
	    opt_device = optarg;
	    break;
	case 'F':
	    opt_filter = optarg;
	    break;
	default:
	    die(STATUS_COMMANDLINE, "error parsing command line");
	}
    }
  end_options:

    do_filter();

    return STATUS_OK;
}


/****************************************************************************
 *                                 FILTER
 ****************************************************************************/

/*
 * Set up the pcap plumbing and call the main filter loop.
 */
static void do_filter(void)
{
    char errbuf[PCAP_ERRBUF_SIZE];	/* Error buffer for pcap */
    struct pcap_plumbing plumbing = { NULL, NULL };

    if (opt_infile) {
	plumbing.source = pcap_open_offline(opt_infile, errbuf);
    } else {
	/* A NULL opt_device is safe; pcap takes it to mean "any".  */
	plumbing.source = 
            pcap_open_live(opt_device, PCAP_MAX_PKT_SZ, PCAP_PROMISC,
                           PCAP_TIMEOUT, errbuf);
    }

    if (!plumbing.source) {
	die(STATUS_IO, "error opening file '%s' or device '%s'%s",
	    opt_infile, opt_device, errbuf);
    }

    if (opt_outfile) {
	plumbing.sink = pcap_dump_open(plumbing.source, opt_outfile);
	if (!plumbing.sink) {
	    die(STATUS_IO, "error opening output file %s", opt_outfile);
	}
    }

    if (opt_filter) {
	struct bpf_program bpf;
	if (pcap_compile(plumbing.source, &bpf, opt_filter, 0, 0)) {
            char *pcap_error = pcap_geterr(plumbing.source);
            die(STATUS_PCAP, "bpf compile error %s", pcap_error);
	}
	if (pcap_setfilter(plumbing.source, &bpf)) {
            char *pcap_error = pcap_geterr(plumbing.source);
	    die(STATUS_PCAP, "bpf filter error %s", pcap_error);
	}
	pcap_freecode(&bpf);
    }

    filter_loop(&plumbing);

    pcap_dump_flush(plumbing.sink);
    pcap_dump_close(plumbing.sink);
    pcap_close(plumbing.source);
}

/*
 * Run the packet filter repeatedly on the pcap capture descriptor
 * 'source'. Write output to the pcap dump descriptor sink, unless it
 * is NULL, and to stderr, if opt_verbose is set.
 */
static void filter_loop(struct pcap_plumbing *plumbing)
{
    int pcap_loop_s;

    assert(plumbing->source);

    /* Loop over incoming packets calling the callback function. */
    pcap_loop_s = pcap_loop(plumbing->source, -1, do_pkt, (u_char *) plumbing);

    /* When it finishes, we always exit, but depending on the status
     * code we may print a more or less nasty message. */
    if (pcap_loop_s < 0 && pcap_loop_s != -2) {
        die(STATUS_PCAP, "pcap_loop error %i", pcap_loop_s);
    } 
}

/* 
 * This is the callback function that gets called on each
 * packet. Originally, I had this as a nested function within
 * filter_loop(), so that it could refer to the sink object by lexical
 * scoping; but alas, Apple broke the nested function implementation
 * in their version of gcc. Fortunately, pcap provides for a
 * user-specified (u_char *) pointer (called 'user') to be passed to
 * do_pkt, which I use to sneak a struct pcap_plumbing into this
 * function. Bad Apple.
 */
static void do_pkt(u_char *user, 
                   const struct pcap_pkthdr *hdr, 
                   const u_char *pkt) 
{
    struct pcap_plumbing *plumbing = (struct pcap_plumbing *) user;
    struct pcap_pkthdr new_hdr;
    u_char out[PCAP_MAX_PKT_SZ];

    assert(plumbing->source);

    /* We need to make a copy of hdr because it is const. */
    memcpy(&new_hdr, hdr, sizeof(*hdr));

    /* The filter function is defined in the compiled Piffle code. It
     * takes packets as arrays of the Piffle "u8" type, which
     * corresponds to C "uint8_t". I'm pretty sure it's safe to assume
     * that we can cast (u_char *) to (uint8_t *). */
    new_hdr.caplen =
        filter((uint8_t *) pkt, hdr->caplen, (uint8_t *) out,
               PCAP_MAX_PKT_SZ);

    /* If there is a pcap dump file handle for us to dump the
     * processed packet to, do so. XXX I could eliminate the flush
     * call for speed, but in that case, I would need code to make
     * sure a flush happens when the process is killed by a SIGINT. */
    if ( plumbing->sink && new_hdr.caplen ) {
        pcap_dump( (u_char *) plumbing->sink, &new_hdr, out);
        pcap_dump_flush(plumbing->sink);
    }

    /* Dump packet if requested, but don't dump empty packets. */
    if ( opt_verbose && new_hdr.caplen )
        pp_pkt(hdr->ts, new_hdr.caplen, out);
}


/*
 * Dump a packet pkt of size pkt_sz arriving at time t to stderr.
 */
static void pp_pkt(struct timeval t, unsigned pkt_sz, const u_char *pkt)
{
    unsigned i;

    assert(pkt);
    assert(pkt_sz < PCAP_MAX_PKT_SZ);

    fprintf(stderr, "<time=%li:%li len=%u>\n", t.tv_sec, t.tv_usec,
	    pkt_sz);

    for (i = 0; i < pkt_sz; i++) {
	char c = pkt[i];
	fprintf(stderr, "%c", isprint(c) ? c : '.');
	if (!((i - 16 + 1) % 75) || i == 15) {
	    fprintf(stderr, "\n");
	}
    }

    fprintf(stderr, "\n");

    fflush(stderr);
}


/****************************************************************************
 *                               ERROR MESSAGES
 ****************************************************************************/

/*
 * Printf with format @fmt on stderr, and then exit with @errno. NOTE:
 * I have no idea why GNU wants program_invocation_short_name to be
 * declared extern rather than declaring it in some header file or
 * another, but this is apparently how it's done.
 */
static void die(int err, char *fmt, ...)
{
    va_list ap;
    extern char *program_invocation_short_name;

    assert(fmt);

    fprintf(stderr, "%s: ", program_invocation_short_name);
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");
    exit(errno);
}

