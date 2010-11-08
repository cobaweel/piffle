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
 * This is the test boilerplate, which lets you create a very simple
 * interface for Piffle programs, which is only really useful for
 * testing. Unlike a real boilerplate file would, this one does only
 * minimal error checking.
 */


#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

uint32_t filter(uint8_t inp[], uint32_t in_sz, uint8_t out[],
		uint32_t out_sz);

static void eat_newlines(void);

/****************************************************************************
 *                                  MAIN
 ****************************************************************************/

#define MAX_PKT_SZ 1024

/*
 * Main
 */
int main(void)
{
    uint8_t *in;
    uint32_t in_sz;
    uint8_t out[MAX_PKT_SZ];
    uint32_t out_sz = MAX_PKT_SZ;
    uint32_t actual_out_sz;

    while (1) {
	switch (scanf("%a[^\n]", &in)) {
	case EOF:
	    eat_newlines();
	    return 0;

	case 1:
	    in_sz = strlen((char *) in);
	    assert(in_sz < MAX_PKT_SZ);
	    actual_out_sz = filter(in, in_sz, out, out_sz);
	    assert(actual_out_sz < MAX_PKT_SZ);
	    out[actual_out_sz] = 0;
	    printf("%s", out);
	    fflush(stdout);
	    break;
	}

	if (in)
	    free(in);

	eat_newlines();
	printf("\n");
    }

    return 0;
}

static void eat_newlines(void) 
{
     scanf("%*[\n]");
}

