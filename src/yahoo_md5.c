/*
 * gaim
 *
 * Some code copyright (C) 1998-1999, Mark Spencer <markster@marko.net>
 * libfaim code copyright 1998, 1999 Adam Fritzler <afritz@auk.cx>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <ctype.h>
#include "md5.h"

#ifdef DMALLOC
#include <dmalloc.h>
#endif

extern char *yahoo_crypt(char *, char *);

char result6[25];
char result96[25];

/* This is the y64 alphabet... it's like base64, but has a . and a _ */
char base64digits[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._";

/* This is taken from Sylpheed by Hiroyuki Yamamoto.  We have our own tobase64 function
 * in util.c, but it has a bug I don't feel like finding right now ;) */
void to_y64(unsigned char *out, const unsigned char *in, int inlen)
     /* raw bytes in quasi-big-endian order to base 64 string (NUL-terminated) */
{
	for (; inlen >= 3; inlen -= 3)
		{
			*out++ = base64digits[in[0] >> 2];
			*out++ = base64digits[((in[0] << 4) & 0x30) | (in[1] >> 4)];
			*out++ = base64digits[((in[1] << 2) & 0x3c) | (in[2] >> 6)];
			*out++ = base64digits[in[2] & 0x3f];
			in += 3;
		}
	if (inlen > 0)
		{
			unsigned char fragment;

			*out++ = base64digits[in[0] >> 2];
			fragment = (in[0] << 4) & 0x30;
			if (inlen > 1)
				fragment |= in[1] >> 4;
			*out++ = base64digits[fragment];
			*out++ = (inlen < 2) ? '-' : base64digits[(in[1] << 2) & 0x3c];
			*out++ = '-';
		}
	*out = '\0';
}

void curphoo_process_auth(char *myusername, char *mypasswd, char *myseed)
/* 
WAS: static void yahoo_process_auth(struct gaim_connection *gc, struct yahoo_packet *pkt)
*/
{
	char *seed = myseed;
	char *sn   = myusername;
	
	
	
		
	md5_byte_t result[16];
	md5_state_t ctx;
	char *crypt_result;
	char *password_hash = (char *)malloc(25);
	char *crypt_hash = (char *)malloc(25);
	char *hash_string_p = (char *)malloc(50 + strlen(sn));
	char *hash_string_c = (char *)malloc(50 + strlen(sn));
	
	char checksum;
	
	int sv;
	
	memset((char *)result6, 0, 25);
	memset((char *)result96, 0, 25);

	sv = seed[15];
	sv = sv % 8;

	md5_init(&ctx);
	md5_append(&ctx, mypasswd, strlen(mypasswd));
	md5_finish(&ctx, result);
	to_y64(password_hash, result, 16);
	
	md5_init(&ctx);
	crypt_result = yahoo_crypt(mypasswd, "$1$_2S43d5f$");  
	md5_append(&ctx, crypt_result, strlen(crypt_result));
	md5_finish(&ctx, result);
	to_y64(crypt_hash, result, 16);

	switch (sv) {
	case 1:
	case 6:
		checksum = seed[seed[9] % 16];
		snprintf(hash_string_p, strlen(sn) + 50,
			   "%c%s%s%s", checksum, myusername, seed, password_hash);
		snprintf(hash_string_c, strlen(sn) + 50,
			   "%c%s%s%s", checksum, myusername, seed, crypt_hash);
		break;
	case 2:
	case 7:
		checksum = seed[seed[15] % 16];
		snprintf(hash_string_p, strlen(sn) + 50,
			   "%c%s%s%s", checksum, seed, password_hash, myusername);
		snprintf(hash_string_c, strlen(sn) + 50,
			   "%c%s%s%s", checksum, seed, crypt_hash, myusername);
		break;
	case 3:
		checksum = seed[seed[1] % 16];
		snprintf(hash_string_p, strlen(sn) + 50,
			   "%c%s%s%s", checksum, myusername, password_hash, seed);
		snprintf(hash_string_c, strlen(sn) + 50,
			   "%c%s%s%s", checksum, myusername, crypt_hash, seed);
		break;
	case 4:
		checksum = seed[seed[3] % 16];
		snprintf(hash_string_p, strlen(sn) + 50,
			   "%c%s%s%s", checksum, password_hash, seed, myusername);
		snprintf(hash_string_c, strlen(sn) + 50,
			   "%c%s%s%s", checksum, crypt_hash, seed, myusername);
		break;
	case 0:
	case 5:
		checksum = seed[seed[7] % 16];
		snprintf(hash_string_p, strlen(sn) + 50,
			   "%c%s%s%s", checksum, password_hash, myusername, seed);
		snprintf(hash_string_c, strlen(sn) + 50,
			   "%c%s%s%s", checksum, crypt_hash, myusername, seed);
		break;
	}
	
	md5_init(&ctx);  
	md5_append(&ctx, hash_string_p, strlen(hash_string_p));
	md5_finish(&ctx, result);
	to_y64(result6, result, 16);


	md5_init(&ctx);  
	md5_append(&ctx, hash_string_c, strlen(hash_string_c));
	md5_finish(&ctx, result);
	to_y64(result96, result, 16);


	
	free(password_hash);
	free(crypt_hash);
	free(hash_string_p);
	free(hash_string_c);

}

char *getstr1(char *username, char *password, char *key) {
	curphoo_process_auth(username, password, key);
	return result6;
}

char *getstr2(char *username, char *password, char *key) {
	curphoo_process_auth(username, password, key);
	return result96;
}

