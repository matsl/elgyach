
/* $Id$
 *
 * Some code copyright (C) 1998-1999, Mark Spencer <markster@marko.net>
 * libfaim code copyright 1998, 1999 Adam Fritzler <afritz@auk.cx>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
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
#include "sha.h"
#include "yahoo.h"
#include "yahoo_crypt.h"

/* old auth */
static char result6[25];
static char result96[25];

/* new auth */
static char resp_6[100];
static char resp_96[100];

/* This is the y64 alphabet... it's like base64, but has a . and a _ */
static char base64digits[] = 
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._";

/* This is taken from Sylpheed by Hiroyuki Yamamoto.  We have our own tobase64 function
 * in util.c, but it has a bug I don't feel like finding right now ;) */

static void 
to_y64(unsigned char *out, const unsigned char *in, int inlen)
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

static void
curphoo_process_auth_old(char *myusername, char *mypasswd, const char *seed)
{
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

static void 
yahoo_process_auth_new(char *username, char *password, char *seed)
{
  char *name = username;
  char *pass = password;
  md5_byte_t result[16];
  md5_state_t ctx;
  SHA_CTX ctx1;
  SHA_CTX ctx2;
  char *alphabet1 = "FBZDWAGHrJTLMNOPpRSKUVEXYChImkwQ";
  char *alphabet2 = "F0E1D2C3B4A59687abcdefghijklmnop";
  char *challenge_lookup = "qzec2tb3um1olpar8whx4dfgijknsvy5";
  char *operand_lookup = "+|&%/*^-";
  char *delimit_lookup = ",;";
  char *password_hash = calloc(25, 1);
  char *crypt_hash = calloc(25, 1);
  char *crypt_result = NULL;
  char pass_hash_xor1[64];
  char pass_hash_xor2[64];
  char crypt_hash_xor1[64];
  char crypt_hash_xor2[64];
  unsigned char digest1[20];
  unsigned char digest2[20];
  unsigned char magic_key_char[4];
  unsigned char *magic_ptr;

  unsigned int magic[64];
  unsigned int magic_work;
  unsigned int value = 0;
	
  int x;
  int cnt = 0;
  int magic_cnt = 0;
  int magic_len;
  int times = 0;
	
  memset(&pass_hash_xor1, 0, 64);
  memset(&pass_hash_xor2, 0, 64);
  memset(&crypt_hash_xor1, 0, 64);
  memset(&crypt_hash_xor2, 0, 64);
  memset(&digest1, 0, 20);
  memset(&digest2, 0, 20);
  memset(&magic, 0, 64);
  memset(&resp_6, 0, 100);
  memset(&resp_96, 0, 100);
  memset(&magic_key_char, 0, 4);

  /* 
   * Magic: Phase 1.  Generate what seems to be a 30 byte value (could
   * change if base64 ends up differently?  I don't remember and I'm
   * tired, so use a 64 byte buffer.
   */

  magic_ptr = seed;
	
  while (*magic_ptr != (int)NULL) {
    char *loc;
		
    /* Ignore parentheses.  */
		
    if (*magic_ptr == '(' || *magic_ptr == ')') {
      magic_ptr++;
      continue;
    }
		
    /* Characters and digits verify against the challenge lookup. */
		
    if (isalpha(*magic_ptr) || isdigit(*magic_ptr)) {
      loc = strchr(challenge_lookup, *magic_ptr);
      if (!loc) {
	/* This isn't good */
      }
			
      /* Get offset into lookup table and lsh 3. */
			
      magic_work = loc - challenge_lookup;
      magic_work <<= 3;
			
      magic_ptr++;
      continue;
    } else {
      unsigned int local_store;
			
      loc = strchr(operand_lookup, *magic_ptr);
      if (!loc) {
	/* Also not good. */
      }
			
      local_store = loc - operand_lookup;
					
      /* Oops; how did this happen? */
      if (magic_cnt >= 64) 
	break;
					
      magic[magic_cnt++] = magic_work | local_store;
      magic_ptr++;
      continue;
    }
  }
	
  magic_len = magic_cnt;
  magic_cnt = 0;
	
  /* Magic: Phase 2.  Take generated magic value and sprinkle fairy
   * dust on the values. */
	
  for (magic_cnt = magic_len-2; magic_cnt >= 0; magic_cnt--) {
    unsigned char byte1;
    unsigned char byte2;
		
    /* Bad.  Abort.
     */
    if ((magic_cnt + 1 > magic_len) || 
	(magic_cnt > magic_len))
      break;
		
    byte1 = magic[magic_cnt];
    byte2 = magic[magic_cnt+1];

    byte1 *= 0xcd;
    byte1 ^= byte2;
		
    magic[magic_cnt+1] = byte1;
  }
	
  /* Magic: Phase 3.  Final phase; this gives us our key. */
	
  magic_cnt = 1;
	
  for (;;) {
    unsigned int cl = magic[magic_cnt] & 0xff;
    unsigned int bl = magic[magic_cnt+1] & 0xff;
		
    if (!bl || !cl)
      break;
		
    if (magic_cnt > magic_len)
      break;
		
    if (cl <= 0x7f)
      bl = cl;
    else {
      if (cl >= 0x0e0) {
	cl = cl & 0x0f;
	cl = cl << 6;
	bl = bl & 0x3f;
	bl = cl + bl;
	bl = bl << 6;
      } else {
	cl = cl & 0x1f;
	cl = cl << 6;
	bl = cl;
      }
			
      cl = magic[magic_cnt+2];
			
      if (!cl)
	break;
			
      cl = cl & 0x3f;
      bl = bl + cl;
    }
		
    /* Result is bl.
     */
		
    magic_cnt += 3;

    if (times == 0) {
      value |= (bl & 0xff) << 8; 
      value |= (bl & 0xff00) >> 8;           
    } else { 
      value |= (bl & 0xff) << 24; 
      value |= (bl & 0xff00) << 8;           
      break; 
    } 
		
    times++;
  }
	
  /* Dump magic key into a char for SHA1 action. */
  memcpy(&magic_key_char[0], &value, sizeof(int));

  /* Get password and crypt hashes as per usual. */
  md5_init(&ctx);
  md5_append(&ctx, pass,  strlen(pass));
  md5_finish(&ctx, result);
  to_y64(password_hash, result, 16);
	
  md5_init(&ctx);
  crypt_result = yahoo_crypt(pass, "$1$_2S43d5f$");  
  md5_append(&ctx, crypt_result, strlen(crypt_result));
  md5_finish(&ctx, result);
  to_y64(crypt_hash, result, 16);
	
  /* Our first authentication response is based off 
   * of the password hash. */
	
  for (x = 0; x < (int)strlen(password_hash); x++) 
    pass_hash_xor1[cnt++] = password_hash[x] ^ 0x36;
	
  if (cnt < 64) 
    memset(&(pass_hash_xor1[cnt]), 0x36, 64-cnt);
	
  cnt = 0;
	
  for (x = 0; x < (int)strlen(password_hash); x++) 
    pass_hash_xor2[cnt++] = password_hash[x] ^ 0x5c;
	
  if (cnt < 64) 
    memset(&(pass_hash_xor2[cnt]), 0x5c, 64-cnt);
	
  shaInit(&ctx1);
  shaInit(&ctx2);
	
  /* The first context gets the password hash XORed with 0x36 plus a
   * magic value which we previously extrapolated from our
   * challenge. */
	
  shaUpdate(&ctx1, pass_hash_xor1, 64);
  shaUpdate(&ctx1, magic_key_char, 4);
  shaFinal(&ctx1, digest1);
	
  /* The second context gets the password hash XORed with 0x5c plus
   * the SHA-1 digest of the first context. */
	
  shaUpdate(&ctx2, pass_hash_xor2, 64);
  shaUpdate(&ctx2, digest1, 20);
  shaFinal(&ctx2, digest2);
	
  /* Now that we have digest2, use it to fetch characters from an
   * alphabet to construct our first authentication response. */
	
  for (x = 0; x < 20; x += 2) {
    unsigned int    val = 0;
    unsigned int    lookup = 0;
    char            byte[6];
		
    memset(&byte, 0, 6);
		
    /* First two bytes of digest stuffed together. */
		
    val = digest2[x];
    val <<= 8;
    val += digest2[x+1];
		
    lookup = (val >> 0x0b);
    lookup &= 0x1f;
    if (lookup >= strlen(alphabet1))
      break;
    sprintf(byte, "%c", alphabet1[lookup]);
    strcat(resp_6, byte);
    strcat(resp_6, "=");
		
    lookup = (val >> 0x06);
    lookup &= 0x1f;
    if (lookup >= strlen(alphabet2))
      break;
    sprintf(byte, "%c", alphabet2[lookup]);
    strcat(resp_6, byte);
		
    lookup = (val >> 0x01);
    lookup &= 0x1f;
    if (lookup >= strlen(alphabet2))
      break;
    sprintf(byte, "%c", alphabet2[lookup]);
    strcat(resp_6, byte);
		
    lookup = (val & 0x01);
    if (lookup >= strlen(delimit_lookup))
      break;
    sprintf(byte, "%c", delimit_lookup[lookup]);
    strcat(resp_6, byte);
  }
	
  /* Our second authentication response is based off of the crypto
   * hash. */
	
  cnt = 0;
  memset(&digest1, 0, 20);
  memset(&digest2, 0, 20);
	
  for (x = 0; x < (int)strlen(crypt_hash); x++) 
    crypt_hash_xor1[cnt++] = crypt_hash[x] ^ 0x36;
	
  if (cnt < 64) 
    memset(&(crypt_hash_xor1[cnt]), 0x36, 64-cnt);
	
  cnt = 0;
	
  for (x = 0; x < (int)strlen(crypt_hash); x++) 
    crypt_hash_xor2[cnt++] = crypt_hash[x] ^ 0x5c;
	
  if (cnt < 64) 
    memset(&(crypt_hash_xor2[cnt]), 0x5c, 64-cnt);
	
  shaInit(&ctx1);
  shaInit(&ctx2);
	
  /* The first context gets the password hash XORed with 0x36 plus a
   * magic value which we previously extrapolated from our
   * challenge. */
	
  shaUpdate(&ctx1, crypt_hash_xor1, 64);
  shaUpdate(&ctx1, magic_key_char, 4);
  shaFinal(&ctx1, digest1);
	
  /* The second context gets the password hash XORed with 0x5c plus
   * the SHA-1 digest of the first context. */
	
  shaUpdate(&ctx2, crypt_hash_xor2, 64);
  shaUpdate(&ctx2, digest1, 20);
  shaFinal(&ctx2, digest2);
	
  /* Now that we have digest2, use it to fetch characters from an
   * alphabet to construct our first authentication response.  */
	
  for (x = 0; x < 20; x += 2) {
    unsigned int val = 0;
    unsigned int lookup = 0;
		
    char byte[6];
		
    memset(&byte, 0, 6);
		
    /* First two bytes of digest stuffed together. */
		
    val = digest2[x];
    val <<= 8;
    val += digest2[x+1];
		
    lookup = (val >> 0x0b);
    lookup &= 0x1f;
    if (lookup >= strlen(alphabet1))
      break;
    sprintf(byte, "%c", alphabet1[lookup]);
    strcat(resp_96, byte);
    strcat(resp_96, "=");
		
    lookup = (val >> 0x06);
    lookup &= 0x1f;
    if (lookup >= strlen(alphabet2))
      break;
    sprintf(byte, "%c", alphabet2[lookup]);
    strcat(resp_96, byte);
		
    lookup = (val >> 0x01);
    lookup &= 0x1f;
    if (lookup >= strlen(alphabet2))
      break;
    sprintf(byte, "%c", alphabet2[lookup]);
    strcat(resp_96, byte);
		
    lookup = (val & 0x01);
    if (lookup >= strlen(delimit_lookup))
      break;
    sprintf(byte, "%c", delimit_lookup[lookup]);
    strcat(resp_96, byte);
  }
	
  free(password_hash);
  free(crypt_hash);
}

char *
getstr1_old(char *username, char *password, const char *key) {
  curphoo_process_auth_old(username, password, key);
  return result6;
}

char * 
getstr2_old(char *username, char *password, const char *key) {
  curphoo_process_auth_old(username, password, key);
  return result96;
}

char *
getstr1_new(char *username, char *password, char *key) 
{
  yahoo_process_auth_new(username, password, key);
  return resp_6;
}

char *
getstr2_new(char *username, char *password, char *key) 
{
  yahoo_process_auth_new(username, password, key);
  return resp_96;
}

