
/**
 * @file sha.h SHA1 Hash functions
 *
 * gaim
 *
 * Copyright (C) 2003 Nathan Walp <faceprint@fa...>
 * Copyright (C) 2003 Matthew Kennedy <mkennedy@gentoo.org>
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
 */

#ifndef SHA_H
#define SHA_H
 
typedef struct {
  unsigned long H[5];
  unsigned long W[80];
  int lenW;
  unsigned long sizeHi, sizeLo;
} j_SHA_CTX;

void shaInit(j_SHA_CTX *ctx);
void shaUpdate(j_SHA_CTX *ctx, unsigned char *dataIn, int len);
void shaFinal(j_SHA_CTX *ctx, unsigned char hashout[20]);

typedef j_SHA_CTX SHA_CTX;

#endif 
