/* -*- mode: c; mode: folding; eval: (c-set-offset 'arglist-cont-nonempty '+) -*-
 *
 * Copyright (C) 2002-2004, Philip S Tellis <philip.tellis AT gmx.net>
 * Copyright (C) 2004, Matthew Kennedy <mkennedy@gentoo.org>
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

#ifndef GLOBALS_H
#define GLOBALS_H

typedef struct {
  char yahoo_id[255];
  char password[255];
  int id;
  int fd;
  int status;
  char *msg;
} yahoo_local_account;

extern yahoo_local_account *ylad;

#define PACKAGE_URL "http://savannah.nongnu.org/projects/elgyach"

#endif
