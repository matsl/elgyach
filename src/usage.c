/* usage.c: help system
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 *
 * Copyright (C) 2003 Matthew Kennedy
 */

#include <stdio.h>
#include <stdlib.h>

#include "usage.h"

void
usage(char *program, FILE *stream) 
{
  fprintf(stream, "USAGE: %s -u login -p password [OPTIONS]\n", program);
  fputs("    where OPTIONS are one or more of:\n", stream);
  fputs("        -u <login>           - yahoo login\n", stream);
  fputs("        -p <password>        - yahoo password.  If no password\n", stream);
  fputs("                               is given, gyach-text will prompt for one\n", stream);
  fputs("        -r <roomname>        - room name to join upon login\n", stream);
  fputs("                               defaults to 'linux, freebsd, solaris:1'\n", stream);
  fputs("        -s <hostname>        - server name to connect to\n", stream);
  fputs("                               defaults to 'scs.yahoo.com'\n", stream);
  fputs("        -f                   - fancy gyach-text, try to keep\n", stream);
  fputs("                               typed text on the bottom line\n", stream);
  fputs("                               of the screen like a real client would.\n", stream);
  fputs("        -c                   - turn on some ANSI colors\n", stream);
  fputs("        -b                   - change black text to default when using colors\n", stream);
}
