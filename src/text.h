/* text.h: text utils
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

#ifndef TEXT_H
#define TEXT_H

extern int text_init();
extern void text_done();

extern char *find_whitespace(char *str);
extern char *skip_whitespace(char *str);
extern char *strip_tags(char *str);
extern char *strip_ansi(char *str);

#endif
