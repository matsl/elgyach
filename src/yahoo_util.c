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

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if STDC_HEADERS
# include <string.h>
#else
# if !HAVE_STRCHR
#  define strchr index
#  define strrchr rindex
# endif
char *strchr (), *strrchr ();
# if !HAVE_MEMCPY
#  define memcpy(d, s, n) bcopy ((s), (d), (n))
#  define memmove(d, s, n) bcopy ((s), (d), (n))
# endif
#endif

#include <glib.h>

#include "yahoo_util.h"

/* 
 * Convert a string to UTF-8 format.
 */
gchar *
y_str_to_utf8(const gchar *in)
{
  size_t in_len = strlen(in);

  if(in == NULL || in_len == 0)
    return g_strdup("");
	
  gchar *result = g_malloc0(in_len * 2 + 1);
  guint n, i;
  for (i = 0, n = 0; n < in_len; n++) 
    {
      unsigned char c = (unsigned char) in[n];

      if (c < 128)
	result[i++] = (char) c;
      else
	result[i++] = (char) ((c >> 6) | 192);
      result[i++] = (char) ((c & 63) | 128);

    }
  result[i] = '\0';

  return result;
}


/* 
 * Convert a string from UTF-8 format.
 */
gchar * 
y_utf8_to_str(const gchar *in)
{
  size_t in_len = strlen(in);

  if(in == NULL || in_len == 0)
    return g_strdup("");

  gchar *result = g_malloc0(in_len + 1);
  guint i, n;
  for (i = 0, n = 0; n < in_len; n++)
    {
      unsigned char c = in[n];

      if (c < 128) 
	result[i++] = (char) c;
      else 
	result[i++] = (c << 6) | (in[++n] & 63);
    }
  result[i] = '\0';

  return result;
}
