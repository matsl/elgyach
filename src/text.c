/* text.c: text utils
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
 * Copyright (C) 2000-2002 Chris Pinkham
 * Copyright (C) 2003 Matthew Kennedy
 */

#include <ctype.h>
#include <string.h>
#include <regex.h>
#include <sys/types.h>

#include "text.h"

#define TAG_REGEX   "</?(font|fade|face|alt|b|u|green|red|organge|purple)[^>]*>"
#define TAG_REGEX_N 2

static regex_t preg;

int
text_init()
{
  int ret;

  ret = regcomp(&preg, TAG_REGEX, REG_EXTENDED | REG_ICASE);
  if (ret != 0)
    goto error_comp;
  
  return 0;

 error_comp:
  return -1;
}

void 
text_done()
{
  regfree(&preg);
}

static char *
blank_tags(char *str)
{
  int ret;
  regoff_t i;
  regmatch_t pmatch[TAG_REGEX_N];

  while ((ret = regexec(&preg, str, TAG_REGEX_N, pmatch, 0)) != REG_NOMATCH)
    {
      for (i = pmatch[0].rm_so; i < pmatch[0].rm_eo; i++)
	str[i] = 0xff;
    }
  return str;
}

char *
strip_tags(char *str)
{
  char *ret, *r, *w;

  ret = blank_tags(str);
  if (ret == NULL)
    goto error_blank;
  for (w = str, r = str; *r; r++)
    {
      if ((unsigned char) (*r) != 0xff) 
	{
	  *w = *r; w++;
	}
    }
  *w = '\0';
  return str;

 error_blank:
  return NULL;
}

char *
strip_ansi(char *str)
{
  int in_ansi = 0;
  char *ptr, *w, *r;

  for (ptr = str; *ptr; ptr++)
    {
      switch (*ptr) 
	{
	case 0x1b:
	  in_ansi ^= 1;
	  *ptr = 0xff;
	  break;
	case 'm':
	  if (in_ansi)
	    {
	      in_ansi ^= 1;
	      *ptr = 0xff;
	    }
	  break;
	default:
	  if (in_ansi)
	    *ptr = 0xff;
	  break;
	}
    }
  for (w = str, r = str; *r; r++)
    {
      if ((unsigned char) (*r) != 0xff) 
	{
	  *w = *r; w++;
	}
    }
  *w = '\0';
  
  return str;
}

/* return a pointer to the first whitespace character contained in str
 * according to isspace(3).
 *
 * @param str the string to search
 *
 * @return a pointer to the first isspace(3) character within str. if
 * str does not contain an isspace(3) character, return a pointer to
 * the '\0' character which terminates str.
 */
char *
find_whitespace(char *str) 
{
  while(*str && (!isspace(*str)))
    str++;

  return str;
}

unsigned long
find_whitespace_offset(const char *str)
{
  unsigned long offset;

  for (offset = 0; *str && ! isspace(*str); offset++);

  return offset;
}

/* return a pointer to the first non-whitespace character in str
 * according to isspace(3).
 *
 * @param str the string to search
 *
 * @return a pointer to the first non-isspace(3) character withing
 * str. if str does not contain a non-isspace(3) character, return a
 * pointer to the '\0' character which terminates str.
 */
char *
skip_whitespace(char *str) 
{
  char *ptr = str;

  while(*ptr && (isspace(*ptr)))
    ptr++;

  return ptr;
}


static int
is_in_charset(char x, const char *set)
{
  while (*set) 
    {
      if (*set == x)
	return 1;
      set++;
    }
  return 0;
}


void
make_tokens(char *buf, int maxwords, int *nwords, char *words[], const char *delimiters)
{
  size_t buf_length = strlen(buf);
  int words_count = 0;
  int state = 0, last_state = 0;
  size_t i;

  *nwords = 0;

  for (i = 0; i < buf_length; i++)
    {
      if (! is_in_charset(buf[i], delimiters))
	state = 1;
      else 
	{
	  state = 0;
	  buf[i] = '\0';
	}
      if (state == 1 && last_state == 0)
	(*nwords)++;
      last_state = state;
    }
  for (i = 0; i < buf_length; i++)
    {
      while (! buf[i] && i < buf_length)
	i++;
      if (i < buf_length && words_count < maxwords)
	{
	  words[words_count] = buf + i;
	  words_count++;
	  while (buf[i] && i < buf_length)
	    i++;
	}
      else 
	break;
    }
}
