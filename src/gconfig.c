/* gconfig.c: routines loading and saving configurations
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

#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>

#include "gconfig.h"

#define CONFIG_DIR    ".yahoorc"
#define CONFIG_IGNORE "ignore.db"
#define CONFIG_REGEX  "regex"

char *config_ignore;
char *config_regex;

int
config_init()
{
  char *home;
  size_t home_length, ignore_length, regex_length;

  home = getenv("HOME");
  if (home == NULL)
    {
      fputs("HOME environment variable is not defined\n", stderr);
      return -1;
    }
  home_length = strlen(home);

  ignore_length = home_length + 1 + strlen(CONFIG_DIR) + 1 + strlen(CONFIG_IGNORE);
  config_ignore = malloc(ignore_length + 1);
  if (config_ignore == NULL)
    goto error_ignore;
  regex_length = home_length + 1 + strlen(CONFIG_DIR) + 1 + strlen(CONFIG_REGEX);
  config_regex = malloc(regex_length + 1);
  if (config_regex == NULL)
    goto error_regex;

  snprintf(config_ignore, ignore_length + 1, "%s/%s/%s", home, CONFIG_DIR, CONFIG_IGNORE);
  snprintf(config_regex, regex_length + 1, "%s/%s/%s", home, CONFIG_DIR, CONFIG_REGEX);

  config_ignore[strlen(config_ignore)] = '\0';
  config_regex[strlen(config_regex)] = '\0';

  return 0;

 error_regex:
  free(config_ignore);
 error_ignore:
  config_ignore = NULL;
  config_regex = NULL;
  return -1;
}


void
config_done()
{
  free(config_ignore);
  free(config_regex);
}

