/* input.c: command input handler
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
 * Copyright (C) 2003 Matthew Kennedy <mkennedy@gentoo.org>
 */

#include <stdlib.h>
#include <stdio.h>
#include <regex.h>
#include <string.h>
#include <unistd.h>

#include "text.h"
#include "yahoochat.h"
#include "input.h"

typedef struct {
  char *command;
  int (*callee)(YMSG9_SESSION *session, const char *);
} command_pair_t;

static int input_think(YMSG9_SESSION *, const char *);
static int input_say(YMSG9_SESSION *, const char *);
static int input_join(YMSG9_SESSION *, const char *);
static int input_goto(YMSG9_SESSION *, const char *);
static int input_tell(YMSG9_SESSION *, const char *);
static int input_boot(YMSG9_SESSION *, const char *);
static int input_emote(YMSG9_SESSION *, const char *);
static int input_help(YMSG9_SESSION *, const char *);
static int input_quit(YMSG9_SESSION *, const char *);

static command_pair_t command_table[] = {
  {"think", input_think},
  {"say",   input_say},
  {"join",  input_join},
  {"goto",  input_goto},
  {"tell",  input_tell},
  {"emote", input_emote},
  {"boot",  input_boot},
  {"help",  input_help},
  {"quit",  input_quit},
  {NULL, NULL}};

#define MAX_MESSAGE_LENGTH MAX_DATA_LENGTH

static char message_buffer[MAX_MESSAGE_LENGTH + 1];


static int
input_think(YMSG9_SESSION *session, const char *input)
{
  snprintf(message_buffer, MAX_MESSAGE_LENGTH, "* %s . o O ( %s )", session->user, input);
  message_buffer[MAX_MESSAGE_LENGTH] = '\0';
  return ymsg9_think(session, message_buffer);
}

static int
input_say(YMSG9_SESSION *session, const char *input)
{
  snprintf(message_buffer, MAX_MESSAGE_LENGTH, "%s: %s", session->user, input);
  message_buffer[MAX_MESSAGE_LENGTH] = '\0';
  return ymsg9_comment(session, message_buffer);
}

static int
input_join(YMSG9_SESSION *session, const char *input)
{  
  strncpy(session->room, input, YMSG9_ROOM_SIZE);
  return ymsg9_join(session);
}

static int
input_goto(YMSG9_SESSION *session, const char *input)
{
  return ymsg9_goto(session, input);
}

static int
input_tell(YMSG9_SESSION *session, const char *input)
{
  /* determine the username and the text to send */
  char username[YMSG9_USER_SIZE + 1];
  unsigned long offset = find_whitespace_offset(input);
  strncpy(username, input, (offset > YMSG9_USER_SIZE) ? YMSG9_USER_SIZE : offset);
  
  return ymsg9_pm(session, username, input + offset);
}

static int
input_emote(YMSG9_SESSION *session, const char *input)
{
  snprintf(message_buffer, MAX_MESSAGE_LENGTH, "* %s %s", session->user, input);
  message_buffer[MAX_MESSAGE_LENGTH] = '\0';
  return ymsg9_emote(session, message_buffer);
}

static int
input_boot(YMSG9_SESSION *session, const char *input)
{
  /* you're such a n00b. */
  return 0;
}

static int
input_help(YMSG9_SESSION *session, const char *input)
{
  command_pair_t *ptr;

  for (ptr = command_table; ptr; ptr++)
    fprintf(stdout, "%s ", ptr->command);

  return 0;
}

static int
input_quit(YMSG9_SESSION *session, const char *input)
{
  close(session->sock);
  session->quit = 1;

  return 0;
}


#define MAX_NMATCH 10
#define COMMAND_REGEX "^ */([a-z]+)( +(.*))? *$"

static regex_t command_pattern;

int
input_init()
{
  return regcomp(&command_pattern, COMMAND_REGEX, REG_EXTENDED | REG_ICASE);
}

void 
input_done()
{
  regfree(&command_pattern);
}

int 
input_handler(YMSG9_SESSION *session, char *input) 
{
  regmatch_t matches[MAX_NMATCH];
  int ret;
  char *command, *arguments;
  command_pair_t *ptr;

  ret = regexec(&command_pattern, input, MAX_NMATCH, matches, 0);
  if (ret == 0)
    {
      input[matches[1].rm_eo] = '\0'; 
      command = input + matches[1].rm_so;
      if (matches[3].rm_so == -1)
        arguments = NULL;
      else 
        {
          input[matches[3].rm_eo] = '\0'; 
          arguments = input + matches[3].rm_so;
          if (strlen(arguments) == 0)
            arguments = NULL;
        }
      for (ptr = command_table; ptr != NULL; ptr++) 
        {
          if (strcasecmp(ptr->command, command) == 0)
            (ptr->callee)(session, arguments);
        }
      if (ptr == NULL)
        return 0;
    }

  return 1;
}
