/* display.c: handle incoming packets
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

#include <assert.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "yahoochat.h"
#include "display.h"

#define STREQ(a, b) (strcmp(a, b) == 0)

static int display_comment(YMSG9_SESSION *);
static int display_pm(YMSG9_SESSION *);
static int display_exit(YMSG9_SESSION *);
static int display_get_key(YMSG9_SESSION *);
static int display_join(YMSG9_SESSION *);
static int display_quit(YMSG9_SESSION *);
static int display_cookie(YMSG9_SESSION *);
static int display_online(YMSG9_SESSION *);
static int display_notify(YMSG9_SESSION *);

typedef struct {
  int packet_type;
  int (*callee)(YMSG9_SESSION *session);
} display_pair_t;

static display_pair_t display_table[] = {
  {YMSG9_COMMENT, display_comment},
  {YMSG9_PM,      display_pm},
  {YMSG9_PM_RECV, display_pm},
  {YMSG9_EXIT,    display_exit},
  {YMSG9_GET_KEY, display_get_key},
  {YMSG9_JOIN,    display_join},
  {YMSG9_LOGOUT,  display_quit},
  {YMSG9_COOKIE,  display_cookie},
  {YMSG9_ONLINE,  display_online},
  {YMSG9_NOTIFY,  display_notify},
  {0, NULL}};

static void
zero_separators(char data[], int data_len)
{
  int i;

  for (i = 0; i < data_len - 1; i++)
    if (data[i] == YMSG9_SEP[0] && data[i + 1] == YMSG9_SEP[1])
      memset(data + i, 0, 2);
}

typedef struct {
  char *key, *value;
} field_pair_t;

#define MAX_FIELDS 100

static field_pair_t packet_fields[MAX_FIELDS];

static int
parse_packet(char data[], int data_len)
{
  int field;
  int i;

  zero_separators(data, data_len);
  memset(packet_fields, 0, sizeof(packet_fields));
  for (i = 0, field = 0; data[i] != '\0' && field < MAX_FIELDS; field++)
    {
      packet_fields[field].key = data + i;
      i += strlen(data + i) + 2;
      packet_fields[field].value = data + i;
      i += strlen(data + i) + 2;
    }

  return field;
}

static char *
get_value_for_key(const char *key)
{
  int i;

  for (i = 0; i < MAX_FIELDS; i++)
    {
      if (packet_fields[i].key == NULL)
	break;
      if (strcmp(packet_fields[i].key, key) == 0)
	return packet_fields[i].value;
    }

  return NULL;
}


#define MAX_OUTPUT_LENGTH MAX_DATA_LENGTH

static char output_buffer[MAX_OUTPUT_LENGTH + 1];

static int
print_output(void)
{
  fputs(output_buffer, stdout);
  fputc(0, stdout);
  fflush(stdout);

  return 1;
}

static int 
display_comment(YMSG9_SESSION *session)
{
  char *user = get_value_for_key("109");
  char *comment = get_value_for_key("117");
  char *type = get_value_for_key("124");
  int count;

  if (strcmp(type, "1") == 0)
    count = snprintf(output_buffer, MAX_OUTPUT_LENGTH, "SAYS %s|%s", user, comment);
  else if (strcmp(type, "2") == 0)
    count = snprintf(output_buffer, MAX_OUTPUT_LENGTH, "DOES %s|%s", user, comment);
  else 
    {
      fprintf(stderr, "Warning: Unknown field 124 value %s in comment packet\n", type);
      return 0;
    }
  output_buffer[count] = '\0';
  print_output();
  return 1;
}

static int
display_pm(YMSG9_SESSION *session)
{
  char *user = get_value_for_key("4");
  char *comment = get_value_for_key("14");
  int count;

  count = snprintf(output_buffer, MAX_OUTPUT_LENGTH, "PM %s|%s", user, comment);
  output_buffer[count] = '\0';
  print_output();

  return 1;
}

/* /\* -------------------------------------------------------------------------------- *\/ */

/* case YMSG9_JOIN: */
/* /\* fixme, need to verify what exactly constitutes a join pkt *\/ */
/* strcpy( tmp, ymsg9_field( "126" ));     /\* dunno what *\/ */
/* strcpy( tmp2, ymsg9_field( "104" ));    /\* room name *\/ */
/* if (( strcmp( tmp, "" )) || */
/*     ( ! strcmp( tmp2, "" ))) { */
/*   /\* I joined the room *\/ */
/*   if ( strcmp( tmp2, "" )) { */
/*     strcpy( ymsg_sess->room, tmp2 ); */

/*     sprintf( buf, "You are now in %s%s%s\n", */
/* 	     ANSI_COLOR_BLUE, tmp2, ANSI_ATTR_RESET ); */
/*     if ( strcmp( ymsg9_field( "105" ), "" )) { */
/*       strcat( buf, ANSI_COLOR_CYAN ); */
/*       strcat( buf, ymsg9_field( "105" )); */
/*       strcat( buf, ANSI_ATTR_RESET ); */
/*       strcat( buf, "\n" ); */
/*     } */
/*   } */

/*   strcpy( tmp, ymsg9_field( "109" )); */

/*   snprintf(buf, BUF_LEN, "init room %s\n", tmp); */
/*   append_to_textbox(buf); */

/*   /\* 					make_tokens(tmp, max_words, &nwords, words, ","); *\/ */
/*   /\* 					for (i = 0; i < nwords; i++) *\/ */
/*   /\* 					  { *\/ */
/*   /\* 					    sprintf(buf, "%s enters the room\n", words[i]); *\/ */
/*   /\* 					    append_to_textbox( buf ); *\/ */
/*   /\* 					  } *\/ */

/* } else if (( ! strcmp( ymsg9_field( "108" ), "1" )) && */
/* 	   ( strcasecmp( ymsg9_field( "109" ), ymsg_sess->user ))) { */
/*   /\* someone else joined the room while I'm in there *\/ */
/*   strcpy(tmp, ymsg9_field( "109" )); */
/*   sprintf(buf, "%s enters the room\n", tmp); */
/*   append_to_textbox(buf); */
/* } */
/* break; */

/* /\* -------------------------------------------------------------------------------- *\/ */

static int
display_join(YMSG9_SESSION *session)
{
  int count1, count2;
  char *mystery = get_value_for_key("126"), *room = get_value_for_key("104");

  if (! STREQ(mystery, "") || STREQ(room, ""))
    {
      /* we joined the room */
      count1 = 0;
      if (! STREQ(room, ""))
	{
	  strncpy(session->room, room, YMSG9_ROOM_SIZE);
	  session->room[YMSG9_ROOM_SIZE] = '\0';
	  count1 = snprintf(output_buffer, MAX_OUTPUT_LENGTH, "ROOM %s|", room);
	  output_buffer[count1] = '\0';

	  if (! STREQ(get_value_for_key("105"), ""))
	    {
	      count2 = snprintf(output_buffer + count1, MAX_OUTPUT_LENGTH - count1, "ROOMDATA %s|", get_value_for_key("105"));
	      output_buffer[count1 + count2] = '\0';
	    }
	}
      count1 = snprintf(output_buffer, MAX_OUTPUT_LENGTH, "ROOMINIT %s|", get_value_for_key("109"));
      output_buffer[count1] = '\0';
      print_output();
      return 1;
    }
  else if (STREQ(get_value_for_key("108"), "1"))
    {
      count1 = snprintf(output_buffer, MAX_OUTPUT_LENGTH, "ENTER %s|", get_value_for_key("109"));
      output_buffer[count1] = '\0';
      print_output();
      return 1;
    }

  return 0;
}

static int
display_exit(YMSG9_SESSION *session)
{
  char *user = get_value_for_key("109");
  int count;

  count = snprintf(output_buffer, MAX_OUTPUT_LENGTH, "EXIT %s|", user);
  output_buffer[count] = '\0';
  print_output();
  return 1;
}

static int
display_get_key(YMSG9_SESSION *session)
{
  char *user = get_value_for_key("94");
  int count;

  ymsg9_login(session, user);
  session->cookie[0] = '\0'; /* reset cookies */
  count = snprintf(output_buffer, MAX_OUTPUT_LENGTH, "LOGIN %s %d %s|", session->host, session->port, user);
  output_buffer[count] = '\0';
  print_output();

  return 1;
}

static int
display_quit(YMSG9_SESSION *session) 
{
  int count;

  close(session->sock);
  session->sock = -1;
  session->quit = 1;
  count = snprintf(output_buffer, MAX_OUTPUT_LENGTH, "QUIT|");
  output_buffer[count] = '\0';
  print_output();

  return 1;
}

static int
display_online(YMSG9_SESSION *session)
{
  ymsg9_join(session);
  return 1;
}

static int
display_notify(YMSG9_SESSION *session)
{
  return 1;
}

static int
display_cookie(YMSG9_SESSION *session)
{
  int count;
  char *ptr;

  if (! STREQ(get_value_for_key("3"), ""))
    {
      count = snprintf(output_buffer, MAX_OUTPUT_LENGTH, "ONLINE|");
      output_buffer[count] = '\0';
      print_output();
      ymsg9_online(session);
    }
  if (! STREQ(get_value_for_key("59"), ""))
    {
      if (session->cookie[0]) 
	{
	  /* append to what we already have */
	  count = strlen(session->cookie);
	  ptr = session->cookie + count;
	  count = snprintf(ptr, YMSG9_COOKIE_SIZE - count, ";%s", get_value_for_key("59"));
	  ptr[count] = '\0';
	}
      else 
	{
	  strncpy(session->cookie, get_value_for_key("59"), YMSG9_COOKIE_SIZE);
	  session->cookie[YMSG9_COOKIE_SIZE] = '\0';
	}
      for (ptr = strchr(session->cookie, '\t'); ptr != NULL; )
	{
	  *ptr = '=';
	  ptr = strchr(ptr, '|');
	  if (ptr)
	    {
	      *ptr = ';';
	      ptr = strchr(ptr, '\t');
	    }
	}
    }
  
  return 1;
}

int 
display_packet(YMSG9_SESSION *session) 
{
  display_pair_t *ptr;
  int ret;

  for (ptr = display_table; ptr != NULL; ptr++)
    if (ptr->packet_type == session->pkt.type)
      {
	parse_packet(session->pkt.data, session->pkt.size);
	ret = (ptr->callee)(session);
	break;
      }
  if (ptr == NULL)
    return 0;

  return ret;
}  
