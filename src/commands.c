/* -*- mode: c; mode: folding; eval: (c-set-offset 'arglist-cont-nonempty '+) -*-
 *
 * Comman dispatch
 *
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
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <glib.h>

#include "base64.h"
#include "globals.h"
#include "commands.h"
#include "yahoo_util.h"
#include "misc.h"

static command_result_t
command_chat_message(gchar **argv) /* <message-type> <room-name> <message> */
{
  if (strvlen(argv) != 3)
    return COMMAND_ERROR;

  gchar *type = argv[0];
  gchar *room = b64_decode_string(argv[1]);
  gchar *message = b64_decode_string(argv[2]);
  command_result_t result = COMMAND_OK;
  
  if (! g_strcasecmp(type, "plain"))
    yahoo_chat_message(ylad->id, NULL, room, message, 1, 0);
  else if (! g_strcasecmp(type, "emote"))
    yahoo_chat_message(ylad->id, NULL, room, message, 2, 0);
  else 
    result = COMMAND_ERROR;

  FREE(room);
  FREE(message);

  return result;
}


static command_result_t
command_chat_join(gchar **argv) /* <room-name> <room-id> */
{
  /* Linux, FreeBSD, Solaris:1 */
  /* 1600326591 */
  if (strvlen(argv) != 2)
    return COMMAND_ERROR;
  gchar *room = b64_decode_string(argv[0]);
  gchar *roomid = b64_decode_string(argv[1]);

  yahoo_chat_logon(ylad->id, NULL, room, roomid);
  g_free(room);
  g_free(roomid);
  return  COMMAND_OK;
}

static command_result_t 
command_chat_logoff(gchar **argv)
{
  if (strvlen(argv) != 0)
    return COMMAND_ERROR;
  yahoo_chat_logoff(ylad->id, NULL);
  return COMMAND_OK;
}

static command_result_t
command_im_message(gchar **argv) /* <recipient> <message> */
{
  if (strvlen(argv) != 2)
    return COMMAND_ERROR;
  
  gchar *recipient = b64_decode_string(argv[0]);
  gchar *message = b64_decode_string(argv[1]);
  gchar *message_utf8 = y_str_to_utf8(message);

  yahoo_send_im(ylad->id, NULL, recipient, message, 1);
  g_free(message_utf8);
  g_free(recipient);
  g_free(message);

  return COMMAND_OK;
}

static command_result_t
command_im_bell(gchar **argv) /* <recipient> */
{
  if (strvlen(argv) != 1)
    return COMMAND_ERROR;
  
  gchar *recipient = b64_decode_string(argv[0]);
  
  yahoo_send_im(ylad->id, NULL, recipient, "<ding>", 0);
  g_free(recipient);

  return COMMAND_OK;
}

static command_result_t 
command_version(gchar **argv) /* <room-name> */
{
  if (strvlen(argv) != 1)
    return COMMAND_ERROR;

  gchar *room = b64_decode_string(argv[0]);
  yahoo_chat_message(ylad->id, NULL, room, "is using " PACKAGE "-" VERSION " " PACKAGE_URL , 2, 0);
  g_free(room);

  return COMMAND_OK;
}

typedef struct {
  gchar *command;
  command_result_t (* callback)(gchar **);
} command_dispatch_t;

static command_dispatch_t dispatch_table[] =
  {{"CHAT-MESSAGE", command_chat_message},
   {"CHAT-JOIN",    command_chat_join},
   {"CHAT-LOGOFF",  command_chat_logoff},
   {"IM-MESSAGE",   command_im_message},
   {"IM-BELL",      command_im_bell},
   {"VERSION",      command_version},
   {NULL, NULL}};

command_result_t
command_handler(const gchar *command, gchar **argv)
{
  command_result_t result = COMMAND_UNHANDLED;
  command_dispatch_t *dispatcher;

  for (dispatcher = dispatch_table; dispatcher->command != NULL; dispatcher++)
    if (! g_ascii_strcasecmp(dispatcher->command, command))
      {
	result = dispatcher->callback(argv + 1);
	break;
      }

  return result;
}


/* static void */
/* process_commands_old(char *line) */
/* { */
/* 	char *cmd, *to, *msg; */

/* 	char *tmp, *start; */
/* 	char *copy = strdup(line); */

/* 	enum yahoo_status state; */

/* 	start = cmd = copy; */
/* 	tmp = strchr(copy, ' '); */
/* 	if(tmp) { */
/* 		*tmp = '\0'; */
/* 		copy = tmp+1; */
/* 	} else { */
/* 		copy = NULL; */
/* 	} */

/* 	} else if(!strncasecmp(cmd, "CMS", strlen("CMS"))) { */
/* 		/\* send a message *\/ */
/* 		conf_room * cr; */
/* 		to = copy; */
/* 		tmp = strchr(copy, ' '); */
/* 		if(tmp) { */
/* 			*tmp = '\0'; */
/* 			copy = tmp+1; */
/* 		} */
/* 		msg = copy; */
/* 		cr = find_conf_room_by_name_and_id(ylad->id, to); */
/* 		if(!cr) { */
/* 			print_message("no such room: %s", copy); */
/* 			goto end_parse; */
/* 		} */
/* 		if(msg) */
/* 			yahoo_conference_message(ylad->id, NULL, cr->members, to, msg, 0); */
/* 	} else if(!strncasecmp(cmd, "CLS", strlen("CLS"))) { */
/* 		YList * l; */
/* 		if(copy) { */
/* 			conf_room * cr = find_conf_room_by_name_and_id(ylad->id, copy); */
/* 			if(!cr) { */
/* 				print_message("no such room: %s", copy); */
/* 				goto end_parse; */
/* 			} */
/* 			print_message("Room: %s", copy); */
/* 			for(l = cr->members; l; l=l->next) { */
/* 			  print_message("%s", (char *)l->data); */
/* 			} */
/* 		} else { */
/* 			print_message("All Rooms:"); */
/* 			for(l = conferences; l; l=l->next) { */
/* 				conf_room * cr = l->data; */
/* 				print_message("%s", cr->room_name); */
/* 			} */
/* 		} */

/* 	} else if(!strncasecmp(cmd, "CCR", strlen("CCR"))) { */
/* 		conf_room * cr = y_new0(conf_room, 1); */
/* 		while((tmp = strchr(copy, ' ')) != NULL) { */
/* 			*tmp = '\0'; */
/* 			if(!cr->room_name) */
/* 				cr->room_name = strdup(copy); */
/* 			else */
/* 				cr->members = y_list_append(cr->members, */
/* 						strdup(copy)); */
/* 			copy = tmp+1; */
/* 		} */
/* 		cr->members = y_list_append(cr->members, strdup(copy)); */

/* 		if(!cr->room_name || !cr->members) { */
/* 			FREE(cr); */
/* 		} else { */
/* 			cr->id = ylad->id; */
/* 			cr->joined = 1; */
/* 			conferences = y_list_append(conferences, cr); */
/* 			yahoo_conference_invite(ylad->id, NULL, cr->members, cr->room_name, "Join my conference"); */
/* 			cr->members = y_list_append(cr->members,strdup(ylad->yahoo_id)); */
/* 		} */
/* 	} else if(!strncasecmp(cmd, "CIN", strlen("CIN"))) { */
/* 		conf_room * cr; */
/* 		char * room=copy; */
/* 		YList * l1, *l = NULL; */

/* 		while((tmp = strchr(copy, ' ')) != NULL) { */
/* 			*tmp = '\0'; */
/* 			copy = tmp+1; */
/* 			l = y_list_append(l, copy); */
/* 		} */

/* 		cr = find_conf_room_by_name_and_id(ylad->id, room); */
/* 		if(!cr) { */
/* 			print_message("no such room: %s", room); */
/* 			y_list_free(l); */
/* 			goto end_parse; */
/* 		} */

/* 		for(l1 = l; l1; l1=l1->next) { */
/* 			char * w = l1->data; */
/* 			yahoo_conference_addinvite(ylad->id, NULL, w, room, cr->members, "Join my conference"); */
/* 			cr->members = y_list_append(cr->members, strdup(w)); */
/* 		} */
/* 		y_list_free(l); */

/* 	} else if(!strncasecmp(cmd, "CLN", strlen("CLN"))) { */
/* 		conf_room * cr = find_conf_room_by_name_and_id(ylad->id, copy); */
/* 		YList * l; */
/* 		if(!cr) { */
/* 			print_message("no such room: %s", copy); */
/* 			goto end_parse; */
/* 		} */

/* 		cr->joined = 1; */
/* 		for(l = cr->members; l; l=l->next) { */
/* 			char * w = l->data; */
/* 			if(!strcmp(w, ylad->yahoo_id)) */
/* 				break; */
/* 		} */
/* 		if(!l) */
/* 			cr->members = y_list_append(cr->members, strdup(ylad->yahoo_id)); */
/* 		yahoo_conference_logon(ylad->id, NULL, cr->members, copy); */

/* 	} else if(!strncasecmp(cmd, "CLF", strlen("CLF"))) { */
/* 		conf_room * cr = find_conf_room_by_name_and_id(ylad->id, copy); */
		
/* 		if(!cr) { */
/* 			print_message("no such room: %s", copy); */
/* 			goto end_parse; */
/* 		} */

/* 		yahoo_conference_logoff(ylad->id, NULL, cr->members, copy); */

/* 		conferences = y_list_remove(conferences, cr); */
/* 		FREE(cr->room_name); */
/* 		FREE(cr->host); */
/* 		while(cr->members) { */
/* 			YList *n = cr->members->next; */
/* 			FREE(cr->members->data); */
/* 			FREE(cr->members); */
/* 			cr->members=n; */
/* 		} */
/* 		FREE(cr); */

/* 	} else if(!strncasecmp(cmd, "CDC", strlen("CDC"))) { */
/* 		conf_room * cr; */
/* 		char * room = copy; */
/* 		tmp = strchr(copy, ' '); */
/* 		if(tmp) { */
/* 			*tmp = '\0'; */
/* 			copy = tmp+1; */
/* 			msg = copy; */
/* 		} else { */
/* 			msg = "Thanks, but no thanks!"; */
/* 		} */
		
/* 		cr = find_conf_room_by_name_and_id(ylad->id, room); */
/* 		if(!cr) { */
/* 			print_message("no such room: %s", room); */
/* 			goto end_parse; */
/* 		} */

/* 		yahoo_conference_decline(ylad->id, NULL, cr->members, room,msg); */

/* 		conferences = y_list_remove(conferences, cr); */
/* 		FREE(cr->room_name); */
/* 		FREE(cr->host); */
/* 		while(cr->members) { */
/* 			YList *n = cr->members->next; */
/* 			FREE(cr->members->data); */
/* 			FREE(cr->members); */
/* 			cr->members=n; */
/* 		} */
/* 		FREE(cr); */


/* 	} else if(!strncasecmp(cmd, "CHL", strlen("CHL"))) { */
/* 		int roomid; */
/* 		roomid = atoi(copy); */
/* 		yahoo_get_chatrooms(ylad->id, roomid); */


/* 	} else if(!strncasecmp(cmd, "STA", strlen("STA"))) { */
/* 		if(isdigit(copy[0])) { */
/* 			state = (enum yahoo_status)atoi(copy); */
/* 			copy = strchr(copy, ' '); */
/* 			if(state == 99) { */
/* 				if(copy) */
/* 					msg = copy; */
/* 				else */
/* 					msg = "delta x * delta p too large"; */
/* 			} else */
/* 				msg = NULL; */
/* 		} else { */
/* 			state = YAHOO_STATUS_CUSTOM; */
/* 			msg = copy; */
/* 		} */

/* 		yahoo_set_away(ylad->id, state, msg, 1); */





/* 	} else if(!strncasecmp(cmd, "OFF", strlen("OFF"))) { */
/* 		/\* go offline *\/ */
/* 		printf("Going offline\n"); */
/* 		poll_loop=0; */
/* 	} else if(!strncasecmp(cmd, "IDS", strlen("IDS"))) { */
/* 		/\* print identities *\/ */
/* 		const YList * ids = yahoo_get_identities(ylad->id); */
/* 		printf("Identities: "); */
/* 		for(; ids; ids = ids->next) */
/* 			printf("%s, ", (char *)ids->data); */
/* 		printf("\n"); */
/* 	} else if(!strncasecmp(cmd, "AID", strlen("AID"))) { */
/* 		/\* activate identity *\/ */
/* 		yahoo_set_identity_status(ylad->id, copy, 1); */
/* 	} else if(!strncasecmp(cmd, "DID", strlen("DID"))) { */
/* 		/\* deactivate identity *\/ */
/* 		yahoo_set_identity_status(ylad->id, copy, 0); */
/* 	} else if(!strncasecmp(cmd, "LST", strlen("LST"))) { */
/* 		YList * b = buddies; */
/* 		for(; b; b=b->next) { */
/* 			yahoo_account * ya = b->data; */
/* 			if(ya->status == YAHOO_STATUS_OFFLINE) */
/* 				continue; */
/* 			if(ya->msg) */
/* 			  print_message("%s (%s) is now %s", ya->name, ya->yahoo_id, ya->msg); */
/* 			else */
/* 			  print_message("%s (%s) is now %s", ya->name, ya->yahoo_id, yahoo_status_code(ya->status)); */
/* 		} */
/* 	} else if(!strncasecmp(cmd, "NAM", strlen("NAM"))) { */
/* 		struct yab * yab; */
		
/* 		to = copy; */
/* 		tmp = strchr(copy, ' '); */
/* 		if(tmp) { */
/* 			*tmp = '\0'; */
/* 			copy = tmp+1; */
/* 		} */
/* 		msg = copy; */

/* 		if(to && msg) { */
/* 			yab = y_new0(struct yab, 1); */
/* 			yab->id = to; */
/* 			yab->fname = msg; */
/* 			yahoo_set_yab(ylad->id, yab); */
/* 			FREE(yab); */
/* 		} */
/* 	} else if(!strncasecmp(cmd, "WCAM", strlen("WCAM"))) { */
/* 		if (copy) */
/* 		{ */
/* 			printf("Viewing webcam (%s)\n", copy); */
/* 			webcam_direction = YAHOO_WEBCAM_DOWNLOAD; */
/* 			yahoo_webcam_get_feed(ylad->id, copy); */
/* 		} else { */
/* 			printf("Starting webcam\n"); */
/* 			webcam_direction = YAHOO_WEBCAM_UPLOAD; */
/* 			yahoo_webcam_get_feed(ylad->id, NULL); */
/* 		} */
/* 	} else if(!strncasecmp(cmd, "WINV", strlen("WINV"))) { */
/* 		printf("Inviting %s to view webcam\n", copy); */
/* 		yahoo_webcam_invite(ylad->id, copy); */
/* 	} else { */
/* 		fprintf(stderr, "Unknown command: %s\n", cmd); */
/* 	} */

/* end_parse: */
/* 	FREE(start); */
/* } */

