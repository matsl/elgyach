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
#include <stdlib.h>
#include <stdio.h>

#include "yahoochat.h"
#include "display.h"

typedef struct {
  int packet_type;
  int (*callee)(YMSG9_SESSION *session, const char *);
} display_pair_t;

static int display_comment(YMSG9_SESSION *, const char *);

static display_pair_t display_table[] = {
  {YMSG9_COMMENT, display_comment},
  {0, NULL}};

static int
display_comment(YMSG9_SESSION *session, char *fields[])
{
  
}

/*   case YMSG9_COMMENT:  */
/*     /\* user comment/emote/thought *\/ */
/*     strcpy( tmp, ymsg9_field( "109" )); */
/*     strcpy( tmp2, ymsg9_field( "117" )); */

/*     if ( ! strcmp( ymsg9_field( "124"), "1" )) { */
/*       sprintf( buf, "%s%s%s%s: %s%s%s\n", */
/* 	       ANSI_BOLDON, tmp, ANSI_ATTR_RESET, ANSI_BOLDOFF, */
/* 	       ANSI_ATTR_RESET, tmp2, ANSI_ATTR_RESET ); */
/*     } else { */
/*       sprintf( buf, "* %s%s%s%s %s%s\n", */
/* 	       ANSI_BOLDON, tmp, ANSI_ATTR_RESET, ANSI_BOLDOFF, */
/* 	       tmp2, ANSI_ATTR_RESET ); */
/*     } */
/*     append_to_textbox( buf ); */

/*     break; */



int 
display_packet(YMSG9_SESSION *session) {
  
  
  





char buf[BUF_LEN];  /* these are static so they aren't on the stack */
  char tmp[2048];
  char tmp2[2048];
  char tmp3[2048];
  char *ptr;
  time_t time_now;

  if ( ymsg_sess->pkt.size ) {
    split( ymsg_sess->pkt.data, "\xC0\x80" );
  } else {
    strcpy( ymsg_fields[0], "__END__" );
    strcpy( ymsg_fields[1], "__END__" );
  }

  strcpy( buf, "" );
  strcpy( tmp, "" );
  strcpy( tmp2, "" );
  strcpy( tmp3, "" );

  switch( ymsg_sess->pkt.type ) {
  case YMSG9_GET_KEY:
    strcpy( tmp, ymsg9_field( "94" ));
    ymsg9_login( ymsg_sess, tmp );

    sprintf( buf, "*** Logging in to chat server...\n" );
    append_to_textbox( buf );

    /* reset cookies */
    ymsg_sess->cookie[0] = '\0';
    break;

  case YMSG9_COOKIE:
    /* set online if we got the right cookie packet */
    if ( strcmp( ymsg9_field( "3" ), "" )) {
      sprintf( buf, "*** Setting status to Online...\n" );
      append_to_textbox( buf );

      ymsg9_online( ymsg_sess );
    }

    /* store the cookies */
    if ( strcmp( ymsg9_field( "59" ), "" )) {
      if ( ymsg_sess->cookie[0] ) {
	strcat( ymsg_sess->cookie, ";" );
	strcat( ymsg_sess->cookie, ymsg9_field_p( "59" ));
      } else {
	strcpy( ymsg_sess->cookie, ymsg9_field_p( "59" ));
      }
      ptr = ymsg_sess->cookie;
      ptr = strchr( ptr, '	' ); /* a tab */
      while( ptr ) {
	*ptr = '=';
	ptr = strchr( ptr, '|' );

	if ( ptr ) {
	  *ptr = ';';
	  ptr = strchr( ptr, '	' ); /* a tab */
	}
      }
    }
    break;

  case YMSG9_ONLINE:
    ymsg9_join( ymsg_sess );
    break;

  case YMSG9_JOIN:
    /* fixme, need to verify what exactly constitutes a join pkt */
    strcpy( tmp, ymsg9_field( "126" ));     /* dunno what */
    strcpy( tmp2, ymsg9_field( "104" ));    /* room name */
    if (( strcmp( tmp, "" )) ||
	( ! strcmp( tmp2, "" ))) {
      /* I joined the room */
      if ( strcmp( tmp2, "" )) {
	strcpy( ymsg_sess->room, tmp2 );

	sprintf( buf, "You are now in %s%s%s\n",
		 ANSI_COLOR_BLUE, tmp2, ANSI_ATTR_RESET );
	if ( strcmp( ymsg9_field( "105" ), "" )) {
	  strcat( buf, ANSI_COLOR_CYAN );
	  strcat( buf, ymsg9_field( "105" ));
	  strcat( buf, ANSI_ATTR_RESET );
	  strcat( buf, "\n" );
	}
      }

      strcpy( tmp, ymsg9_field( "109" ));

      snprintf(buf, BUF_LEN, "init room %s\n", tmp);
      append_to_textbox(buf);

      /* 					make_tokens(tmp, max_words, &nwords, words, ","); */
      /* 					for (i = 0; i < nwords; i++) */
      /* 					  { */
      /* 					    sprintf(buf, "%s enters the room\n", words[i]); */
      /* 					    append_to_textbox( buf ); */
      /* 					  } */

    } else if (( ! strcmp( ymsg9_field( "108" ), "1" )) &&
	       ( strcasecmp( ymsg9_field( "109" ), ymsg_sess->user ))) {
      /* someone else joined the room while I'm in there */
      strcpy(tmp, ymsg9_field( "109" ));
      sprintf(buf, "%s enters the room\n", tmp);
      append_to_textbox(buf);
    }
    break;

  case YMSG9_EXIT:
    if ( strcasecmp( ymsg_sess->user, ymsg9_field( "109"))) {
      /* someone else left */
      strcpy( tmp, ymsg9_field( "109" ));
      sprintf(buf, "%s leaves the room\n", tmp);
      append_to_textbox( buf );
    } else {
      /* packet that shows up when I leave */
    }
    break;

  case YMSG9_PM:
  case YMSG9_PM_RECV:
    strcpy( tmp, ymsg9_field( "4" ));
    strcpy( tmp2, ymsg9_field( "14" ));

    /* don't think we should get any of these, but just in cas */
    if ( ! strcmp( ymsg9_field( "49" ), "TYPING" )) {
      return;
    }

    sprintf(buf, "%s <private to %s> %s\n", tmp, ymsg_sess->user, tmp2);
    append_to_textbox(buf);
    break;

  case YMSG9_NOTIFY:
    strcpy( tmp, ymsg9_field( "4" ));
    break;

  case YMSG9_INVITE: 
    strcpy( tmp, ymsg9_field( "119" ));
    strcpy( tmp2, ymsg9_field( "104" ));

    if ( ! strcmp( tmp, "" )) {
      sprintf( buf, "%sUser has been invited to %s%s\n",
	       ANSI_COLOR_PURPLE, ymsg_sess->room, ANSI_ATTR_RESET );
      append_to_textbox( buf );
      return;
    }
			
    sprintf( buf, "You have been invited to (%s) by %s\n", tmp2, tmp );
    append_to_textbox( buf );
    break;

  case YMSG9_LOGOUT:
    close( ymsg_sess->sock );
    ymsg_sess->sock = -1;

    time_now = time(NULL);

    /* show user that we aren't logged in anymore */
    sprintf( buf, "%sDisconnected from Chat!%s\n",
	     ANSI_COLOR_RED, ANSI_ATTR_RESET );
    append_to_textbox( buf );

    break;
  }

}



/* function related to data arriving from the network */

#define YMSG_FIELDS 300
#define YMSG_FIELD_LENGTH 1024

#define YMSG_FIELD_END "__END__"

static char ymsg_fields[YMSG_FIELDS][YMSG_FIELD_LENGTH];

int 
split(char *str, char *connector)
{
  char *last, *orig, *ptr;
  int result;
  
  size_t connector_len;
  size_t tmp_str_len;
  char *tmp_str;
  
  tmp_str_len = strlen(str) + 1;
  tmp_str = malloc(tmp_str_len);
  if (tmp_str == NULL)
    goto error_malloc;
  strcpy(tmp_str, str); /* guarded */

  str = tmp_str;
  last = orig = str;
  
  for (result = 0, connector_len = strlen(connector);
       (ptr = strstr(str, connector)) && result < YMSG_FIELDS; 
       result++) 
    {
      *ptr = '\0';
      strncpy(ymsg_fields[result], last, YMSG_FIELD_LENGTH);
      ymsg_fields[result][YMSG_FIELD_LENGTH - 1] = '\0';
      str = last = ptr + connector_len;
    }

  if (last != orig) 
    {
      strncpy(ymsg_fields[result], last, YMSG_FIELD_LENGTH);
      ymsg_fields[result][YMSG_FIELD_LENGTH - 1] = '\0';
    } 
  else
    result--;

  
  if (result > 0 && result + 1 < YMSG_FIELDS)
    {
      /*       fprintf(stderr, ">>>%d<<<>>%s<<\n", result, tmp_str); */
      assert(result > 0 && result + 1 < YMSG_FIELDS);
    }
  strncpy(ymsg_fields[result], "__END__", YMSG_FIELD_LENGTH);
  strncpy(ymsg_fields[result + 1], "__END__", YMSG_FIELD_LENGTH);
  ymsg_fields[result][YMSG_FIELD_LENGTH - 1] = '\0';
  ymsg_fields[result + 1][YMSG_FIELD_LENGTH - 1] = '\0';

  free(tmp_str);
  return result + 1;

 error_malloc:
  return -2;
}

#define YMSG9_RESULT_LENGTH 2048

char *
ymsg9_field_p(char *key) 
{
  int i = 0;
  static char result[YMSG9_RESULT_LENGTH];
  size_t result_total;

  result_total = 0;
  result[0] = '\0';
  while (strcmp(ymsg_fields[i], "__END__")) 
    {
      if (!strcmp( ymsg_fields[i], key)) {
	if (result[0])
	  {
	    strcat(result, "|"); 
	  }
	strcat(result, ymsg_fields[i + 1]);
      }
      i += 2;
    }

  return result;
}

char *ymsg9_field( char *key ) {
  int i = 0;
  static char result[2048];

  result[0] = '\0';
  while( strcmp( ymsg_fields[i], "__END__" )) {
    if ( ! strcmp( ymsg_fields[i], key )) {
      if ( result[0] ) {
	strcat( result, "," );
      }
      strcat( result, ymsg_fields[i+1] );
    }
    i += 2;
  }
  return( result );
}

