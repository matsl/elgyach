/* main.c: main loop
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
 *
 * cpinkham@corp.infi.net, cpinkham@bc2va.org
 * mkennedy@gentoo.org
 */

#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <sys/wait.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "main.h"
#include "usage.h"
#include "yahoochat.h"
#include "gconfig.h"
#include "text.h"
#include "output.h"
#include "config.h"

YMSG9_SESSION ymsg_session, *ymsg_sess = &ymsg_session;
time_t connect_time = 0;
char input[2048] = "";
int fancy_gyach_text = 0;
int show_colors = 0;
int no_black = 0;
int lf_from_input = 0;

int join_state = 0;

#define GYACH_URL "http://www4.infi.net/~cpinkham/gyach/"


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

/* THIS FUNCTION NOT VERIFIED */

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

#define MAX_WORDS 1000

void show_yahoo_packet() {
	char buf[1024];  /* these are static so they aren't on the stack */
	char tmp[2048];
	char tmp2[2048];
	char tmp3[2048];
	char *ptr;
	char *src;
	time_t time_now;
	time_t exp_time;

	int nwords = 0;
	int max_words = MAX_WORDS;
	char *words[MAX_WORDS];
	int i;

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

				sprintf( buf, "Logging in to chat server...\n" );
 				append_to_textbox( buf );

				/* reset cookies */
				ymsg_sess->cookie[0] = '\0';
				break;

		case YMSG9_COOKIE:
				/* set online if we got the right cookie packet */
				if ( strcmp( ymsg9_field( "3" ), "" )) {
					sprintf( buf, "Setting status to Online...\n" );
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

				/* yahoo buddy list */
/* 				if ( strcmp( ymsg9_field( "87" ), "" )) { */
/* 					sprintf( buf, */
/* 						"%s%sBuddies%s: %s\n", */
/* 						ANSI_BOLDON, ANSI_COLOR_BLUE, ANSI_ATTR_RESET, */
/* 						ymsg9_field( "87" )); */
/* 					append_to_textbox( buf ); */
/* 				} */

				/* yahoo ignore list */
/* 				if ( strcmp( ymsg9_field( "88" ), "" )) { */
/* 					sprintf( buf, "%s%sYahoo Ignore List%s: %s\n", */
/* 						ANSI_BOLDON, ANSI_COLOR_CYAN, ANSI_ATTR_RESET, */
/* 						ymsg9_field( "88" )); */
/* 					append_to_textbox( buf ); */
/* 				} */
				break;

		case YMSG9_ONLINE:
				ymsg9_join( ymsg_sess );
				break;

		case YMSG9_MAIL:
/* 				sprintf( buf, "Gyach - You have %s%s%s email(s).  " */
/* 						"http://mail.yahoo.com\n", */
/* 					ANSI_COLOR_RED, ymsg9_field( "9" ), ANSI_ATTR_RESET ); */
/* 				append_to_textbox( buf ); */
				break;

		case YMSG9_COMMENT: 
				/* user comment/emote/thought */
				strcpy( tmp, ymsg9_field( "109" ));
				strcpy( tmp2, ymsg9_field( "117" ));

				if ( ! strcmp( ymsg9_field( "124"), "1" )) {
					sprintf( buf, "%s%s%s%s: %s%s%s\n",
						ANSI_BOLDON, tmp, ANSI_ATTR_RESET, ANSI_BOLDOFF,
						ANSI_ATTR_RESET, tmp2, ANSI_ATTR_RESET );
				} else {
					sprintf( buf, "* %s%s%s%s %s%s\n",
						 ANSI_BOLDON, tmp, ANSI_ATTR_RESET, ANSI_BOLDOFF,
						 tmp2, ANSI_ATTR_RESET );
				}
				append_to_textbox( buf );

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

					/* avoid sending this more than once */
					if (join_state == 0) {
					  make_tokens(tmp, max_words, &nwords, words, ",");
					  for (i = 0; i < nwords; i++)
					    {
					      sprintf(buf, "%s enters the room\n", words[i]);
					      append_to_textbox( buf );
					    }
					  join_state = 1;
					}

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

		case YMSG9_BUDDY_ON:
/* 				if ( ! strcmp( ymsg9_field( "7" ), "" )) { */
/* 					return; */
/* 				} */

/* 				if ( strcmp( ymsg9_field( "8" ), "" )) { */
					/* initial buddy list of people online */
/* 					sprintf( buf, "Friends Online: %s\n", ymsg9_field( "7")); */
/* 					append_to_textbox( buf ); */
/* 					return; */
/* 				} */
			
/* 				strcpy( tmp, ymsg9_field( "7" )); */

/* 				if ( ! strcasecmp( tmp, ymsg_sess->user )) { */
/* 					return; */
/* 				} */

/* 				sprintf( buf, "%sBuddy:%s %s%s%s has logged ON.\n", */
/* 					ANSI_COLOR_BLUE, ANSI_ATTR_RESET, ANSI_COLOR_RED, */
/* 					tmp, ANSI_ATTR_RESET ); */
/* 				append_to_textbox( buf ); */
				break;

		case YMSG9_BUDDY_OFF:
/* 				if ( ! ymsg_sess->pkt.size ) { */
/* 					ymsg_sess->pkt.type = YMSG9_LOGOUT; */
/* 					show_yahoo_packet(); */
/* 				} */
/* 				if ( ! strcmp( ymsg9_field( "7" ), "" )) { */
/* 					return; */
/* 				} */
/* 				strcpy( tmp, ymsg9_field( "7")); */

/* 				if ( ! strcasecmp( tmp, ymsg_sess->user )) { */
/* 					return; */
/* 				} */

/* 				sprintf( buf, "%sBuddy:%s %s%s%s has logged OFF.\n", */
/* 					ANSI_COLOR_BLUE, ANSI_ATTR_RESET, ANSI_COLOR_RED, */
/* 					tmp, ANSI_ATTR_RESET ); */
/* 				append_to_textbox( buf ); */
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

		case YMSG9_FILETRANSFER:
/* 				strcpy( tmp, ymsg9_field( "4" )); */

/* 				if ( ! strcmp( tmp, "FILE_TRANSFER_SYSTEM" )) { */
					/* we sent a file */
/* 					strcpy( tmp2, ymsg9_field( "14" )); */
/* 					sprintf( buf, "FILE: %s\n", tmp2 ); */
/* 					append_to_textbox( buf ); */
/* 				} else { */
					/* we received a file */
/* 					strcpy( tmp2, ymsg9_field( "20" )); */
/* 					strcpy( tmp3, ymsg9_field( "14" )); */
/* 					exp_time = atoi( ymsg9_field( "38" )); */
/* 					sprintf( buf, "FILE: %s has sent you a file with " */
/* 						"the message: '%s'.\n" */
/* 						"        %s\n" */
/* 						"        It is available until: %s", */
/* 						tmp, tmp3, tmp2, ctime( &exp_time )); */
/* 					append_to_textbox( buf ); */
/* 				} */
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

void cmd_tell( char *args ) {
	char *ptr;

	args = skip_whitespace( args );
	ptr = args;

	if ( strlen( ptr )) {
		/* they gave a user & message so send message */
		/* format is "tell dummytext\001user\001message" */
		ptr = find_whitespace( ptr );
		*ptr = '\0';
		ptr++;

		ymsg9_pm( ymsg_sess, args, ptr );
	}
}


void cmd_exec( char *cmd, int local ) {
	char buf[601] = "";
	int p[2];
	int pid;
	int chars;
	char new_cmd[513];
	char *ptr;
	char tmp[1024];
	int chars_read = 0;
	int elapsed = 0;
	int timed_out = 0;
	int flags;


	pipe( p );
	strcpy( buf, cmd );
	strcat( buf, ":\n" );
	chars = strlen( buf );

	sprintf( new_cmd, "%s 2>&1", cmd );

	if (( pid = fork()) == -1 ) {
		strcat( buf, "couldn't fork!\n" );
	} else if ( pid ) {
		close( p[1] );
		flags = fcntl( p[0], F_GETFL, 0 );
		fcntl( p[0], F_SETFL, flags | O_NONBLOCK );
		ptr = buf + chars;
		while( chars < 512 ) {
			if ( elapsed > ( 3 * 1000 * 1000 )) {
				timed_out = 1;
				break;
			}

			if (( chars_read = read( p[0], ptr, (512 - chars ))) < 0 ) {
				usleep( 50000 );
				elapsed += 50000;
				continue;
			} else if ( chars_read == 0 ) {
				break;
			}
			chars += chars_read;
			ptr += chars_read;
			*ptr = 0;
		}

		if ( kill( pid, 0 ) == 0 ) {
			kill( pid, SIGKILL );
		}

		if ( chars > 511 ) {
			buf[509] = '.';
			buf[510] = '.';
			buf[511] = '.';
		}

		waitpid( pid, NULL, 0 );
	} else {
		close( p[0] );
		if ( p[1] != STDOUT_FILENO ) {
			dup2( p[1], STDOUT_FILENO );
			close( p[1] );
		}
		if ( STDERR_FILENO != STDOUT_FILENO ) {
			dup2(STDOUT_FILENO, STDERR_FILENO);
		}

		execlp( "sh", "sh", "-c", new_cmd, NULL );
	}

	/* ! or !! in chat window */
	if ( local ) {
		strcat( buf, "\n" );
		append_to_textbox( buf );
	} else {
		ymsg9_comment( ymsg_sess, buf );
		sprintf( tmp, "%s| %s\n", ymsg_sess->user, buf );
		append_to_textbox( tmp );
	}
}

void chat_command( char *cmd ) {
	char buf[2048] = "";
	time_t current_time;
	int hours, minutes, seconds;
	struct utsname un;
	char *ptr = NULL;
	char *action = NULL;
	char *args = NULL;
	char tmp_input[ 512 ];

	strcpy( tmp_input, cmd );

	ptr = skip_whitespace( cmd );

	action = skip_whitespace( tmp_input ); /* find head of command */
	args = action;
	args = find_whitespace( args ); /* find whitespace after command */
	if ( *args ) {
		*args = '\0';
		args++;
		args = skip_whitespace( args ); /* find text after whitespace */
	}

	if ( action[0] == '\0' )
		return;

	if ( action[0] == '/' ) {
		action++;
		if ( ! strcasecmp( action, "think" )) {
			sprintf( buf, "* %s . o O ( %s )\n", ymsg_sess->user, args );
			ymsg9_think( ymsg_sess, args );
		} else if ( ! strcasecmp( action, "help" )) {
			sprintf( buf,
				"------------------------------------------------------------\n"
				"  Gyach-Text v" VERSION " - Copyright 2000-2002 "
					"Chris T. Pinkham, ALL RIGHTS RESERVED\n"
				"\n"
				"Chat Commands:\n"
				"/think text   - think 'text'\n"
				"/ignore user  - ignore a user matching regex\n"
				"/quit         - quit Gyach-Text\n"
				"/ver          - print Gyach-Text version\n"
				":text         - send 'text' as emote\n"
				"!cmd          - run 'cmd' and send results to chat room\n"
				"!!cmd         - run 'cmd' and display results locally\n"
				"------------------------------------------------------------\n"
				"\n" );
		} else if ( ! strcasecmp( action, "ver" )) {
			uname( &un );
			current_time = time(NULL);
			hours = (int)(( current_time - connect_time ) / 3600);
			minutes = (int)(( current_time - connect_time ) % 3600 ) / 60;
			seconds = (int)(( current_time - connect_time ) % 60 );
			sprintf( buf,
				"is using Gyach-Text v%s %s online for %d:%02d:%02d "
				"under %s %s [%s]",
				VERSION, GYACH_URL,
				hours, minutes, seconds, un.sysname, un.release, un.machine );
			ymsg9_emote( ymsg_sess, buf );
			sprintf( buf, "%s%s%s%s %s"
				"is using Gyach-Text v%s %s%s%s%s%s online for %d:%02d:%02d "
				"under %s %s [%s]%s\n",
				ANSI_COLOR_BLUE, ANSI_BOLDON, ymsg_sess->user,
				ANSI_ATTR_RESET, ANSI_COLOR_PURPLE,
				VERSION, ANSI_COLOR_BLUE, ANSI_BOLDON,
				GYACH_URL, ANSI_ATTR_RESET, ANSI_COLOR_PURPLE,
				hours, minutes, seconds, un.sysname, un.release, un.machine,
				ANSI_ATTR_RESET );
		} else if ( ! strcasecmp( action, "quit" )) {
			ymsg9_logout( ymsg_sess );
			close( ymsg_sess->sock );
			printf( "Gyach-Text exiting...\n" );
			exit(EXIT_SUCCESS);
		} else if ( ! strcasecmp( action, "clear" )) {
			if ( fancy_gyach_text ) {
				printf( ANSI_CLEAR );
				printf( ANSI_HOME );
				fflush( stdout );
			} else {
				sprintf( buf, "/clear unsupported unless in -f mode\n" );
			}
		} else if ( ! strcasecmp( action, "join" )) {
			strcpy( ymsg_sess->room, args );
			ymsg9_join( ymsg_sess );
		} else if ( ! strcasecmp( action, "goto" )) {
			ymsg9_goto( ymsg_sess, args );
		} else if ( ! strcasecmp( action, "invite" )) {
			ymsg9_invite( ymsg_sess, args, ymsg_sess->room );
		} else if ( ! strcasecmp( action, "tell" )) {
			cmd_tell(args);
		} 
	} else if ( ptr[0] == '!' ) {
		ptr++;
		if ( ptr[0] == '!' ) {
			cmd_exec( ptr + 1, 1 );
		} else {
			cmd_exec( ptr, 0 );
		}
	} else if (ptr[0] == ':' && ptr[1] == ' ') {
		ptr++;
		sprintf( buf, "* %s %s\n", ymsg_sess->user, ptr);
		ymsg9_emote(ymsg_sess, ptr);
	} else {
		sprintf(buf, "%s: %s\n", ymsg_sess->user, ptr);
		ymsg9_comment(ymsg_sess, ptr);
	}

	input[0] = '\0';
	if ( buf[0] ) {
		append_to_textbox( buf );
	}
}

int
main(int argc, char **argv) {
  struct timeval tv;
  time_t ping_time = 0;
  fd_set set;
  int ret;
  int result;
  char *ptr = input;
  int len = 0;
  int mode;
  int c;

  ret = config_init();
  if (ret != 0)
    return EXIT_FAILURE;

  ret = text_init();
  if (ret != 0)
    return EXIT_FAILURE;

  memset( ymsg_sess, 0, sizeof(YMSG9_SESSION)); 
  ymsg_sess->sock = -1; 
  ymsg_sess->port = YMSG9_CHAT_PORT;
  strcpy( ymsg_sess->room, "linux, freebsd, solaris:1" );
  strcpy( ymsg_sess->host, "scs.yahoo.com" );

  while ((c = getopt(argc, argv, "u:p:r:s:fcb")) != -1) {
    switch( c ) {
    case 'u':	strcpy( ymsg_sess->user, optarg );
      break;
    case 'p':	strcpy( ymsg_sess->password, optarg );
      break;
    case 'r':	strcpy( ymsg_sess->room, optarg );
      break;
    case 's':	strcpy( ymsg_sess->host, optarg );
      break;
    case 'f':	fancy_gyach_text = 1;
      break;
    case 'c':	show_colors = 1;
      break;
    case 'b':	no_black = 1;
      break;
    }
  }

  if ( ! ymsg_sess->user[0] ) {
    usage(argv[0], stderr);
    return EXIT_FAILURE;
  }

  if ( ! ymsg_sess->password[0] ) {
    printf( "Password: " );
    if ( fgets( ymsg_sess->password, YMSG9_PASSWORD_SIZE, stdin )) {
      if ( ymsg_sess->password[0] ) {
	/* strip the newline */
	ymsg_sess->password[strlen(ymsg_sess->password)-1] = '\0';
	if ( fancy_gyach_text ) {
	  printf( ANSI_UP
		  "\r                                              \r" );
	}
      } else {
	usage(argv[0], stderr);
	return EXIT_FAILURE;
      }
    }
  }


  mode = fcntl( fileno( stdin ), F_GETFL );
  fcntl( fileno( stdin ), F_SETFL, mode | O_NONBLOCK );
  setvbuf( stdin, (char *)NULL, _IONBF, 0 );

  result = ymsg9_open_socket( ymsg_sess );

  connect_time = time(NULL);

  if ( ! result ) {
    printf( "error opening socket: %s\n", ymsg_sess->error_msg );
    exit( 0 );
  }

  ymsg9_request_key( ymsg_sess );

  while( ! ymsg_sess->quit ) {
    /* now check for input on our socket */
    FD_ZERO( &set );
    if ( ymsg_sess->sock != -1 )
      FD_SET( ymsg_sess->sock, &set );

    FD_SET( fileno( stdin ), &set );

    tv.tv_sec = 0;
    tv.tv_usec = 250000;
    ret = select( ymsg_sess->sock + 1, &set, NULL, NULL, &tv );

    if ( ret ) {
      if ( FD_ISSET( ymsg_sess->sock, &set )) {
	if ( ymsg9_recv_data( ymsg_sess )) {
	  show_yahoo_packet();
	}
      }

      if ( FD_ISSET( fileno( stdin ), &set )) {
	ret = read( fileno( stdin ), ptr, 2048 - len );
	if ( fancy_gyach_text ) {
	  if ( ret ) {
	    if ( ret == 1 ) {
	      if (( *ptr == '' ) ||
		  ( *ptr == 127 )) {
		if ( len > 0 ) {
		  ptr--;
		  *ptr = '\0';
		  len = strlen( input );
		  printf( "\b \b" ); fflush( stdout );
		}
	      } else {
		ptr[ret] = '\0';
		len += ret;
		ptr += ret;
	      }
	    } else {
	      ptr[ret] = '\0';
	      len += ret;
	      ptr += ret;
	    }
	  }

	  if (( ret ) &&
	      ( *(ptr-1) == '\n' )) {
	    *(ptr-1) = '\0';
	    if ( len != 1 ) {
	      lf_from_input = 1;
	      chat_command( input );
	    }
	    input[0] = '\0';
	    ptr = input;
	    len = 0;
	  }
	} else {
	  if ( ret ) {
	    ptr[ret] = '\0';
	    len += ret;
	    ptr += ret;

	    if ( input[len-1] == '\n' ) {
	      input[len-1] = '\0';
	      if ( len != 1 ) {
		chat_command( input );
	      }
	      input[0] = '\0';
	      ptr = input;
	      len = 0;
	    }
	  }
	}
      }
    }

    /* ping regularly, every 5 minutes */
    if ( ping_time < ( time(NULL) - ( 5 * 60 ))) {
      ymsg9_ping( ymsg_sess );
      ping_time = time(NULL);
    }
  }

  return( 0 );
}

