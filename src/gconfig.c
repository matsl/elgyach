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
#include <errno.h>
#include <string.h>

#include "yahoochat.h"
#include "gconfig.h"
#include "config_api.h"

#define CONFIG_FILE "/.elgyach"
#define FIELD_LENGTH 50

#define DEFAULT_HOST "scs.yahoo.com"
#define DEFAULT_PORT 5050
#define DEFAULT_ROOM "Linux, FreeBSD, Solaris:1"

int
config_init(YMSG9_SESSION *session, const char *config_file, const char *profile)
{
  char *home_var = NULL, *file = NULL;
  int temp;
  config_t config;
  
  struct {
    char host[FIELD_LENGTH + 1];
    char port[FIELD_LENGTH + 1];
    char username[FIELD_LENGTH + 1];
    char password[FIELD_LENGTH + 1];
    char room[FIELD_LENGTH + 1];
  } opt;

  int port;

  memset(&opt, 0, sizeof(opt));

  if (config_file != NULL)
    file = strdup(config_file);
  else
    { 
      home_var = getenv("HOME");
      if (home_var != NULL) 
        {
          file = malloc(strlen(home_var) + strlen(CONFIG_FILE) + 1); 
          if (file == NULL) 
            {
              temp = errno;   
              session->error_msg = strdup(strerror(errno));
              goto error_malloc;
            }
          strcpy(file, home_var);
          strcat(file, CONFIG_FILE);
          if (! config_open(&config, file, C_READ)) 
            {
              temp = errno;
              session->error_msg = strdup(strerror(errno));
              goto error_open;
            }
          config_read(&config, "general", "host", opt.host, FIELD_LENGTH);
          config_read(&config, "general", "port", opt.port, FIELD_LENGTH);
          config_read(&config, profile, "username", opt.username, FIELD_LENGTH);
          config_read(&config, profile, "password", opt.password, FIELD_LENGTH);
          config_read(&config, profile, "room", opt.room, FIELD_LENGTH);

          port = atoi(opt.port);

	  /* validate host and port settings */
	  if (! *opt.host) 
	    {
	      strncpy(session->host, DEFAULT_HOST, YMSG9_HOST_SIZE);
	      session->host[YMSG9_HOST_SIZE] = '\0';
	    }
	  else 
	    {
	      strncpy(session->host, opt.host, YMSG9_HOST_SIZE);
	      session->host[YMSG9_HOST_SIZE] = '\0';
	    }
	  if (! *opt.port)
	    session->port = DEFAULT_PORT;
	  else if (port == 0)
	    {
	      temp = errno;
	      session->error_msg = strdup("invalid port number");
	      goto error_port;
	    }
	  else 
	    session->port = port;
	  /* validate username, password and room settings */
	  if (! *opt.username || ! *opt.password)
	    {
	      temp = errno;
	      session->error_msg = strdup("username and password must be given");
	      goto error_username_password;
	    }
	  strncpy(session->user, opt.username, YMSG9_USER_SIZE);
	  session->user[YMSG9_USER_SIZE] = '\0';
	  strncpy(session->password, opt.password, YMSG9_PASSWORD_SIZE);
	  session->password[YMSG9_PASSWORD_SIZE] = '\0';
	  if (! *opt.room)
	    {
	      strncpy(session->room, DEFAULT_ROOM, YMSG9_ROOM_SIZE);
	      session->room[YMSG9_ROOM_SIZE] = '\0';
	      goto error_room;
	    }
	  else 
	    {
	      strncpy(session->room, opt.room, YMSG9_ROOM_SIZE);
	      session->room[YMSG9_ROOM_SIZE] = '\0';
	    }
        }
      else 
        { 
          temp = errno;
          session->error_msg = strdup("failure to determine HOME environement variable");
          goto error_home;
        }
    }
  config_close(&config);
  free(file);
  return 0;

 error_room:
 error_username_password:
 error_port:
  config_close(&config);
 error_open:
  free(file);
 error_malloc:
 error_home:
  errno = temp;
  return -1;
}
