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
#include "input.h"

YMSG9_SESSION ymsg_session, *ymsg_sess = &ymsg_session;
int lf_from_input = 0;


int
main(int argc, char *argv[]) 
{
  time_t ping_time;
  int mode, ret;

  char input_buffer[INPUT_BUFFER_LENGTH];
  ssize_t input_length;

  if (argc < 2)
    return EXIT_FAILURE;
      
  memset(ymsg_sess, 0, sizeof(YMSG9_SESSION)); 
  ymsg_sess->sock = -1; 
  
  ret = config_init(ymsg_sess, NULL, argv[1]);
  if (ret != 0)
    return EXIT_FAILURE;
  ret = text_init();
  if (ret != 0)
    return EXIT_FAILURE;
  ret = input_init();
  if (ret != 0)
    return EXIT_FAILURE;

  mode = fcntl(fileno(stdin), F_GETFL);
  fcntl(fileno(stdin), F_SETFL, mode | O_NONBLOCK);
  setvbuf(stdin, NULL, _IONBF, 0);

  ret = ymsg9_open_socket(ymsg_sess);
  if (ret == -1) 
    {
      fprintf(stderr, "Error opening socket: %s\n", ymsg_sess->error_msg);
      return EXIT_FAILURE;
    }

  ymsg9_request_key(ymsg_sess);

  for (input_length = 0, ping_time = 0; ! ymsg_sess->quit; )
    {
      struct timeval tv;
      fd_set set;

      FD_ZERO(&set);
      FD_SET(ymsg_sess->sock, &set);
      FD_SET(fileno(stdin), &set);
      tv.tv_sec = 0;
      tv.tv_usec = 250000;
      ret = select(ymsg_sess->sock + 1, &set, NULL, NULL, &tv);

      if (ret == -1)
        {
          fprintf(stderr, "Error on select: %s\n", strerror(errno));
          return EXIT_FAILURE;
        }
      else if (ret > 0) 
        {
          if (FD_ISSET(ymsg_sess->sock, &set) &&  ymsg9_recv_data(ymsg_sess) != -1) 
            show_yahoo_packet();
          
          if (FD_ISSET(fileno(stdin), &set)) 
            {
              char input;

              ret = read(fileno(stdin), &input, 1);
              if (ret == -1)
                {
                  fprintf(stderr, "Error reading from standard input: %s\n", strerror(errno));
                  return EXIT_FAILURE;
                }
              else if (ret > 0) 
                {
                  input_buffer[input_length++] = input;
                  if (input == 0 && input_length > 0)
                    input_handler(ymsg_sess, input_buffer);

                  input_buffer[0] = '\0';
                  input_length = 0;
                }
            }
        }
      /* ping regularly, every 5 minutes */
      if (ping_time < (time(NULL) - (5 * 60))) 
        {
          ymsg9_ping(ymsg_sess);
          ping_time = time(NULL);
        }
    }

  return EXIT_SUCCESS;
}

