/* yahoochat.c: ymsg9 protocol
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
 * Released under the terms of the GPL.
 * *NO WARRANTY*
 *
 * cpinkham@corp.infi.net, cpinkham@bc2va.org
 * mkennedy@gentoo.org
 * http://www4.infi.net/~cpinkham/gyach/
 *
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <netdb.h>
#include <errno.h>
#include <unistd.h>

#include "yahoochat.h"
#include "yahoo_md5.h"
#include "socket.h"

static u_char PACKET_BUFFER[MAX_PACKET_LENGTH + 1]; /* + 1 for a null terminator */


/* CONVERTED */
int 
ymsg9_open_socket(YMSG9_SESSION *session) {
  struct sockaddr_in addr;
  struct hostent *hinfo;
  int sock = 1;
  int temp;

  /* initialize the session vars */
  session->sock = -1;
  memset(&addr, 0, sizeof(addr)); 

  addr.sin_addr.s_addr = inet_addr(session->host); 
  
  if (addr.sin_addr.s_addr == (u_int) INADDR_NONE) 
    {
      hinfo = gethostbyname(session->host); 
      
      if (!hinfo || hinfo->h_addrtype != AF_INET) 
        {
          temp = errno;
          session->error_msg = strdup(hstrerror(h_errno));
          goto error_gethostbyname;
        } 
      memset(&addr, 0, sizeof(addr)); 
      memcpy(&addr.sin_addr.s_addr, hinfo->h_addr, hinfo->h_length); 
    } 
  addr.sin_family = AF_INET; 
  addr.sin_port = htons(session->port); 
  sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP); 
  if (sock == -1) 
    {
      temp = errno;
      session->error_msg = strdup(strerror(errno));
      goto error_socket;
    } 
  else if (connect(sock, (struct sockaddr*) &addr, sizeof(addr)) == -1) 
    {
      temp = errno;
      session->error_msg = strdup(strerror(errno));
      goto error_connect;
    } 
  
  session->sock = sock;
  
  return sock;

 error_connect:
  close(sock);
 error_socket:
 error_gethostbyname:

  if (errno == 0) 
    h_errno = temp;
  else
    errno = temp;

  return -1;
}

/* CONVERTED */
int 
ymsg9_sock_send(YMSG9_SESSION *session, u_char *buf, int size) 
{
  int sent, temp;
  char *ptr;

  for (ptr = buf; size; ) 
    {
      sent = send(session->sock, ptr, size, 0);
      if (sent == -1) 
        {
          temp = errno;
          session->error_msg = strdup(strerror(errno));
          goto error_send;
        }
      ptr += sent;
      size -= sent;
    }
  return 0;

 error_send:
  errno = temp;

  return -1;
}

/* CONVERTED */
int 
ymsg9_sock_recv(YMSG9_SESSION *session, u_char *buf, int size) 
{
  int recvd, temp;
  char *ptr;

  for (ptr = buf; size; ) 
    {
      recvd = recv(session->sock, ptr, size, 0);
      if (recvd == -1) 
        {
          temp = errno;
          session->error_msg = strdup(strerror(errno));
          goto error_recv;
        }
      ptr += recvd;
      size -= recvd;
    }
  return 0;
  
 error_recv:
  errno = temp;
 
  return -1;
}



/* CONVERTED */
u_char *
ymsg9_header(YMSG9_SESSION *session, u_long pkt_type) 
{
  u_char *ptr = PACKET_BUFFER;

  memcpy(ptr, "YMSG", 4);
  ptr += 4;  
  switch (pkt_type) 
    {
    case YMSG9_AWAY:
    case YMSG9_BACK:
      *((u_long *) ptr) = htonl(0x00070000);	
      ptr += 4;  
      break;
    case YMSG9_ADD_BUDDY:
    case YMSG9_REM_BUDDY:
      *((u_long *) ptr) = htonl(0x06000000);	
      ptr += 4;  
      break;
    default:
      *((u_long *) ptr) = htonl(0x09000000);	
      ptr += 4;  
      break;
    }
  /* next 4 bytes are packet length which we'll fill in later, so set
     to 0 for now */
  *((u_short *) ptr) = htons(0x0000);		
  ptr += 2;  /* data length  */
  *((u_short *) ptr) = htons(pkt_type);
  ptr += 2;  /* packet type  */
  *((u_long *) ptr) = htonl(0x00000000);
  ptr += 4;  /* status       */
  switch(pkt_type) 
    {
    case YMSG9_ADD_BUDDY:
    case YMSG9_REM_BUDDY:
      *((u_long *) ptr) = htonl(0x00000000);
      ptr += 4;  /* Session ID   */
      break;
    default:
      *((u_long *) ptr) = htonl(session->session_id);	
      ptr += 4;  /* Session ID   */
      break;
    }

  return ptr;
}

/* CONVERTED */
int 
ymsg9_send_packet(YMSG9_SESSION *session, int size) 
{
  /* set data length in packet */
  *((u_short *) (PACKET_BUFFER + 8)) = htons(size - MAX_HEADER_LENGTH);
  /* don't try to send if we're not connected */
  if (session->sock == -1)
    return 0;

  return ymsg9_sock_send(session, PACKET_BUFFER, size);
}


/* CONVERTED */
int
ymsg9_recv_data(YMSG9_SESSION *session) 
{
  if (ymsg9_sock_recv(session, PACKET_BUFFER, MAX_HEADER_LENGTH) == -1)
    return -1;
  session->pkt.size = ntohs((u_short) *((u_short *) (PACKET_BUFFER + 8)));
  if (ymsg9_sock_recv(session, PACKET_BUFFER + MAX_HEADER_LENGTH, session->pkt.size ) == -1)
    return -1;
  session->pkt.type = ntohs((u_short) *((u_short *) (PACKET_BUFFER + 10)));
  if (session->pkt.type == YMSG9_GET_KEY)
    session->session_id  = ntohl((u_long) *((u_long *) (PACKET_BUFFER + 16)));
  
  PACKET_BUFFER[session->pkt.size + 20] = '\0';

  memcpy(session->pkt.data, PACKET_BUFFER + MAX_HEADER_LENGTH, session->pkt.size);
  session->pkt.data[session->pkt.size] = '\0';
  
  return 1;
}


/* CONVERTED */
int
ymsg9_request_key(YMSG9_SESSION *session) 
{
  u_char *ptr = ymsg9_header(session, YMSG9_GET_KEY);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "1%s%s%s", YMSG9_SEP, session->user, YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);
	
  return 0;
}

/* CONVERTED */
int 
ymsg9_login(YMSG9_SESSION *session, char *key) 
{
  u_char *ptr = ymsg9_header(session, YMSG9_LOGIN);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
                 "0", YMSG9_SEP, session->user, YMSG9_SEP,
                 "6", YMSG9_SEP, getstr1( session->user, session->password, key ), YMSG9_SEP,
                 "96", YMSG9_SEP, getstr2( session->user, session->password, key ), YMSG9_SEP,
                 "2", YMSG9_SEP, session->user, YMSG9_SEP,
                 "1", YMSG9_SEP, session->user, YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);
	
  return 0;
}

/* CONVERTED */
int
ymsg9_online(YMSG9_SESSION *session) 
{
  u_char *ptr = ymsg9_header(session, YMSG9_ONLINE);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s%s%s%s%s%s%s%s%s",
                 "109", YMSG9_SEP, session->user, YMSG9_SEP,
                 "1", YMSG9_SEP, session->user, YMSG9_SEP,
                 "6", YMSG9_SEP, "abcde", YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);

  return 0;
}

/* CONVERTED */
int 
ymsg9_join(YMSG9_SESSION *session) 
{
  u_char *ptr = ymsg9_header(session, YMSG9_JOIN);
  int len;
  
  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
                 "1", YMSG9_SEP, session->user, YMSG9_SEP,
                 "62", YMSG9_SEP, "2", YMSG9_SEP,
                 "104", YMSG9_SEP, session->room, YMSG9_SEP,
                 "129", YMSG9_SEP, "0", YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);
  
  return 0;
}

/* CONVERTED */

int
ymsg9_comment(YMSG9_SESSION *session, char *text) 
{
  u_char *ptr = ymsg9_header(session, YMSG9_COMMENT);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
                 "1", YMSG9_SEP, session->user, YMSG9_SEP,
                 "104", YMSG9_SEP, session->room, YMSG9_SEP,
                 "117", YMSG9_SEP, text, YMSG9_SEP,
                 "124", YMSG9_SEP, "1", YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);
	
  return 0;
}

/* CONVERTED */
int
ymsg9_emote(YMSG9_SESSION *session, char *text)
{
  u_char *ptr = ymsg9_header(session, YMSG9_COMMENT);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
                 "1", YMSG9_SEP, session->user, YMSG9_SEP,
                 "104", YMSG9_SEP, session->room, YMSG9_SEP,
                 "117", YMSG9_SEP, text, YMSG9_SEP,
                 "124", YMSG9_SEP, "2", YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);
	
  return 0;
}

/* CONVERTED */
int 
ymsg9_think(YMSG9_SESSION *session, char *text) 
{
  u_char *ptr = ymsg9_header(session, YMSG9_COMMENT);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s%s%s%s%s%s%s. o O ( %s )%s%s%s%s%s",
                 "1", YMSG9_SEP, session->user, YMSG9_SEP,
                 "104", YMSG9_SEP, session->room, YMSG9_SEP,
                 "117", YMSG9_SEP, text, YMSG9_SEP,
                 "124", YMSG9_SEP, "3", YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);
	
  return 0;
}

/* CONVERTED */
int
ymsg9_logout(YMSG9_SESSION *session)
{
  u_char *ptr = ymsg9_header(session, YMSG9_LOGOUT);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s",
                 "1", YMSG9_SEP, session->user, YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);

  return 0;
}

/* CONVERTED */
int
ymsg9_ping(YMSG9_SESSION *session) 
{
  u_char *ptr = ymsg9_header(session, YMSG9_PING);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s",
                 "109", YMSG9_SEP, session->user, YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);
  
  return 0;
}

/* CONVERTED */
int 
ymsg9_pm(YMSG9_SESSION *session, char *remote_user, char *msg)
{
  u_char *ptr = ymsg9_header(session, YMSG9_PM);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
                 "1", YMSG9_SEP, session->user, YMSG9_SEP,
                 "5", YMSG9_SEP, remote_user, YMSG9_SEP,
                 "14", YMSG9_SEP, msg, YMSG9_SEP,
                 "97", YMSG9_SEP, "1", YMSG9_SEP,
                 "63", YMSG9_SEP, ";0", YMSG9_SEP,
                 "64", YMSG9_SEP, "0", YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);
  
  return 0;
}

/* CONVERTED */
int 
ymsg9_add_buddy(YMSG9_SESSION *session, char *friend) 
{
  u_char *ptr = ymsg9_header(session, YMSG9_ADD_BUDDY);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s%s%s%s%s%s%s%s%s",
                 "1", YMSG9_SEP, session->user, YMSG9_SEP,
                 "7", YMSG9_SEP, friend, YMSG9_SEP,
                 "65", YMSG9_SEP, "Friends", YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);

  return 0;
}

/* CONVERTED */
int 
ymsg9_remove_buddy(YMSG9_SESSION *session, char *friend) 
{
  u_char *ptr = ymsg9_header(session, YMSG9_REM_BUDDY);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s%s%s%s%s%s%s%s%s",
                 "1", YMSG9_SEP, session->user, YMSG9_SEP,
                 "7", YMSG9_SEP, friend, YMSG9_SEP,
                 "65", YMSG9_SEP, "Friends", YMSG9_SEP);
  len += MAX_HEADER_LENGTH;

  ymsg9_send_packet(session, len);

  return 0;
}

/* CONVERTED */
int 
ymsg9_goto(YMSG9_SESSION *session, char *friend) 
{
  u_char *ptr = ymsg9_header(session, YMSG9_GOTO);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s%s%s%s%s%s%s%s%s",
                 "109", YMSG9_SEP, friend, YMSG9_SEP,
                 "1", YMSG9_SEP, session->user, YMSG9_SEP,
                 "62", YMSG9_SEP, "2", YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);

  return 0;
}

/* CONVERTED */
int
ymsg9_invite(YMSG9_SESSION *session, char *remote_user, char *room)
{
  u_char *ptr = ymsg9_header(session, YMSG9_INVITE);
  int len;

  len = snprintf(ptr, MAX_DATA_LENGTH, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
                 "1", YMSG9_SEP, session->user, YMSG9_SEP,
                 "118", YMSG9_SEP, remote_user, YMSG9_SEP,
                 "104", YMSG9_SEP, room, YMSG9_SEP,
                 "117", YMSG9_SEP, "", YMSG9_SEP,
                 "129", YMSG9_SEP, "0", YMSG9_SEP);
  len += MAX_HEADER_LENGTH;
  ymsg9_send_packet(session, len);

  return 0;
}
