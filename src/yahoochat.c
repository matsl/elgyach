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

#define SCRATCH 1024

int 
ymsg9_open_socket(YMSG9_SESSION *session) {
  struct sockaddr_in addr;
  struct hostent *hinfo;
  int sock = 1;
  int temp;
  char scratch[SCRATCH];
  char tmp[128];

  /* initialize the session vars */
  session->sock = -1;
  memset(&addr, 0, sizeof(addr)); 

  /* figure out the hosts address */
  if (session->proxy_host[0])
    addr.sin_addr.s_addr = inet_addr(session->proxy_host); 
  else
    addr.sin_addr.s_addr = inet_addr(session->host); 
  
  if (addr.sin_addr.s_addr == (u_int) INADDR_NONE) 
    {
      if (session->proxy_host[0]) 
        hinfo = gethostbyname(session->proxy_host); 
      else 
        hinfo = gethostbyname(session->host); 
      
      if (!hinfo || hinfo->h_addrtype != AF_INET) 
        {
          temp = errno;

          if (session->proxy_host[0]) 
            session->error_msg = strdup(hstrerror(h_errno));
          else 
            session->error_msg = strdup(hstrerror(h_errno));

          goto error_gethostbyname;
        } 
      memset(&addr, 0, sizeof(addr)); 
      memcpy(&addr.sin_addr.s_addr, hinfo->h_addr, hinfo->h_length); 
    } 

  addr.sin_family = AF_INET; 

  if (session->proxy_host[0]) 
    addr.sin_port = htons(session->proxy_port); 
  else
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
  
  /* if we are using a proxy host, send the CONNECT string */
  if (session->proxy_host[0]) 
      {
        snprintf(scratch, SCRATCH, "CONNECT %s:%d HTTP/1.1\r\n", session->host, session->port);
        if (write_to_socket(sock, scratch, strlen(scratch)) == -1)
          goto error_proxy_write;
        snprintf(scratch, SCRATCH, "Host: %s:%d\r\n", session->host, session->port);
        if (write_to_socket(sock, scratch, strlen(scratch)) == -1)
          goto error_proxy_write;
        snprintf(scratch, SCRATCH, "\r\n");
        if (write_to_socket(sock, scratch, strlen(scratch)) == -1)
          goto error_proxy_write;

        /* get the response from the proxy */
        read(sock, tmp, sizeof(tmp) - 1);
        
        if (!strstr(tmp, "OK")) 
          {
            /* proxy connect failed */
            temp = errno;
            session->error_msg = strdup(strerror(errno));
            goto error_proxy_read;
          }
        close(sock);
        return  0;
      }

  session->sock = sock;
  
  return sock;

 error_proxy_read:
 error_proxy_write:
 error_connect:
  close(sock);
 error_socket:
 error_gethostbyname:

  if (errno == 0) 
    h_errno = temp;
  else
    errno = temp;

  return 0;
}


int 
ymsg9_sock_has_data(YMSG9_SESSION *session) 
{
  int result;
  struct timeval tv;
  fd_set fds;
  char str[2];

  tv.tv_sec = 0;
  tv.tv_sec = 50000;

  FD_ZERO(&fds);
  FD_SET(session->sock, &fds);

  result = select(session->sock + 1, &fds, NULL, NULL, &tv);

  if (result > 0) 
    {
      if (recv(session->sock, str, 1, MSG_PEEK ) < 1)
        result = -1;
    }
		
  return result;
}


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
  return size == 0;
  
 error_send:
  errno = temp;

  return 0;
}


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
  return size == 0;

 error_recv:
  errno = temp;
 
  return 0;
}



u_char *
ymsg9_header(YMSG9_SESSION *session, u_char *buf, u_long pkt_type) 
{
  /* note: header minimum size is 4 + 4 + 2 + 2 + 4 + 4 = 20 */

  u_char *ptr = buf;

  memcpy(buf, "YMSG", 4);					
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

void ymsg9_dump_packet( YMSG9_SESSION *session, u_char *pkt ) {
  int len, type;
  int i = 0;
  int x = 0;
  int y = 0;
  char *ptr;

  if ( strncmp( pkt, "YMSG", 4 )) {
    printf( "ymsg9_dump_packet(), invalid packet\n" );
    return;
  }

  len = ntohs((u_short)*((u_short*)(pkt+8)));
  type = ntohs((u_short)*((u_short*)(pkt+10)));

  fprintf( stderr, "-----------------------------------------------------"
	   "-------------------------\n" );
  fprintf( stderr, "sess id   : %u\n", session->session_id );
  fprintf( stderr, "data len  : %d\n", len );
  fprintf( stderr, "pkt type  : 0x%x\n", type );

  for( i = 0; i < 20; i++ ) {
    fprintf( stderr, "%02x ", *(pkt+i) & 0xFF );
  }
  fprintf( stderr, "\n" );
  for( i = 0; i < 20; i++ ) {
    if ( isgraph( *(pkt+i) ) || *(pkt+i)  == ' ' ) {
      fprintf( stderr, "%c  ", *(pkt+i) );
    } else {
      fprintf( stderr, ".  " );
    }
  }
  fprintf( stderr, "\n" );

  i = 0;
  ptr = pkt + 20;
  while( i < len ) {
    if ( x < 16 ) {
      fprintf( stderr, "%02x ", *(ptr+i) & 0xFF );
      x++;
      i++;
    } else {
      fprintf( stderr, "   " );
      for( ; x > 0 ; x-- ) {
	if ( isgraph( *(ptr+i-x) ) || *(ptr+i-x)  == ' ' ) {
	  fprintf( stderr, "%c", *(ptr+i-x) );
	} else {
	  fprintf( stderr, "." );
	}
      }

      fprintf( stderr, "\n" );
      x = 0;
    }
  }
  for( y = x; y < 16 ; y++ ) {
    fprintf( stderr, "   " );
  }
  fprintf( stderr, "   " );
  for( ; x > 0 ; x-- ) {
    if ( isgraph( *(ptr+i-x) ) || *(ptr+i-x)  == ' ' ) {
      fprintf( stderr, "%c", *(ptr+i-x) );
    } else {
      fprintf( stderr, "." );
    }
  }

  fprintf( stderr, "\n" );
}


int 
ymsg9_send_packet(YMSG9_SESSION *session, u_char *bufp, int size) 
{
  /* set data length in packet */
  *((u_short *) (bufp + 8)) = htons(size - 20);

  /* don't try to send if we're not connected */
  if (session->sock == -1)
    return 0;

  if (session->debug_packets)
    ymsg9_dump_packet(session, bufp);

  return ymsg9_sock_send(session, bufp, size);
}


int ymsg9_recv_data( YMSG9_SESSION *session ) {
	u_char buf[2048];

	if ( ! ymsg9_sock_recv( session, buf, 20 )) {
		return( 0 );
	}

	session->pkt.size = ntohs((u_short)*((u_short*)(buf+8)));

	if ( ! ymsg9_sock_recv( session, buf + 20, session->pkt.size )) {
		return( 0 );
	}

	session->pkt.type = ntohs((u_short)*((u_short*)(buf+10)));

	if ( session->pkt.type == YMSG9_GET_KEY ) {
		session->session_id  = ntohl((u_long)*((u_long *)(buf+16)));
	}

	if ( session->debug_packets )
		ymsg9_dump_packet( session, buf );

	buf[session->pkt.size + 20] = '\0';

	memcpy( session->pkt.data, buf + 20, session->pkt.size );

	session->pkt.data[ session->pkt.size ] = '\0';

	return( 1 );
}

int ymsg9_request_key( YMSG9_SESSION *session ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_GET_KEY );
	int len;

	sprintf( ptr, "1%s%s%s", YMSG9_SEP, session->user, YMSG9_SEP );
	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );
	
	return( 0 );
}

int ymsg9_login( YMSG9_SESSION *session, char *key ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_LOGIN );
	int len;

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
		"0", YMSG9_SEP, session->user, YMSG9_SEP,
		"6", YMSG9_SEP, getstr1( session->user, session->password, key ),
			YMSG9_SEP,
		"96", YMSG9_SEP, getstr2( session->user, session->password, key ),
			YMSG9_SEP,
		"2", YMSG9_SEP, session->user, YMSG9_SEP,
		"1", YMSG9_SEP, session->user, YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );
	
	return( 0 );
}

int ymsg9_online( YMSG9_SESSION *session ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_ONLINE );
	int len;

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s",
		"109", YMSG9_SEP, session->user, YMSG9_SEP,
		"1", YMSG9_SEP, session->user, YMSG9_SEP,
		"6", YMSG9_SEP, "abcde", YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );
	
	return( 0 );
}

int ymsg9_join( YMSG9_SESSION *session ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_JOIN );
	int len;

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
		"1", YMSG9_SEP, session->user, YMSG9_SEP,
		"62", YMSG9_SEP, "2", YMSG9_SEP,
		"104", YMSG9_SEP, session->room, YMSG9_SEP,
		"129", YMSG9_SEP, "0", YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );
	
	return( 0 );
}

int ymsg9_comment( YMSG9_SESSION *session, char *text ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_COMMENT );
	int len;

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
		"1", YMSG9_SEP, session->user, YMSG9_SEP,
		"104", YMSG9_SEP, session->room, YMSG9_SEP,
		"117", YMSG9_SEP, text, YMSG9_SEP,
		"124", YMSG9_SEP, "1", YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );
	
	return( 0 );
}

int ymsg9_emote( YMSG9_SESSION *session, char *text ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_COMMENT );
	int len;

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
		"1", YMSG9_SEP, session->user, YMSG9_SEP,
		"104", YMSG9_SEP, session->room, YMSG9_SEP,
		"117", YMSG9_SEP, text, YMSG9_SEP,
		"124", YMSG9_SEP, "2", YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );
	
	return( 0 );
}

int ymsg9_think( YMSG9_SESSION *session, char *text ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_COMMENT );
	int len;

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s. o O ( %s )%s%s%s%s%s",
		"1", YMSG9_SEP, session->user, YMSG9_SEP,
		"104", YMSG9_SEP, session->room, YMSG9_SEP,
		"117", YMSG9_SEP, text, YMSG9_SEP,
		"124", YMSG9_SEP, "3", YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );
	
	return( 0 );
}

int ymsg9_logout( YMSG9_SESSION *session ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_LOGOUT );
	int len;

	sprintf( ptr, "%s%s%s%s",
		"1", YMSG9_SEP, session->user, YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );

	return( 0 );
}

int ymsg9_ping( YMSG9_SESSION *session ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_PING );
	int len;

	sprintf( ptr, "%s%s%s%s",
		"109", YMSG9_SEP, session->user, YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );

	return( 0 );
}

/* static char *away_msgs[] =  */
/*   { */
/*     "Here", */
/*     "Be Right Back", */
/*     "Busy", */
/*     "Not At Home", */
/*     "Not At My Desk", */
/*     "Not In The Office", */
/*     "On The Phone", */
/*     "On Vacation", */
/*     "Out To Lunch", */
/*     "Stepped Out", */
/*     "", */
/*     "Auto-Away", */
/*     "Invisible", */
/*     NULL */
/*   }; */

int ymsg9_away( YMSG9_SESSION *session, char *msg ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_AWAY );
	int len;
	char *mptr;
	char tmp[256];

	strcpy( tmp, msg );
	mptr = strchr( tmp, ':' );
	*mptr = '\0';
	mptr++;

	if ( ! strcmp( tmp, "10" )) {
		strcpy( tmp, "99" );
	}

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s",
		"10", YMSG9_SEP, tmp, YMSG9_SEP,
		"19", YMSG9_SEP, mptr, YMSG9_SEP,
		"47", YMSG9_SEP, "0"
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );

	return( 0 );
}

int ymsg9_back( YMSG9_SESSION *session ) {
	u_char buf[4096];
	int len;

	ymsg9_header( session, buf, YMSG9_BACK );
	len = 20;

	ymsg9_send_packet( session, buf, len );

	return( 0 );
}

int ymsg9_pm( YMSG9_SESSION *session, char *remote_user, char *msg ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_PM );
	int len;

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
		"1", YMSG9_SEP, session->user, YMSG9_SEP,
		"5", YMSG9_SEP, remote_user, YMSG9_SEP,
		"14", YMSG9_SEP, msg, YMSG9_SEP,
		"97", YMSG9_SEP, "1", YMSG9_SEP,
		"63", YMSG9_SEP, ";0", YMSG9_SEP,
		"64", YMSG9_SEP, "0", YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );

	return( 0 );
}

int ymsg9_add_buddy( YMSG9_SESSION *session, char *friend ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_ADD_BUDDY );
	int len;

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s",
		"1", YMSG9_SEP, session->user, YMSG9_SEP,
		"7", YMSG9_SEP, friend, YMSG9_SEP,
		"65", YMSG9_SEP, "Friends", YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );

	return( 0 );
}

int ymsg9_remove_buddy( YMSG9_SESSION *session, char *friend ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_REM_BUDDY );
	int len;

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s",
		"1", YMSG9_SEP, session->user, YMSG9_SEP,
		"7", YMSG9_SEP, friend, YMSG9_SEP,
		"65", YMSG9_SEP, "Friends", YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );

	return( 0 );
}

int ymsg9_goto( YMSG9_SESSION *session, char *friend ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_GOTO );
	int len;

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s",
		"109", YMSG9_SEP, friend, YMSG9_SEP,
		"1", YMSG9_SEP, session->user, YMSG9_SEP,
		"62", YMSG9_SEP, "2", YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );

	return( 0 );
}

int ymsg9_invite( YMSG9_SESSION *session, char *remote_user, char *room ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_INVITE );
	int len;

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
		"1", YMSG9_SEP, session->user, YMSG9_SEP,
		"118", YMSG9_SEP, remote_user, YMSG9_SEP,
		"104", YMSG9_SEP, room, YMSG9_SEP,
		"117", YMSG9_SEP, "", YMSG9_SEP,
		"129", YMSG9_SEP, "0", YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );

	return( 0 );
}

int ymsg9_typing( YMSG9_SESSION *session, char *remote_user, int typing ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_NOTIFY );
	int len;

	/* set the typing status field */
	*((u_long *)(buf+12)) = htonl( 0x16 );

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
		"1", YMSG9_SEP, session->user, YMSG9_SEP,
		"4", YMSG9_SEP, session->user, YMSG9_SEP,
		"5", YMSG9_SEP, remote_user, YMSG9_SEP,
		"13", YMSG9_SEP, typing ? "1" : "0", YMSG9_SEP,
		"14", YMSG9_SEP, " ", YMSG9_SEP,
		"49", YMSG9_SEP, "TYPING", YMSG9_SEP,
		"1002", YMSG9_SEP, "1", YMSG9_SEP
		);

	len = 20 + strlen( ptr );

	ymsg9_send_packet( session, buf, len );

	return( 0 );
}

int ymsg9_send_file( YMSG9_SESSION *session, char *remote_user, char *msg,
		char *filename ) {
	u_char buf[4096];
	u_char *ptr = ymsg9_header( session, buf, YMSG9_FILETRANSFER );
	int len;
	int sock;
	struct sockaddr_in sa;
	struct hostent *hinfo;
	char url[1025];
	struct stat sbuf;
	int fd;
	int bytes;

	/*****************************************/
	/* FIXME: this function needs major work */
	/*****************************************/

	if ( stat( filename, &sbuf )) {
		/* file doesn't exist so exit */
fprintf( stderr, "File '%s' doesn't exist\n", filename ); fflush( stderr );
		return( 0 );
	}

	fd = open( filename, O_RDONLY, 0600 );
	if ( fd == -1 ) {
		/* can't open file */
fprintf( stderr, "Can't open File '%s'\n", filename ); fflush( stderr );
		return( 0 );
	}

	sprintf( ptr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%d%s%s%s",
		"0", YMSG9_SEP, session->user, YMSG9_SEP,
		"5", YMSG9_SEP, remote_user, YMSG9_SEP,
		"14", YMSG9_SEP, msg, YMSG9_SEP,
		"27", YMSG9_SEP, filename, YMSG9_SEP,
		"28", YMSG9_SEP, (int)sbuf.st_size, YMSG9_SEP,
		"29", YMSG9_SEP
		);

	len = 20 + strlen( ptr ) + sbuf.st_size;

	*((u_short *)(buf+8)) = htons( len - 20 );

	hinfo = gethostbyname( "filetransfer.msg.yahoo.com" );
	memset( &sa, 0, sizeof(sa));
	memmove((void*)&sa.sin_addr.s_addr, hinfo->h_addr, hinfo->h_length );

	sa.sin_family = AF_INET;
	sa.sin_port = htons( 80 );

	sock = socket( AF_INET, SOCK_STREAM, 6 );
	connect( sock, (struct sockaddr*)&sa, sizeof(sa));

	snprintf( url, 1024,
		"POST /notifyt HTTP/1.0\r\n"
		"Content-length: %d\n"
		"Accept: text/html\r\n"
		"Accept: text/plain\r\n"
		"User-Agent: Gyach/%s\r\n"
		"Host: %s\r\n",
		len,
		"v0.8.3", "filetransfer.msg.yahoo.com" );

	strcat( url, "Cookie: " );
	strcat( url, session->cookie );
	strcat( url, "\r\n" );
	strcat( url, "\r\n" );

	write( sock, url, strlen( url ));

	write( sock, buf, 20 + strlen( ptr ));

	/* now read the file in and send it */
	bytes = read( fd, buf, 512 );
	while( bytes ) {
		write( sock, buf, bytes );

		bytes = read( fd, buf, 512 );
	}

	close( fd );
	close( sock );

	return( 0 );
}



