/* yahoochat.h -- ymsg9 protocol
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
 * Released under the terms of the GPL.
 * *NO WARRANTY*
 *
 * cpinkham@infi.net, cpinkham@bc2va.org
 * http://www4.infi.net/~cpinkham/gyach/
 */

#ifndef YAHOOCHAT_H
#define YAHOOCHAT_H

#include <sys/types.h>

#define YMSG9_USER_SIZE		64
#define YMSG9_PASSWORD_SIZE	32
#define YMSG9_ROOM_SIZE		64
#define YMSG9_HOST_SIZE		128
#define YMSG9_COOKIE_SIZE	1024
#define YMSG9_DATA_SIZE		2048

typedef struct YMSG9_PACKET_STRUCT 
{
  int type;                       /* packet type */
  int size;                       /* data length */
  char data[YMSG9_DATA_SIZE + 1]; /* packet data */
} YMSG9_PACKET;

typedef struct YMSG9_SESSION_STRUCT {
  int port; /* port to connect to */
  int sock; /* network socket */
  int quit; /* true if should quit */
  char user[YMSG9_USER_SIZE + 1];         /* login */
  char password[YMSG9_PASSWORD_SIZE + 1]; /* password */
  char room[YMSG9_ROOM_SIZE + 1];         /* current room */
  char host[YMSG9_HOST_SIZE + 1];         /* hostname to connect to */
  char proxy_host[YMSG9_HOST_SIZE + 1];   /* hostname to connect to */
  int proxy_port;                         /* port to connect to */
  char cookie[YMSG9_COOKIE_SIZE + 1];     /* cookie(s) */
  int session_id;        /* session id */
  int io_callback_tag;   /* input callback tag */
  int ping_callback_tag; /* ping callback tag */
  char *error_msg;       /* error message if needed */
  int debug_packets;     /* debug in/out packets */
  YMSG9_PACKET pkt;      /* data packet */
} YMSG9_SESSION;

#define YMSG9_CHAT_PORT  5050

#define YMSG9_SEP        "\xC0\x80"

#define YMSG9_BUDDY_ON          0x01
#define YMSG9_BUDDY_OFF         0x02
#define YMSG9_AWAY              0x03
#define YMSG9_BACK              0x04
#define YMSG9_PM                0x06
#define YMSG9_MAIL              0x0b
#define YMSG9_NEW_CONTACT       0x0f
#define YMSG9_PM_RECV           0x20
#define YMSG9_FILETRANSFER      0x46
#define YMSG9_NOTIFY            0x4b
#define YMSG9_VERIFY            0x4c
#define YMSG9_LOGIN             0x54
#define YMSG9_COOKIE            0x55
#define YMSG9_GET_KEY           0x57
#define YMSG9_ADD_BUDDY         0x83
#define YMSG9_REM_BUDDY         0x84
#define YMSG9_ONLINE            0x96
#define YMSG9_GOTO              0x97
#define YMSG9_JOIN              0x98
#define YMSG9_EXIT              0x9b
#define YMSG9_INVITE            0x9d
#define YMSG9_LOGOUT            0xa0
#define YMSG9_PING              0xa1
#define YMSG9_COMMENT           0xa8

extern int ymsg9_open_socket(YMSG9_SESSION *session);
extern int ymsg9_recv_data(YMSG9_SESSION *session);
extern int ymsg9_request_key(YMSG9_SESSION *session);
extern int ymsg9_login(YMSG9_SESSION *session, char *key);
extern int ymsg9_online(YMSG9_SESSION *session);
extern int ymsg9_join(YMSG9_SESSION *session);
extern int ymsg9_comment(YMSG9_SESSION *session, char *text);
extern int ymsg9_emote(YMSG9_SESSION *session, char *text);
extern int ymsg9_think(YMSG9_SESSION *session, char *text);
extern int ymsg9_logout(YMSG9_SESSION *session);
extern int ymsg9_ping(YMSG9_SESSION *session);
extern int ymsg9_away(YMSG9_SESSION *session, char *msg);
extern int ymsg9_back(YMSG9_SESSION *session);
extern int ymsg9_pm(YMSG9_SESSION *session, char *remote_user, char *msg);
extern int ymsg9_add_buddy(YMSG9_SESSION *session, char *friend);
extern int ymsg9_remove_buddy(YMSG9_SESSION *session, char *friend);
extern int ymsg9_goto(YMSG9_SESSION *session, char *friend);
extern int ymsg9_invite(YMSG9_SESSION *session, char *remote_user, char *room);
extern int ymsg9_typing(YMSG9_SESSION *session, char *remote_user, int typing);
extern int ymsg9_send_file(YMSG9_SESSION *session, char *who, char *msg, char *filename);

#endif
