/* -*- mode: c; mode: folding; eval: (c-set-offset 'arglist-cont-nonempty '+) -*-
 *
 * Yahoo! Chat shell based on the sampple client distrubuted with libyahoo2
 *
 * Copyright (C) 2002-2004, Philip S Tellis <philip.tellis AT gmx.net>
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
#include <netdb.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <termios.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <time.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#include <yahoo2.h>
#include <yahoo2_callbacks.h>
#include <glib.h>

#include "commands.h"
#include "yahoo_util.h"
#include "base64.h"
#include "misc.h"
#include "globals.h"
#include "config_api.h"

#define PING_TIMEOUT_INTERVAL 600
#define MAX_PREF_LEN 255

#define INPUT_LINE_LENGTH 2048

static char *local_host = NULL;

static int do_mail_notify = 0;
static int do_yahoo_debug = 0;
static int ignore_system = 0;
static int do_typing_notify = 1;
static int accept_webcam_viewers = 1;
static int send_webcam_images = 0;
static int webcam_direction = YAHOO_WEBCAM_DOWNLOAD;
static time_t curTime = 0;
static time_t pingTimer = 0;
static time_t webcamTimer = 0;
static double webcamStart = 0;

/* id of the webcam connection (needed for uploading) */
static int webcam_id = 0;

static int POLL_LOOP = 1;

static void register_callbacks();

typedef struct {
	char yahoo_id[255];
	char name[255];
	int status;
	int away;
	char *msg;
	char group[255];
} yahoo_account;

typedef struct {
	int id;
	char *label;
} yahoo_idlabel;

typedef struct {
	int id;
	char *who;
} yahoo_authorize_data;

yahoo_idlabel yahoo_status_codes[] = {
	{YAHOO_STATUS_AVAILABLE, "Available"},
	{YAHOO_STATUS_BRB, "BRB"},
	{YAHOO_STATUS_BUSY, "Busy"},
	{YAHOO_STATUS_NOTATHOME, "Not Home"},
	{YAHOO_STATUS_NOTATDESK, "Not at Desk"},
	{YAHOO_STATUS_NOTINOFFICE, "Not in Office"},
	{YAHOO_STATUS_ONPHONE, "On Phone"},
	{YAHOO_STATUS_ONVACATION, "On Vacation"},
	{YAHOO_STATUS_OUTTOLUNCH, "Out to Lunch"},
	{YAHOO_STATUS_STEPPEDOUT, "Stepped Out"},
	{YAHOO_STATUS_INVISIBLE, "Invisible"},
	{YAHOO_STATUS_IDLE, "Idle"},
	{YAHOO_STATUS_OFFLINE, "Offline"},
	{YAHOO_STATUS_CUSTOM, "[Custom]"},
	{YAHOO_STATUS_NOTIFY, "Notify"},
	{0, NULL}
};

char * yahoo_status_code(enum yahoo_status s)
{
	int i;
	for(i=0; yahoo_status_codes[i].label; i++)
		if(yahoo_status_codes[i].id == s)
			return yahoo_status_codes[i].label;
	return "Unknown";
}

#define YAHOO_DEBUGLOG ext_yahoo_log

int ext_yahoo_log(char *fmt,...)
{
	va_list ap;

	va_start(ap, fmt);

	vfprintf(stderr, fmt, ap);
	fflush(stderr);
	va_end(ap);
	return 0;
}

#define LOG(x) if(do_yahoo_debug) { YAHOO_DEBUGLOG("%s:%d: ", __FILE__, __LINE__); \
	YAHOO_DEBUGLOG x; \
	YAHOO_DEBUGLOG("\n"); }

#define WARNING(x) if(do_yahoo_debug) { YAHOO_DEBUGLOG("%s:%d: warning: ", __FILE__, __LINE__); \
	YAHOO_DEBUGLOG x; \
	YAHOO_DEBUGLOG("\n"); }

void
print_message0(char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  vfprintf(stdout, fmt, ap);
  va_end(ap);

  fflush(stdout);
}

void
print_message(char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  vfprintf(stdout, fmt, ap);
  va_end(ap);

  putc('\n', stdout);
  fflush(stdout);
}

static YList * buddies = NULL;

static char * 
get_buddy_name(char * yid)
{
  YList * b;
  for (b = buddies; b; b = b->next) {
    yahoo_account * ya = b->data;
    if(!strcmp(yid, ya->yahoo_id))
      return ya->name&&*ya->name?ya->name:ya->yahoo_id;
  }

  return yid;
}

/* {{{ Timeout */

static 
int expired(time_t timer)
{
  if (timer && curTime >= timer)
    return 1;

  return 0;
}

static void 
rearm(time_t *timer, int seconds)
{
  time(timer);
  *timer += seconds;
}


static double 
get_time()
{
  struct timeval ct;
  gettimeofday(&ct, 0);

  return (ct.tv_sec * 1E3 + ct.tv_usec / 1E3); /* return time in milliseconds */
}

static int 
yahoo_ping_timeout_callback()
{
  print_message("(message :type ping-timeout)");
  yahoo_keepalive(ylad->id);
  rearm(&pingTimer, PING_TIMEOUT_INTERVAL);

  return 1;
}

/* }}} */
/* {{{ Webcam */

static int 
yahoo_webcam_timeout_callback(int id)
{
	static unsigned image_num = 0;
	unsigned char *image = NULL;
	unsigned int length = 0;
	unsigned int timestamp = get_time() - webcamStart;
	char fname[1024];
	FILE *f_image = NULL;
	struct stat s_image;

	if (send_webcam_images)
	{
		sprintf(fname, "images/image_%.3d.jpc", image_num++);
		if (image_num > 999) image_num = 0;
		if (stat(fname, &s_image) == -1)
			return -1;
		length = s_image.st_size;
		image = y_new0(unsigned char, length);

		if ((f_image = fopen(fname, "r")) != NULL) {
			fread(image, length, 1, f_image);
			fclose(f_image);
		} else {
			printf("Error reading from %s\n", fname);
		}
	}

	print_message("Sending a webcam image (%d bytes)", length);
	yahoo_webcam_send_image(id, image, length, timestamp);
	FREE(image);
	rearm(&webcamTimer, 2);
	return 1;
}

void ext_yahoo_got_webcam_image(int id, const char *who,
		const unsigned char *image, unsigned int image_size, unsigned int real_size,
		unsigned int timestamp)
{
	static unsigned char *cur_image = NULL;
	static unsigned int cur_image_len = 0;
	static unsigned int image_num = 0;
	FILE* f_image;
	char fname[1024];

	/* copy image part to cur_image */
	if (real_size)
	{
		if (!cur_image) cur_image = y_new0(unsigned char, image_size);
		memcpy(cur_image + cur_image_len, image, real_size);
		cur_image_len += real_size;
	}

	if (image_size == cur_image_len)
	{
	  print_message("Received a image update at %d (%d bytes)", timestamp, image_size);

		/* if we recieved an image then write it to file */
		if (image_size)
		{
			sprintf(fname, "images/%s_%.3d.jpc", who, image_num++);

			if ((f_image = fopen(fname, "w")) != NULL) {
				fwrite(cur_image, image_size, 1, f_image);
				fclose(f_image);
			} else {
				printf("Error writing to %s\n", fname);
			}
			FREE(cur_image);
			cur_image_len = 0;
			if (image_num > 999) image_num = 0;
		}
	}
}

void ext_yahoo_webcam_viewer(int id, char *who, int connect)
{
	switch (connect)
	{
		case 0:
			print_message("%s has stopped viewing your webcam", who);
			break;
		case 1:
			print_message("%s has started viewing your webcam", who);
			break;
		case 2:
			print_message("%s is trying to view your webcam", who);
			yahoo_webcam_accept_viewer(id, who, accept_webcam_viewers);
			break;
	}
}

void ext_yahoo_webcam_closed(int id, char *who, int reason)
{
	switch(reason)
	{
		case 1:
			print_message("%s stopped broadcasting", who);
			break;
		case 2:
			print_message("%s cancelled viewing permission", who);
			break;
		case 3:
			print_message("%s declines permission to view his/her webcam", who);
			break;
		case 4:
			print_message("%s does not have his/her webcam online", who);
			break;
	}
}

void ext_yahoo_webcam_data_request(int id, int send)
{
	webcam_id = id;

	if (send) {
		print_message("Got request to start sending images");
		if (!webcamTimer)
			rearm(&webcamTimer, 2);
	} else {
		print_message("Got request to stop sending images");
	}
	send_webcam_images = send;
}

void ext_yahoo_webcam_invite(int id, char *from)
{
	print_message("Got a webcam invitation from %s", from);
}

void ext_yahoo_webcam_invite_reply(int id, char *from, int accept)
{
	if(accept) {
		print_message("%s accepted webcam invitation...", from);
	} else {
		print_message("%s declined webcam invitation...", from);
	}
}

/* }}} */
/* {{{ Conferencing */

YList * conferences = NULL;
typedef struct {
	int id;
	char * room_name;
	char * host;
	YList * members;
	int joined;
} conf_room;

static conf_room * find_conf_room_by_name_and_id(int id, const char * name)
{
	YList * l;
	for(l = conferences; l; l=l->next) {
		conf_room * cr = l->data;
		if(cr->id == id && !strcmp(name, cr->room_name)) {
			return cr;
		}
	}

	return NULL;
}

void ext_yahoo_got_conf_invite(int id, char *who, char *room, char *msg, YList *members)
{
	conf_room * cr = y_new0(conf_room, 1);
	cr->room_name = strdup(room);
	cr->host = strdup(who);
	cr->members = members;
	cr->id = id;

	conferences = y_list_append(conferences, cr);

	print_message(("%s has invited you to a conference: %s\n"
				"with the message: %s",
				who, room, msg));

	for(; members; members=members->next)
		print_message("\t%s", (char *)members->data);
	
}
void ext_yahoo_conf_userdecline(int id, char *who, char *room, char *msg)
{
	YList * l;
	conf_room * cr = find_conf_room_by_name_and_id(id, room);
	
	if(cr)
	for(l = cr->members; l; l=l->next) {
		char * w = l->data;
		if(!strcmp(w, who)) {
			FREE(l->data);
			cr->members = y_list_remove_link(cr->members, l);
			y_list_free_1(l);
			break;
		}
	}

	print_message(("%s declined the invitation to %s\n"
				"with the message: %s", who, room, msg));
}
void ext_yahoo_conf_userjoin(int id, char *who, char *room)
{
	conf_room * cr = find_conf_room_by_name_and_id(id, room);
	if(cr) {
	YList * l = NULL;
	for(l = cr->members; l; l=l->next) {
		char * w = l->data;
		if(!strcmp(w, who))
			break;
	}
	if(!l)
		cr->members = y_list_append(cr->members, strdup(who));
	}

	print_message("%s joined the conference %s", who, room);

}
void ext_yahoo_conf_userleave(int id, char *who, char *room)
{
	YList * l;
	conf_room * cr = find_conf_room_by_name_and_id(id, room);
	
	if(cr)
	for(l = cr->members; l; l=l->next) {
		char * w = l->data;
		if(!strcmp(w, who)) {
			FREE(l->data);
			cr->members = y_list_remove_link(cr->members, l);
			y_list_free_1(l);
			break;
		}
	}

	print_message("%s left the conference %s", who, room);
}
void ext_yahoo_conf_message(int id, char *who, char *room, char *msg, int utf8)
{
	char * umsg = msg;

	if(utf8)
		umsg = y_utf8_to_str(msg);

	who = get_buddy_name(who);

	print_message("%s (in %s): %s", who, room, umsg);

	if(utf8)
		FREE(umsg);
}

/* }}} */

void 
ext_yahoo_chat_cat_xml(int id, char *xml) 
{
}


void 
ext_yahoo_chat_join(int id, char *room, char *topic, YList *members, int fd)
{
  room = b64_encode_string(room);
  topic = b64_encode_string(topic);
  print_message("(message :type herald :room \"%s\" :topic \"%s\")", room, topic);
  g_free(room);
  g_free(topic);

  while (members) 
    {
      YList *n = members->next;
      ext_yahoo_chat_userjoin(id, room, members->data);
      g_free(members);
      members = n;
    }
}

void
ext_yahoo_chat_userjoin(int id, char *room, struct yahoo_chat_member *who)
{
  gchar *sex = NULL;
  
  if (who->attribs & YAHOO_CHAT_MALE)
    sex = "male";
  else if (who->attribs & YAHOO_CHAT_FEMALE)
    sex = "female";
  else 
    sex = "unknown";

  gchar *b64_room = b64_encode_string(room);
  gchar *b64_id = b64_encode_string(who->id);
  print_message0("(message :type join :room \"%s\" :id \"%s\" ", b64_room, b64_id);
  g_free(b64_room);
  g_free(b64_id);

  if (who->alias) 
    {
      gchar *alias = b64_encode_string(who->alias);
      print_message0(":alias \"%s\" ", alias);
      g_free(alias);
    }
  else 
    print_message0(":alias nil ");
  if (who->age)
    print_message0(":age %d ", who->age);
  else 
    print_message0(":age nil ");
  print_message0(":sex %s ", sex);
  if (who->location)
    {
      gchar *location = b64_encode_string(who->location);
      print_message0(":location \"%s\" ", location);
      g_free(location);
    }
  else 
    print_message0(":location nil ");

  print_message(":webcam %s)", (who->attribs & YAHOO_CHAT_WEBCAM) ? "t" : "nil");

  g_free(who->id); who->id = NULL;
  g_free(who->alias); who->alias = NULL;
  g_free(who->location); who->location = NULL;
}

void
ext_yahoo_chat_userleave(int id, char *room, char *who)
{
  who = b64_encode_string(who);
  room = b64_encode_string(room);
  print_message("(message :type leave :room \"%s\" :who \"%s\")", room, who);
  g_free(who);
  g_free(room);
}

void 
ext_yahoo_chat_message(int id, char *who, char *room, char *message, int msgtype, int utf8)
{
  gchar *umessage = NULL;

  if (utf8)
    umessage = y_utf8_to_str(message);
  else 
    umessage = g_strdup(message);

  message = b64_encode_string(umessage);
  room = b64_encode_string(room);
  who = b64_encode_string(who);

  if (msgtype == 2)
    print_message("(message :type emote :room \"%s\" :who \"%s\" :message \"%s\")", 
      room, who, message);
  else
    print_message("(message :type plain :room \"%s\" :who \"%s\" :message \"%s\")", 
      room, who, message);

  g_free(umessage);
  g_free(message);
  g_free(room);
  g_free(who);
}

void
ext_yahoo_mail_notify(int id, char *from, char *subject, int count)
{
  if(! do_mail_notify)
    return;

  if (from && subject)
    {
      from = b64_encode_string(from);
      subject = b64_encode_string(subject);
      print_message("(message :type mail :from \"%s\" :subject \"%s\")", from, subject);
      g_free(from);
      g_free(subject);
    }
  if (count)
    print_message("(message :type mail-count :count %d)", count);
}

void
ext_yahoo_got_im(int id, char *who, char *msg, long tm, int stat, int utf8)
{
  gchar *umsg;
  glong umsg_len;
  gchar *b64_who;
  
  if (stat == 2) 
    {
      b64_who = b64_encode_string(who);
      print_message("(message :type im-error :who \"%s\")", b64_who);
      g_free(b64_who);
      return;
    }
  if(! msg)
    return;
  if (utf8)
    umsg = y_utf8_to_str(msg);
  gchar *b64_umsg = b64_encode_string(umsg);
  who = get_buddy_name(who);
  if (tm) 
    {
      char timestr[255];
      strncpy(timestr, ctime((time_t *) &tm), sizeof(timestr));
      timestr[strlen(timestr) - 1] = '\0';
      gchar *b64_timestr = b64_encode_string(timestr);
      b64_who = b64_encode_string(who);
      print_message("(message :type im-offline :time \"%s\" :who \"%s\" :message \"%s\")",
        b64_timestr, b64_who, b64_umsg);
      g_free(b64_timestr);
      g_free(b64_who);
    } 
  else
    {
      b64_who = b64_encode_string(who);
      if(! strcmp(umsg, "<ding>"))
        print_message("(message :type im-bell :who \"%s\")", b64_who);
      print_message("(message :type im :who \"%s\" :message \"%s\")", b64_who, b64_umsg);
      g_free(b64_who);
    }
  if (utf8)
    g_free(umsg);

  g_free(b64_umsg);
}

void
ext_yahoo_rejected(int id, char *who, char *message)
{
  who = b64_encode_string(who);
  
  if (! message)
    {
      message = b64_encode_string(message);
      print_message("(message :type reject :who \"%s\" :message \"%s\")", who, message);
      g_free(message);
    }
  else
    print_message("(message :type reject :who \"%s\" :message nil)", who);

  g_free(who);
}

void 
ext_yahoo_system_message(int id, char *msg)
{
  msg = b64_encode_string(msg);
  print_message("(message :type system :message \"%s\")", msg);
  g_free(msg);
}

void
ext_yahoo_status_changed(int id, char *who, int stat, char *msg, int away)
{
  yahoo_account *ya = NULL;
  YList * b;


  for(b = buddies; b; b = b->next) 
    {
      if(! strcmp(((yahoo_account *)b->data)->yahoo_id, who)) 
	{
	  ya = b->data;
	  break;
	}
    }

  gchar *name;

  if (ya)
    name = b64_encode_string(ya->name);
  who = b64_encode_string(who);

  if (msg)
    {
      gchar *message = b64_encode_string(msg);
      print_message("(message :type status :name \"%s\" :who \"%s\" :message \"%s\")", 
	ya ? name : who, who, message);
      g_free(message);
    }
  else if (stat == YAHOO_STATUS_IDLE)
    print_message("(message :type status-idle :name \"%s\" :who \"%s\" :time \"%d:%02d:%02d\")", 
      ya ? name : who, who, away / 3600, (away / 60) % 60, away % 60);
  else
    print_message("(message :type status-change :name \"%s\" :who \"%s\" :message \"%s\")", 
      ya ? name : who, who, yahoo_status_code(stat));

  g_free(who);

  if (ya)
    g_free(name);

  if (ya)
    {
      ya->status = stat;
      ya->away = away;
      if (msg) 
	{
	  g_free(ya->msg);
	  ya->msg = strdup(msg);
	}
    }
}

void ext_yahoo_got_buddies(int id, YList * buds)
{
/* 	while(buddies) { */
/* 		FREE(buddies->data); */
/* 		buddies = buddies->next; */
/* 		if(buddies) */
/* 			FREE(buddies->prev); */
/* 	} */
/* 	for(; buds; buds = buds->next) { */
/* 		yahoo_account *ya = y_new0(yahoo_account, 1); */
/* 		struct yahoo_buddy *bud = buds->data; */
/* 		strncpy(ya->yahoo_id, bud->id, 255); */
/* 		if(bud->real_name) */
/* 			strncpy(ya->name, bud->real_name, 255); */
/* 		strncpy(ya->group, bud->group, 255); */
/* 		ya->status = YAHOO_STATUS_OFFLINE; */
/* 		buddies = y_list_append(buddies, ya); */

/* /\*		print_message("%s is %s", bud->id, bud->real_name);*\/ */
/* 	} */
}

void 
ext_yahoo_got_ignore(int id, YList * igns)
{
}

void 
ext_yahoo_contact_added(int id, char *myid, char *who, char *message)
{
  who = b64_encode_string(who);
  if (message)
    {
      message = b64_encode_string(message);
      print_message("(message :type buddy-add :who \"%s\" :message \"%s\")", who, message);
      g_free(message);
    }
  else       
    print_message("(message :type buddy-add :who \"%s\" :message nil)", who);
  g_free(who);
}

void 
ext_yahoo_typing_notify(int id, char *who, int stat)
{
  if (stat && do_typing_notify)
    {
      who = b64_encode_string(who);
      print_message("(message :typing :who \"%s\")", who);
      g_free(who);
    }
}

void ext_yahoo_game_notify(int id, char *who, int stat)
{
}


void yahoo_logout()
{
	if (ylad->id <= 0) {
		return;
	}

	pingTimer=0;

	while(conferences) {
		YList * n = conferences->next;
		conf_room * cr = conferences->data;
		if(cr->joined)
			yahoo_conference_logoff(ylad->id, NULL, cr->members, cr->room_name);
		FREE(cr->room_name);
		FREE(cr->host);
		while(cr->members) {
			YList *n = cr->members->next;
			FREE(cr->members->data);
			FREE(cr->members);
			cr->members=n;
		}
		FREE(cr);
		FREE(conferences);
		conferences = n;
	}
	
	yahoo_logoff(ylad->id);
	yahoo_close(ylad->id);

	ylad->status = YAHOO_STATUS_OFFLINE;
	ylad->id = 0;

	POLL_LOOP=0;

	print_message("logged_out");
}

void ext_yahoo_login(yahoo_local_account * ylad, int login_mode)
{
	LOG(("ext_yahoo_login"));

	ylad->id = yahoo_init_with_attributes(ylad->yahoo_id, ylad->password, 
			"local_host", local_host,
			"pager_port", 23,
			NULL);
	ylad->status = YAHOO_STATUS_OFFLINE;
	yahoo_login(ylad->id, login_mode);

/*	if (ylad->id <= 0) {
		print_message("Could not connect to Yahoo server.  Please verify that you are connected to the net and the pager host and port are correctly entered.");
		return;
	}
*/
	rearm(&pingTimer, 600);
}

void ext_yahoo_got_cookies(int id)
{
	/*yahoo_get_yab(id);*/
}

void 
ext_yahoo_login_response(int id, int succ, char *url)
{
  /* url is the URL provided by the server to re-activate a locked account */
  gchar *message = NULL, *subtype = NULL;
  gboolean terminate = TRUE;

  switch (succ) 
    {
    case YAHOO_LOGIN_OK:
      ylad->status = yahoo_current_status(id);
      message = b64_encode_string("Login OK");
      subtype = "login-ok";
      terminate = FALSE;
      break;
    case YAHOO_LOGIN_UNAME:
      message = b64_encode_string("Invalid username");
      subtype = "invalid-username";
      break;
    case YAHOO_LOGIN_PASSWD:
      message = b64_encode_string("Invalid password");
      subtype = "invalid-password";
      break;
    case YAHOO_LOGIN_LOCK:
      message = b64_encode_string("Account locked");
      subtype = "account-locked";
      break;
    case YAHOO_LOGIN_DUPL:
      message = b64_encode_string("Duplicate login");
      subtype = "duplicate";
      break;
    case YAHOO_LOGIN_SOCK:
      message = b64_encode_string("Socket closed");
      subtype = "socket-closed";
      break;
    default:
      message = b64_encode_string("Unknown failure to login");
      subtype = "unknown";
      break;
    }

  print_message("(message :type login-response :sub-type %s :message \"%s\")", subtype, message);
  g_free(message);
  if (terminate)
    {
      ylad->status = YAHOO_STATUS_OFFLINE;
      yahoo_logout();
      POLL_LOOP = 0;
    }
}

void ext_yahoo_error(int id, char *err, int fatal)
{
	fprintf(stdout, "Yahoo Error: ");
	fprintf(stdout, "%s", err);
	/*
	switch(num) {
		case E_CONFNOTAVAIL:
			fprintf(stdout, "%s is not available for the conference", err);
			break;
		case E_IGNOREDUP:
			fprintf(stdout, "%s is already ignored", err);
			break;
		case E_IGNORENONE:
			fprintf(stdout, "%s is not in the ignore list", err);
			break;
		case E_IGNORECONF:
			fprintf(stdout, "%s is in buddy list - cannot ignore ", err);
			break;
	}
	*/
	fprintf(stdout, "\n");
	if(fatal)
		yahoo_logout();
}

void yahoo_set_current_state(int yahoo_state)
{
	if (ylad->status == YAHOO_STATUS_OFFLINE && yahoo_state != YAHOO_STATUS_OFFLINE) {
		ext_yahoo_login(ylad, yahoo_state);
		return;
	} else if (ylad->status != YAHOO_STATUS_OFFLINE && yahoo_state == YAHOO_STATUS_OFFLINE) {
		yahoo_logout();
		return;
	}

	ylad->status = yahoo_state;
	if(yahoo_state == YAHOO_STATUS_CUSTOM) {
		if(ylad->msg)
			yahoo_set_away(ylad->id, yahoo_state, ylad->msg, 1);
		else
			yahoo_set_away(ylad->id, yahoo_state, "delta p * delta x too large", 1);
	} else
		yahoo_set_away(ylad->id, yahoo_state, NULL, 1);
}

int ext_yahoo_connect(char *host, int port)
{
	struct sockaddr_in serv_addr;
	static struct hostent *server;
	static char last_host[256];
	int servfd;
	char **p;

	if(last_host[0] || strcasecmp(last_host, host)!=0) {
		if(!(server = gethostbyname(host))) {
			WARNING(("failed to look up server (%s:%d)\n%d: %s", 
						host, port,
						h_errno, strerror(h_errno)));
			return -1;
		}
		strncpy(last_host, host, 255);
	}

	if((servfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		WARNING(("Socket create error (%d): %s", errno, strerror(errno)));
		return -1;
	}

	LOG(("connecting to %s:%d", host, port));

	for (p = server->h_addr_list; *p; p++)
	{
		memset(&serv_addr, 0, sizeof(serv_addr));
		serv_addr.sin_family = AF_INET;
		memcpy(&serv_addr.sin_addr.s_addr, *p, server->h_length);
		serv_addr.sin_port = htons(port);

		LOG(("trying %s", inet_ntoa(serv_addr.sin_addr)));
		if(connect(servfd, (struct sockaddr *) &serv_addr, 
					sizeof(serv_addr)) == -1) {
			if(errno!=ECONNREFUSED && errno!=ETIMEDOUT && 
					errno!=ENETUNREACH) {
				break;
			}
		} else {
			LOG(("connected"));
			return servfd;
		}
	}

	WARNING(("Could not connect to %s:%d\n%d:%s", host, port, errno, 
				strerror(errno)));
	close(servfd);
	return -1;
}

/*************************************
 * Callback handling code starts here
 */
YList *CONNECTIONS = NULL;

struct _conn {
	int tag;
	int fd;
	int id;
	yahoo_input_condition cond;
	void *data;
	int remove;
};
static int connection_tags=0;

int ext_yahoo_add_handler(int id, int fd, yahoo_input_condition cond, void *data)
{
	struct _conn *c = y_new0(struct _conn, 1);
	c->tag = ++connection_tags;
	c->id = id;
	c->fd = fd;
	c->cond = cond;
	c->data = data;

	LOG(("Add %d for %d, tag %d", fd, id, c->tag));

	CONNECTIONS = y_list_prepend(CONNECTIONS, c);

	return c->tag;
}

void ext_yahoo_remove_handler(int id, int tag)
{
	YList *l;
	for(l = CONNECTIONS; l; l = y_list_next(l)) {
		struct _conn *c = l->data;
		if(c->tag == tag) {
			/* don't actually remove it, just mark it for removal */
			/* we'll remove when we start the next poll cycle */
			LOG(("Marking id:%d fd:%d tag:%d for removal", c->id, c->fd, c->tag));
			c->remove = 1;
			return;
		}
	}
}

struct connect_callback_data {
	yahoo_connect_callback callback;
	void * callback_data;
	int id;
	int tag;
};

static void connect_complete(void *data, int source, yahoo_input_condition condition)
{
	struct connect_callback_data *ccd = data;
	int error, err_size = sizeof(error);

	ext_yahoo_remove_handler(0, ccd->tag);
	getsockopt(source, SOL_SOCKET, SO_ERROR, &error, (socklen_t *)&err_size);

	if(error) {
		close(source);
		source = -1;
	}

	LOG(("Connected fd: %d, error: %d", source, error));

	ccd->callback(source, error, ccd->callback_data);
	FREE(ccd);
}

void yahoo_callback(struct _conn *c, yahoo_input_condition cond)
{
	int ret=1;
	char buff[1024]={0};

	if(c->id < 0) {
		connect_complete(c->data, c->fd, cond);
	} else {
		if(cond & YAHOO_INPUT_READ)
			ret = yahoo_read_ready(c->id, c->fd, c->data);
		if(ret>0 && cond & YAHOO_INPUT_WRITE)
			ret = yahoo_write_ready(c->id, c->fd, c->data);

		if(ret == -1)
			snprintf(buff, sizeof(buff), 
				"Yahoo read error (%d): %s", errno, strerror(errno));
		else if(ret == 0)
			snprintf(buff, sizeof(buff), 
				"Yahoo read error: Server closed socket");

		if(buff[0])
			print_message(buff);
	}
}

int ext_yahoo_connect_async(int id, char *host, int port, 
		yahoo_connect_callback callback, void *data)
{
	struct sockaddr_in serv_addr;
	static struct hostent *server;
	int servfd;
	struct connect_callback_data * ccd;
	int error;

	if(!(server = gethostbyname(host))) {
		errno=h_errno;
		return -1;
	}

	if((servfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		return -1;
	}

	memset(&serv_addr, 0, sizeof(serv_addr));
	serv_addr.sin_family = AF_INET;
	memcpy(&serv_addr.sin_addr.s_addr, *server->h_addr_list, server->h_length);
	serv_addr.sin_port = htons(port);

	error = connect(servfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr));

	LOG(("Trying to connect: fd:%d error:%d", servfd, error));
	if(!error) {
		callback(servfd, 0, data);
		return 0;
	} else if(error == -1 && errno == EINPROGRESS) {
		ccd = calloc(1, sizeof(struct connect_callback_data));
		ccd->callback = callback;
		ccd->callback_data = data;
		ccd->id = id;

		ccd->tag = ext_yahoo_add_handler(-1, servfd, YAHOO_INPUT_WRITE, ccd);
		return ccd->tag;
	} else {
		if(error == -1) {
			LOG(("%s", strerror(errno)));
		}
		close(servfd);
		return -1;
	}
}
/*
 * Callback handling code ends here
 ***********************************/

static void 
process_commands(const gchar *line)
{
  gchar **parsed_line = g_strsplit(line, " ", 0);
  gchar *command = parsed_line[0];
  gchar *b64_line = NULL;

  switch (command_handler(command, parsed_line)) 
    {
    case COMMAND_OK:
      break;
    case COMMAND_UNHANDLED:
      b64_line = b64_encode_string(line);
      print_message("(error :type command-invalid :command-line \"%s\")", b64_line);
      g_free(b64_line);
      break;
    case COMMAND_ERROR:
      b64_line = b64_encode_string(line);
      print_message("(error :type command-error :command-line \"%s\")", b64_line);
      g_free(b64_line);
      break;
    default:
      break;
    }
  
  g_strfreev(parsed_line);
}


static void 
local_input_callback(int source)
{
  char line[INPUT_LINE_LENGTH] = {0};
  int i;
  char c;
  i=0; c=0;
  do 
    {
      if (read(source, &c, 1) <= 0)
	c='\0';
      if (c == '\r')
	continue;
      if (c == '\n')
	break;
      if (c == '\b') 
	{
	  if (!i)
	    continue;
	  c = '\0';
	  i--;
	}
      if (c)
	{
	  line[i++] = c;
	  line[i]='\0';
	}
    } while (i < INPUT_LINE_LENGTH - 1 && c != '\n');

  if(line[0])
    process_commands(line);
}


/* #define DEFAULT_RC ".elgyachrc" */

/* gint */
/* init_config(const gchar *config_file, const gchar *profile) */
/* { */
/*   config_t config; */
/*   gint ret; */
/*   gchar *file; */
/*   const gchar *home = g_get_home_dir(); */

/*   if (! config_file && home) */
/*     file = g_strdup_printf("%s/%s", home, DEFAULT_RC); */
/*   else if (config_file) */
/*     file = g_strdup(config_file); */
/*   else  */
/*     return -1; */

/*   memset(ylad->yahoo_id, 0, sizeof(ylad->yahoo_id)); */
/*   memset(ylad->password, 0, sizeof(ylad->password)); */

/*   ret = config_open(&config, file, C_READ); */
/*   if (ret == 0) */
/*     goto config_open; */
/*   ret = config_read(&config, profile, "username", ylad->yahoo_id, sizeof(ylad->yahoo_id) - 1); */
/*   if (ret == 0) */
/*     goto config_read; */
/*   ret = config_read(&config, profile, "password", ylad->password, sizeof(ylad->password) - 1); */
/*   if (ret == 0) */
/*     goto config_read; */

/*   config_close(&config); */
/*   g_free(file); */
/*   return 0; */
  
/*  config_read: */
/*  config_open: */
/*   g_free(file); */
/*   return -1; */
/* } */

int 
main(int argc, char * argv[])
{
  gchar buffer[INPUT_LINE_LENGTH];
  int status;
  int log_level;
  int fd_stdin = fileno(stdin);
  YList *l = CONNECTIONS;

  ylad = y_new0(yahoo_local_account, 1);

/*   if (init_config(NULL, argv[1]) == -1) */
/*     { */
/*       fprintf(stderr, "Invalid configuration for profile \"%s\"\n", argv[1]); */
/*       return EXIT_FAILURE; */
/*     } */

  fgets(ylad->yahoo_id, sizeof(ylad->yahoo_id), stdin);
  fgets(ylad->password, sizeof(ylad->password), stdin);
  chomp(ylad->yahoo_id);
  chomp(ylad->password);

  local_host = strdup(get_local_addresses());

  register_callbacks();

  do_yahoo_debug = log_level = 0;
  status = 0;
  yahoo_set_log_level(log_level);
  ext_yahoo_login(ylad, status);

  while (POLL_LOOP) 
    {
      struct timeval tv = {.tv_sec = 1, .tv_usec = 0};
      int lfd = 0;
      fd_set inp, outp;

      FD_ZERO(&inp);
      FD_ZERO(&outp);
      FD_SET(fd_stdin, &inp);

      for(l = CONNECTIONS; l; ) 
	{
	  struct _conn *c = l->data;
	  if(c->remove) 
	    {
	      YList *n = y_list_next(l);
	      LOG(("Removing id:%d fd:%d", c->id, c->fd));
	      CONNECTIONS = y_list_remove_link(CONNECTIONS, l);
	      y_list_free_1(l);
	      free(c);
	      l = n;
	    } 
	  else 
	    {
	      if(c->cond & YAHOO_INPUT_READ)
		FD_SET(c->fd, &inp);
	      if(c->cond & YAHOO_INPUT_WRITE)
		FD_SET(c->fd, &outp);
	      if(lfd < c->fd)
		lfd = c->fd;
	      l = y_list_next(l);
	    }
	}

      select(lfd + 1, &inp, &outp, NULL, &tv);
      time(&curTime);

      if (FD_ISSET(fd_stdin, &inp))
	local_input_callback(0);

      for(l = CONNECTIONS; l; l = y_list_next(l)) 
	{
	  struct _conn *c = l->data;
	  if(c->remove)
	    continue;
	  if(FD_ISSET(c->fd, &inp))
	    yahoo_callback(c, YAHOO_INPUT_READ);
	  if(FD_ISSET(c->fd, &outp))
	    yahoo_callback(c, YAHOO_INPUT_WRITE);
	}

      if(expired(pingTimer))		
	yahoo_ping_timeout_callback();
      if(expired(webcamTimer))	
	yahoo_webcam_timeout_callback(webcam_id);
    }
  LOG(("Exited loop"));

  while (CONNECTIONS) 
    {
      YList *tmp = CONNECTIONS;
      struct _conn *c = CONNECTIONS->data;
      close(c->fd);
      FREE(c);
      CONNECTIONS = y_list_remove_link(CONNECTIONS, CONNECTIONS);
      y_list_free_1(tmp);
    }

  yahoo_logout();

  FREE(ylad);

  return 0;
}

void ext_yahoo_got_file(int id, char *who, char *url, long expires, char *msg, char *fname, unsigned long fesize)
{
}

void ext_yahoo_got_identities(int id, YList * ids)
{
}
void ext_yahoo_chat_yahoologout(int id)
{ 
  LOG(("got chat logout"));
}
void ext_yahoo_chat_yahooerror(int id)
{ 
  LOG(("got chat logout"));
}

void ext_yahoo_got_search_result(int id, int found, int start, int total, YList *contacts)
{
  LOG(("got search result"));
}

static void 
register_callbacks()
{
#ifdef USE_STRUCT_CALLBACKS
  static struct yahoo_callbacks yc;

  yc.ext_yahoo_add_handler = ext_yahoo_add_handler;
  yc.ext_yahoo_chat_cat_xml = ext_yahoo_chat_cat_xml;
  yc.ext_yahoo_chat_join = ext_yahoo_chat_join;
  yc.ext_yahoo_chat_message = ext_yahoo_chat_message;
  yc.ext_yahoo_chat_userjoin = ext_yahoo_chat_userjoin;
  yc.ext_yahoo_chat_userleave = ext_yahoo_chat_userleave;
  yc.ext_yahoo_chat_yahooerror = ext_yahoo_chat_yahooerror;
  yc.ext_yahoo_chat_yahoologout = ext_yahoo_chat_yahoologout;
  yc.ext_yahoo_conf_message = ext_yahoo_conf_message;
  yc.ext_yahoo_conf_userdecline = ext_yahoo_conf_userdecline;
  yc.ext_yahoo_conf_userjoin = ext_yahoo_conf_userjoin;
  yc.ext_yahoo_conf_userleave = ext_yahoo_conf_userleave;
  yc.ext_yahoo_connect = ext_yahoo_connect;
  yc.ext_yahoo_connect_async = ext_yahoo_connect_async;
  yc.ext_yahoo_contact_added = ext_yahoo_contact_added;
  yc.ext_yahoo_error = ext_yahoo_error;
  yc.ext_yahoo_game_notify = ext_yahoo_game_notify;
  yc.ext_yahoo_got_buddies = ext_yahoo_got_buddies;
  yc.ext_yahoo_got_conf_invite = ext_yahoo_got_conf_invite;
  yc.ext_yahoo_got_cookies = ext_yahoo_got_cookies;
  yc.ext_yahoo_got_file = ext_yahoo_got_file;
  yc.ext_yahoo_got_identities = ext_yahoo_got_identities;
  yc.ext_yahoo_got_ignore = ext_yahoo_got_ignore;
  yc.ext_yahoo_got_im = ext_yahoo_got_im;
  yc.ext_yahoo_got_search_result = ext_yahoo_got_search_result;
  yc.ext_yahoo_got_webcam_image = ext_yahoo_got_webcam_image;
  yc.ext_yahoo_log = ext_yahoo_log;
  yc.ext_yahoo_login_response = ext_yahoo_login_response;
  yc.ext_yahoo_mail_notify = ext_yahoo_mail_notify;
  yc.ext_yahoo_rejected = ext_yahoo_rejected;
  yc.ext_yahoo_remove_handler = ext_yahoo_remove_handler;
  yc.ext_yahoo_status_changed = ext_yahoo_status_changed;
  yc.ext_yahoo_system_message = ext_yahoo_system_message;
  yc.ext_yahoo_typing_notify = ext_yahoo_typing_notify;
  yc.ext_yahoo_webcam_closed = ext_yahoo_webcam_closed;
  yc.ext_yahoo_webcam_data_request = ext_yahoo_webcam_data_request;
  yc.ext_yahoo_webcam_invite = ext_yahoo_webcam_invite;
  yc.ext_yahoo_webcam_invite_reply = ext_yahoo_webcam_invite_reply;
  yc.ext_yahoo_webcam_viewer = ext_yahoo_webcam_viewer;

  yahoo_register_callbacks(&yc);
	
#endif
}
