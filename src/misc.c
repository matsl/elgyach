
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <glib.h>

#include "misc.h"

/* 
 * Return the length of a NULL-terminated vector of strings.
 */
gint
strvlen(gchar **strv)
{
  gint count = 0;

  for (; *strv != NULL; strv++)
    count++;
  
  return count;
}

/*
 * Remove a trailing new line from string. 
 * Return string
 */
gchar *
strchomp(gchar *string)
{
  size_t l = strlen(string);
  
  if (l > 0 || string[l - 1] == '\n')
    string[l - 1] = '\0';

  return string;
}

gchar * 
get_local_addresses()
{
  static char addresses[1024];
  char buff[1024];
  char gateway[16];
  char  * c;
  struct hostent * hn;
  int i;
  FILE * f;
  f = popen("netstat -nr", "r");
  if((int)f < 1)
    goto IP_TEST_2;
  while( fgets(buff, sizeof(buff), f)  != NULL ) {
    c = strtok( buff, " " );
    if( (strstr(c, "default") || strstr(c,"0.0.0.0") ) &&
	!strstr(c, "127.0.0" ) )
      break;
  }
  c = strtok( NULL, " " );
  pclose(f);

  strncpy(gateway,c, 16);

  for(i = strlen(gateway); gateway[i] != '.'; i-- )
    gateway[i] = 0;

  gateway[i] = 0;

  for(i = strlen(gateway); gateway[i] != '.'; i-- )
    gateway[i] = 0;

  f = popen("/sbin/ifconfig -a", "r");
  if((int)f < 1)
    goto IP_TEST_2;

  while( fgets(buff, sizeof(buff), f) != NULL ) {
    if( strstr(buff, "inet") && strstr(buff,gateway) )
      break;
  }
  pclose(f);

  c = strtok( buff, " " );
  c = strtok( NULL, " " );

  strncpy ( addresses, c, sizeof(addresses) );
  c = strtok(addresses, ":" );
  strncpy ( buff, c, sizeof(buff) );
  if((c=strtok(NULL, ":")))
    strncpy( buff, c, sizeof(buff) );

  strncpy(addresses, buff, sizeof(addresses));

  return addresses;
		
		
 IP_TEST_2:

  gethostname(buff,sizeof(buff));

  hn = gethostbyname(buff);
  if(hn)
    strncpy(addresses, inet_ntoa( *((struct in_addr*)hn->h_addr)), sizeof(addresses) );
  else
    addresses[0] = 0;

  return addresses;
}
