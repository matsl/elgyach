
#include <sys/types.h>
#include <errno.h>
#include <unistd.h>

#include "socket.h"

ssize_t
write_to_socket(int sockfd, const char *buffer, size_t length)
{
  ssize_t tcount, rcount;
  
  for (tcount = 0; tcount < length; tcount += rcount)
    {
      rcount = write(sockfd, buffer + rcount, length - rcount);
      if (rcount == -1)
        return -1;
    }

  return rcount;
}
