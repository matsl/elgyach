
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "text.h"
#include "output.h"

/* replace \n with \\n in a string
 * 
 * text: the text to escape
 * returns escaped text, or NULL on errror (caller must free result)
 */
static char *
escape_to_line(char *text)
{
  long nl_count;
  char *ptr, *result;
  size_t result_len;
  char *ptr_r, *ptr_w;

  for (nl_count = 0, ptr = text; *ptr; ptr++)
    if (*ptr == '\n')
      nl_count++;

  result_len = strlen(text) + nl_count + 1;
  result = malloc(result_len);
  if (result == NULL)
    goto error_malloc;

  for (ptr_w = result, ptr_r = text; *ptr_r; ptr_r++)
    {
      if (*ptr_r == '\n')
	{
	  *ptr_w = '\\'; *(ptr_w + 1) = 'n'; 
	  ptr_w += 2;
	}
      else
	{
	  *ptr_w = *ptr_r;
	  ptr_w++;
	}
    }
  *ptr_w = '\0';

  return result;

 error_malloc:
  return NULL;
}


int
append_to_textbox(char *text)
{
  strip_tags(text); 
  strip_ansi(text); 

  text = escape_to_line(text);
  if (text == NULL)
    {
      perror("escape_to_line");
      abort();
    }
  fprintf(stdout, "%s\n", text);
  free(text);

  fflush(stdout);
  
  return 0;
}
