/*
 * $Id$
 * 
 * Copyright (C) 2001, 2002 Flavio de Ayra Mendes <h4@locked.org>
 */
 
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "config_api.h"

void spacefix(char *string)
{
	size_t 	len, ilen;
	char 	*tmp, *p = string;

	ilen = strlen(string) + 1;
	tmp = malloc(ilen);
	if (tmp == NULL) {
		*string = 0;
		return;
	}
		
	while ((*p == ' ' || *p == '\t') && *p != '\0') 
		p++;
	memset(tmp, 0, sizeof(tmp));
	strncpy(tmp, p, ilen);

	len = strlen(tmp); 
	p = &tmp[len];

	while ((*p == ' ' || *p == '\t') && len) { 
		p--; 
		len--; 
	}
	*(p + 1) = '\0';

	memset(string, 0, sizeof(string));
	strncpy(string, tmp, strlen(tmp));
	
	return;
}

void chomp(char *str)
{
	char *p = str;

	if (*str == '\n') 
		return;

	while (*p) {
		if (*p == '\n' || *p == '\r') 
			*p = 0;
		p++;
	}

	return;
}

int my_fgets(char *dest, ssize_t dest_size, FILE *fp)
{
        char tmp[MAX_LINE_SIZE];
        unsigned int ok = 0;

	memset(dest, 0, dest_size);
        while (!ok) {
		if (!fgets(tmp, dest_size, fp))
                        return 0;

                if (tmp[strlen(tmp) - 2] == '\\') {
                        tmp[strlen(tmp) - 2] = '\0';
                        ok = 0;
                } else {
                        ok = 1;
                }
                
                strncat(dest, tmp, dest_size);
        }

        return 1;
}

	
FILE *my_tmpfile(char *template, const char *path)
{
	char i;
	char *p = template;
	char *filename;
	struct stat s;
	FILE *fp;

	filename = malloc(strlen(template) + strlen(path) + 2);
	if (filename == NULL)
		return NULL;

	while (*p != 'X')
		p++;

	srand(time(NULL));
	do {
		while (*p == 'X') {
			i = 0;
			while ((i < 48) || (i > 57 && i < 65) || 
			       (i > 90 && i < 97) || (i > 122)) {
				i = 1+(int)(122.0*rand()/(RAND_MAX+1.0));
			}
			*p++ = i;
		}
		sprintf(filename, "%s/%s", path, template);
	} while (!stat(filename, &s));
	
	fp = fopen(filename, "a+");
	
	/* temporary file! when you close this stream the file is removed */
	unlink(filename);
	
	return fp;
}

int is_a_section(const char *buffer, char *dest, size_t dest_size)
{
	char section_tmp[MAX_SECTION_SIZE];

	memset(section_tmp, 0, dest_size + 1);
	/* if its not a section return 0 */

	if (!sscanf(buffer, "[%[^]]]", section_tmp))
		return 0;
	
	/* if the dest argument were set copy the section name there */
	if (dest)
		strncpy(dest, section_tmp, (dest_size < MAX_SECTION_SIZE ? 
				    	    dest_size : MAX_SECTION_SIZE));

	if (!strlen(section_tmp))
		return 0;

	/* return true */
	return 1;
}

int is_an_option(const char *buffer, char *dest_opt, size_t opt_size,
		 char *dest_val, size_t val_size)
{
	char option_tmp[MAX_OPTION_SIZE];
	char value_tmp[MAX_VALUE_SIZE];

	if (option_tmp == NULL || value_tmp == NULL)
		return 0;
		
	memset(option_tmp, 0, MAX_OPTION_SIZE);
	memset(value_tmp, 0, MAX_VALUE_SIZE);
	/* if its not an option return 0 */
	if (sscanf(buffer, "%[^= ] = %[^=\n]", option_tmp, value_tmp) != 2)
		return 0;

	/* if dest_opt were passed copy the option name there */
	if (dest_opt)
		strncpy(dest_opt, option_tmp, (opt_size < MAX_OPTION_SIZE ? 
						opt_size : MAX_OPTION_SIZE));

	/* if dest_val were passed copy the value there */
	if (dest_val)
		strncpy(dest_val, value_tmp, (val_size < MAX_VALUE_SIZE ?
					      val_size : MAX_VALUE_SIZE));

	/* return true */
	return 1;
}
