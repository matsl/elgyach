/*
 * $Id$
 * 
 * Copyright (C) 2001, 2002 Flavio de Ayra Mendes <h4@locked.org>
 */

#include <stdio.h>
#define __USE_XOPEN
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "config_api.h"

off_t find_section(FILE *fp, const char *section)
{
	char tmp[MAX_LINE_SIZE], section_tmp[MAX_SECTION_SIZE];
	off_t saved_offset;
	off_t initial_offset;

	/* save initial offset */
	initial_offset = ftell(fp);
	
	rewind(fp);
	while (!feof (fp)) {
		/* save offset before get the buffer for return exatly the 
		   offset of the section line */
		saved_offset = ftell(fp);
		
		/* get the buffer */
		memset(tmp, 0, MAX_LINE_SIZE);
		if (!my_fgets(tmp, MAX_LINE_SIZE, fp)) 
			break;
		
		/* ignore comments */
		if (*tmp == '#' || *tmp == '\r' || *tmp == '\n') 
			continue;

		/* is a section line */
		if (is_a_section(tmp, section_tmp, MAX_SECTION_SIZE - 1)) {
			/* yes, this is our section. return his offset */
			if (!strcmp(section_tmp, section)) 
				return saved_offset;
		}
	}

	/* get back to initial offset */
	fseek(fp, initial_offset, SEEK_SET);

	return -1;
}

off_t find_option(FILE *fp, const char *option, off_t section_offset)
{
        char 	tmp[MAX_LINE_SIZE], option_tmp[MAX_OPTION_SIZE];
	off_t 	offset;
	int	sec = 0;
	
	fseek(fp, section_offset, SEEK_SET);

	while (!feof(fp)) {
		offset = ftell(fp);
		
		memset(tmp, 0, MAX_LINE_SIZE);
		if (!my_fgets(tmp, MAX_LINE_SIZE, fp))
			break;
		
		if (*tmp == '#' || *tmp == '\r' || *tmp == '\n') 
			continue;
		spacefix(tmp);

		if (*tmp == '[') {
			if (sec) {
				return -1;
			} else {
				sec = 1;
				continue;
			}
		} 

		if (is_an_option(tmp, option_tmp, MAX_OPTION_SIZE - 1, NULL, 
				 0)) {
			if (!strcmp(option_tmp, option))
				return offset;
		}
	}

	return -1;
}


