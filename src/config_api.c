/*
 * $Id$
 * 
 * Copyright (C) 2001, 2002 Flavio de Ayra Mendes <h4@locked.org>
 */
 
#include <stdio.h>
#include <stdlib.h>
#define __USE_XOPEN
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "config_api.h"

/* for using with pthread_create() */
static void on_change(config_t *conf)
{
	struct stat st;
	time_t t = time(NULL);
	
	while (1) {
		/* timeout */
		sleep(conf->timeout);
		

		/* check if file is changed */
		if (fstat(conf->fd, &st) == -1)
			return;
		if (st.st_ctime > t) {
			/* if is changed execute func() and update t */
			t = st.st_ctime;
			conf->func(conf->func_arg);
		}
	}
}

int config_open(config_t *conf, const char *filename, int flag)
{
	struct stat st;
	char mode[2];
	
	/* set fopen() mode or return if flag is not valid */
	if (flag == C_WRITE) {
		if (stat(filename, &st) == -1)
			strcpy(mode, "w");
		else
			strcpy(mode, "r+");
	} else if (flag == C_READ) {
		strcpy(mode, "r");
	} else {
		return 0;
	}

	/* open itself */
	memset(conf, 0, sizeof(config_t));
	if (!(conf->fp = fopen(filename, mode))) {
		if (conf->debug)
			fprintf(stderr, "config_open(): cannot fopen() %s: %s",
					filename, strerror(errno));
		return 0;
	}	

	/* alloc and copy the config filename to the config_t */
	conf->filename = malloc(strlen(filename) + 1);
	if (conf->filename == NULL) {
		if (conf->debug)
			fprintf(stderr, "config_open(): cannot alocate space "
				"for filename: %s\n", strerror(errno));
		return 0;
	}
	strcpy(conf->filename, filename);
	/* set it as openned */
	conf->openned = 1;
	
	return 1;
}

int config_check(config_t *conf, int output)
{
	off_t initial_offset; /* offset saving */
	unsigned int line = 0; /* line number */
	unsigned int section = 0; 
	unsigned int error = 0; /* error checking */
	char buff[MAX_LINE_SIZE]; /* line buffer */
	char section_tmp[MAX_SECTION_SIZE]; /* tmporary for sscanf() */
	char option_tmp[MAX_OPTION_SIZE]; /* tmporary for sscanf() */
	char value_tmp[MAX_VALUE_SIZE]; /* tmporary for sscanf() */

	/* file not openned */
	if (!conf->openned) {
		if (conf->debug)
			fprintf(stderr, "config_check(): config file not "
					"openned\n");
		return 0;
	}

	/* save offset before change */
	initial_offset = ftell(conf->fp);
	
	/* let's work */
	while (!feof(conf->fp)) {
		/* get buffer */
		memset(buff, 0, MAX_LINE_SIZE);
		my_fgets(buff, MAX_LINE_SIZE, conf->fp);
		/* line counting */
		line++;

		/* ignore comments and blank lines */
		if (*buff == '#' || *buff == '\r' || *buff == '\n' || 
		    !strlen(buff))
			continue;
		
		/* cutt off every \n and \r of the string */
		chomp(buff);
		
		memset(section_tmp, 0, MAX_SECTION_SIZE);
		memset(option_tmp, 0, MAX_OPTION_SIZE);
		memset(value_tmp, 0, MAX_VALUE_SIZE);

		if (is_a_section(buff, section_tmp, MAX_SECTION_SIZE - 1)) {
			/* ok, its a section */
			section = 1;	
		} else if (is_an_option(buff, option_tmp, MAX_OPTION_SIZE - 1,
					value_tmp, MAX_VALUE_SIZE - 1)) {
			if (!section) {
				/* option without a section */
				error = 1;
				if (output)
					fprintf(stderr, "%s:%d: option '%s' "
							"whithout a section.\n",
							conf->filename, line, 
							option_tmp);
			}
		} else {
			error = 1;
			if (output)
				fprintf(stderr, "%s:%d: syntax error: '%s'\n", 
						conf->filename, line, buff);
		}
	}

	/* get back to initial offset */
	fseek(conf->fp, initial_offset, SEEK_SET);

	/* return 0 if failed or 1 if ok */
	return (error ? 0: 1);
}


int config_read(config_t *conf, const char *section, const char *option,
			char *dest, size_t dest_size)
{
	off_t option_offset;
	off_t section_offset;
	off_t initial_offset;
	char line_tmp[MAX_LINE_SIZE];
	
	/* return if the file isn't openned */
	if (!conf->openned) {
		if (conf->debug)
			fprintf(stderr, "config_read(): config file not "
					"openned\n");
		return 0;
	}

	/* save initial offset */
	initial_offset = ftell(conf->fp);

	/* return if section not found */
	if ((section_offset = find_section(conf->fp, section)) == -1) {
		if (conf->debug)
			fprintf(stderr, "config_read(): cannot find section "
					"'%s'\n", section);
		return 0;
	}

	/* return if option not found */
	if ((option_offset = find_option(conf->fp, option, 
					 section_offset)) == -1) {
		if (conf->debug)
			fprintf(stderr, "config_read(): cannot find option '%s'"
					" in section '%s'\n", option, section);
		return 0;
	}
	
	/* go to the option offset */
	fseek(conf->fp, option_offset, SEEK_SET);
	
	/* get the buffer and fix spaces */
	memset(line_tmp, 0, MAX_LINE_SIZE);
	my_fgets(line_tmp, MAX_LINE_SIZE, conf->fp);
	spacefix(line_tmp);


	/* return if something nasty happens with sscanf */
	if (!is_an_option(line_tmp, NULL, 0, dest ? dest : NULL, 
			  dest ? dest_size : 0)) {
		fprintf(stderr, "config_read(): cannot get value for option "
				"'%s'\n", option);
		return 0;
	}
	
	/* get back to initia offset */
	fseek(conf->fp, initial_offset, SEEK_SET);

	return 1;
}

int config_read_num(config_t *conf, const char *section, 
		    unsigned int option_num, char *dest, size_t dest_size)
{
	off_t section_offset;
	off_t initial_offset;
	char line_tmp[MAX_LINE_SIZE];
	
	/* return if the file isn't openned */
	if (!conf->openned) {
		if (conf->debug)
			fprintf(stderr, "config_read(): config file not "
					"openned\n");
		return 0;
	}
	
	if (dest == NULL || !dest_size) {
		if (conf->debug)
			fprintf(stderr, 
				"config_read(): invalid dest buffer\n");
		return 0;
	}

	if (!option_num) {
		if (conf->debug)
			fprintf(stderr, "config_read(): option_num is zero\n");
		
		return 0;
	}

	/* save initial offset */
	initial_offset = ftell(conf->fp);

	/* return if section not found */
	if ((section_offset = find_section(conf->fp, section)) == -1) {
		if (conf->debug)
			fprintf(stderr, "config_read(): cannot find section "
					"'%s'\n", section);
		return 0;
	}

	/* go to the option offset */
	fseek(conf->fp, section_offset, SEEK_SET);

	/* ignore the section line */
	my_fgets(line_tmp, MAX_LINE_SIZE, conf->fp);
	
	/* main loop to find the option */
	while (option_num--) {
		/* get the buffer and fix spaces */
		memset(line_tmp, 0, MAX_LINE_SIZE);
		my_fgets(line_tmp, MAX_LINE_SIZE, conf->fp);
		spacefix(line_tmp);
		
//		fprintf(stderr, "linha: %s\n", line_tmp);
		if (is_a_section(line_tmp, NULL, 0))
			return 0;

		/* return if something nasty happens with sscanf */
		if (!is_an_option(line_tmp, NULL, 0, 
				  (!option_num ? dest : NULL),
				  (!option_num ? dest_size : 0))) {
			option_num++;
		}
	}

	/* get back to initia offset */
	fseek(conf->fp, initial_offset, SEEK_SET);
	
	return 1;
}


int config_write(config_t *conf, const char *section, const char *option, 
		 const char *value)
{
	off_t section_offset; 
	off_t option_offset;
	off_t *write_offset;
	off_t initial_offset;
	char tmp[MAX_LINE_SIZE];
	FILE *tmp_fp;
	char template[] = "config_writeXXXXXX";

	/* return if the file isn't openned yet */
	if (!conf->openned) {
		if (conf->debug)
			fprintf(stderr, "config_write(): config file not "
					"openned\n");
		return 0;
	}

	/* save the initial offset */
	initial_offset = ftell(conf->fp);

	/* do nothing if is the same value */
	memset(tmp, 0, MAX_LINE_SIZE); 
	if (config_read(conf, section, option, tmp, MAX_LINE_SIZE))
		if (!strcmp(tmp, value))
			return 1;

	if ((section_offset = find_section(conf->fp, section)) == -1) {
		/* write at the end if section not found */
        	fseek(conf->fp, 0, SEEK_END);
        	fprintf(conf->fp, "\n[%s]\n", section);
        	fprintf(conf->fp, "%s = %s\n", option, value);
	} else {
		if ((option_offset = find_option(conf->fp, option, 
						  section_offset)) == -1) {
			fseek(conf->fp, section_offset, SEEK_SET);
			my_fgets(tmp, MAX_LINE_SIZE, conf->fp);
			section_offset = ftell(conf->fp);
			write_offset = &section_offset;
		} else { 
			/* option exisits. just change */
			write_offset = &option_offset;
		}
		
		/* start a tmporary file */
		tmp_fp = my_tmpfile(template, "/tmp");
		if (tmp_fp == NULL) {
			if (conf->debug)
				fprintf(stderr, "config_write(): cannot open "
						"temporary file\n");
			return 0;
		}

		/* copy everything after truncate point to tmp file */
		fseek(conf->fp, *write_offset, SEEK_SET);
		while (!feof(conf->fp)) {
			memset(tmp, 0, MAX_LINE_SIZE);
			my_fgets(tmp, MAX_LINE_SIZE, conf->fp);	
			fprintf(tmp_fp, "%s", tmp);
		}

		ftruncate(fileno(conf->fp), *write_offset);
		fseek(conf->fp, *write_offset, SEEK_SET);
        	fprintf(conf->fp, "%s = %s\n", option, value);
		
		rewind(tmp_fp);
		while (!feof(tmp_fp)) {
			memset(tmp, 0, MAX_LINE_SIZE);
			my_fgets(tmp, MAX_LINE_SIZE, tmp_fp);
			fprintf(conf->fp, "%s",tmp);
		}	
		fclose(tmp_fp);
	}

	/* get back to initial offset */
	fseek(conf->fp, initial_offset, SEEK_SET);

	return 1;
}

int config_erase(config_t *conf, const char *section, const char *option)
{
	off_t initial_offset;
	off_t section_offset;
	FILE *tmp_fp;
	char buff[MAX_LINE_SIZE];
	char tmp[MAX_LINE_SIZE];
	unsigned int copy = 0;
	unsigned int erased = 0;
	char template[] = "config_eraseXXXXXX";
	
	if (!conf->openned) {
		if (conf->debug)
			fprintf(stderr, "config_erase(): config file not "
					"openned\n");
		return 0;
	}
	
	/* save initial offset */
	initial_offset = ftell(conf->fp);

	if ((section_offset = find_section(conf->fp, section)) == -1)
		return 0;
	
	
	if ((tmp_fp = my_tmpfile(template, "/tmp")) == NULL) {
		if (conf->debug)
			fprintf(stderr, "config_erase(): cannot open temporary "
					"file\n");
		/* lets get back to initial offset before return */
		fseek(conf->fp, initial_offset, SEEK_SET);
		return 0;
	}
	
	/* copy the rest of the file after erase_offset to the tmp_fp */
	fseek(conf->fp, section_offset, SEEK_SET);
	while (!feof(conf->fp)) {
		memset(buff, 0, MAX_LINE_SIZE);
		my_fgets(buff, MAX_LINE_SIZE, conf->fp);
		fprintf(tmp_fp, "%s", buff);
	}

	/* after copy, trucate the rest of the file and get back to the 
	   offset */
	ftruncate(fileno(conf->fp), section_offset);
	fseek(conf->fp, section_offset, SEEK_SET);
	rewind(tmp_fp);

	while (!feof(tmp_fp)) {
		/* get file buffer */
		memset(buff, 0, MAX_LINE_SIZE);
		my_fgets(buff, MAX_LINE_SIZE, tmp_fp);

		/* copy comments */
		if (*buff == '#' || *buff == '\r' || *buff == '\n') {
			fprintf(conf->fp, "%s", buff);
			continue;
		}

		/* if what we want were already erased, copy */
		if (!erased) {
			if (option != NULL) {
				if (is_an_option(buff, tmp, MAX_OPTION_SIZE - 1,
						 NULL, 0)) {
					if (!strcmp(tmp, option)) {
						erased = 1;
						copy = 0;
					} else {
						copy = 1;
					}
				} else {
					copy = 1;
				}
			} else {
				if (is_a_section(buff, tmp, 
							MAX_OPTION_SIZE - 1 )) {
					if (!strcmp(tmp, section)) {
						copy = 0;
					} else {
						erased = 1;
						copy = 1;
					}
				} else {
					copy = 0;
				}
			}
		} else {
			copy = 1;
		}
		
		if (copy != 0) {
			fprintf(conf->fp, "%s", buff);
		}
	}

	fclose(tmp_fp);
	/* get back to initial offset */
	fseek(conf->fp, initial_offset, SEEK_SET);
	
	return 1;
}


int config_get_num_sections(config_t *conf)
{
	off_t initial_offset;
	char line_tmp[MAX_LINE_SIZE];
	int count = 0;
	
	/* save initial offset */
	initial_offset = ftell(conf->fp);
	
	while (!feof(conf->fp)) {
		/* get buffer */
		memset(line_tmp, 0, MAX_LINE_SIZE - 1);
		my_fgets(line_tmp, MAX_LINE_SIZE - 1, conf->fp);

		/* ignore comments */
		if (*line_tmp == '#' || *line_tmp == '\r' || *line_tmp == '\n')
			continue;

		/* ignore if its not a section */
		if (is_a_section(line_tmp, NULL, 0)) {
			count++;
		}  
	}
	

	/* get back to initial offset */
	fseek(conf->fp, initial_offset, SEEK_SET);

	return count;
}


int config_get_num_options(config_t *conf, char *section) 
{
	off_t initial_offset;
	off_t section_offset;
	char line_tmp[MAX_LINE_SIZE];
	int count = 0;

	/* save initial offset */
	initial_offset = ftell(conf->fp);

	/* find the section */
	section_offset = find_section(conf->fp, section);
	if (section_offset == -1) {
		if (conf->debug)
			fprintf(stderr, "config_get_options(): cannot find "
					"section '%s'\n", section);
		
		return -1;
	}

	/* ignore the current section line */
	fseek(conf->fp, section_offset, SEEK_SET);
	my_fgets(line_tmp, MAX_LINE_SIZE, conf->fp);
	
	while (!feof(conf->fp)) {
		/* get the buffer */
		memset(line_tmp, 0, MAX_LINE_SIZE);
		my_fgets(line_tmp, MAX_LINE_SIZE, conf->fp);

		/* ignore if is a new section */
		if (is_a_section(line_tmp, NULL, 0)) {
			continue;
		}

		if (is_an_option(line_tmp, NULL, 0,  NULL, 0)) {
			count++;
		}
	}

	/* get back to initial offset */
	fseek(conf->fp, initial_offset, SEEK_SET);

	return count;
}

char **config_get_sections(config_t *conf)
{
	off_t initial_offset;
	char line_tmp[MAX_LINE_SIZE];
	char section_tmp[MAX_SECTION_SIZE];
	char **p_dest = NULL;
	static char **dest = NULL;

	/* save initial offset */
	initial_offset = ftell(conf->fp);
	
	dest = malloc(config_get_num_sections(conf) * sizeof(char *));
	p_dest = dest;

	fprintf(stderr, "base: %p\n", dest);
	
	if (dest == NULL) {
		if (conf->debug) {
			fprintf(stderr, 
				"config_get_sections: cannot allocate space: %s",
				strerror(errno));
		}
		
		return (char **)NULL;

	}

	while (!feof(conf->fp)) {

		/* get buffer */
		memset(line_tmp, 0, MAX_LINE_SIZE - 1);
		my_fgets(line_tmp, MAX_LINE_SIZE - 1, conf->fp);

		/* ignore comments */
		if (*line_tmp == '#' || *line_tmp == '\r' || *line_tmp == '\n')
			continue;

		/* ignore if its not a section */
		if (is_a_section(line_tmp, section_tmp, MAX_SECTION_SIZE - 1)) {
			/* if dest is set, allocate space and copy
			   the section name there */
			*dest = malloc(strlen(section_tmp) + 1);
			if (*dest == NULL) {
				if (conf->debug)
					fprintf(stderr, 
						"config_get_sections():"
						" cannot allocate space"
						": %s\n", 
						strerror(errno));
				return (char **)NULL;
			}
			
			strcpy(*dest++, section_tmp);
		}  
	}
	
	*dest = NULL;

	/* get back to initial offset */
	fseek(conf->fp, initial_offset, SEEK_SET);
	
	return p_dest;
}	

		

char **config_get_options(config_t *conf, char *section)
{
	off_t initial_offset;
	off_t section_offset;
	char line_tmp[MAX_LINE_SIZE];
	char option_tmp[MAX_OPTION_SIZE];
	static char **dest;
	char **p_dest;

	/* save initial offset */
	initial_offset = ftell(conf->fp);

	/* find the section */
	section_offset = find_section(conf->fp, section);
	if (section_offset == -1) {
		if (conf->debug)
			fprintf(stderr, "config_get_options(): cannot find "
					"section '%s'\n", section);

		return (char **)NULL;
	}

	/* alloc memory for vector's base */
	dest = malloc(config_get_num_options(conf, section) * sizeof(char *));

	p_dest = dest;

	if (dest == NULL) {
		if (conf->debug) {
			fprintf(stderr, 
				"config_get_options(): cannot allocat space: %s\n",
				strerror(errno));
		}

		return (char **)NULL;
	}

	/* ignore the current section line */
	fseek(conf->fp, section_offset, SEEK_SET);
	my_fgets(line_tmp, MAX_LINE_SIZE, conf->fp);
	
	while (!feof(conf->fp)) {
		/* get the buffer */
		memset(line_tmp, 0, MAX_LINE_SIZE);
		my_fgets(line_tmp, MAX_LINE_SIZE, conf->fp);

		/* ignore if is a new section */
		if (is_a_section(line_tmp, NULL, 0))
			break;

		if (is_an_option(line_tmp, option_tmp, MAX_OPTION_SIZE - 1, 
				 NULL, 0)) {
			*dest = malloc(strlen(option_tmp) + 1);
			if (*dest == NULL) {
				if (conf->debug)
					fprintf(stderr, 
						"config_get_options(): "
						"cannot allocate space:"
						" %s\n", 
						strerror(errno));
				
				return (char **)NULL;
			}

			strcpy(*dest++, option_tmp);
		}	
	}

	*dest = NULL;

	/* get back to initial offset */
	fseek(conf->fp, initial_offset, SEEK_SET);

	return p_dest;
}



void config_on_change(config_t *conf, void (*func)(void *), void *func_arg, 
		      unsigned int timeout)
{
	pthread_t pth;

	/* get descriptor to stat() */
	conf->fd = fileno(conf->fp);
	if (conf->fd == -1)
		return;
	
	conf->timeout = timeout;
	conf->func = func;
	conf->func_arg = func_arg;

	pthread_create(&pth, NULL, (void *)&on_change, conf);

	return;
}



int config_close(config_t *conf)
{
	conf->openned = 0;

	return fclose(conf->fp);
}
