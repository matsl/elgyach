/*
 * $Id$
 *
 * Copyright (C) 2001, 2002 Flavio de Ayra Mendes <h4@locked.org>
 */

#ifndef _CONFIG_API_H
#define _CONFIG_API_H

#include "utils.h"
#include "find.h"

/* feel free to change this settings but remember that the line must be 
   bigger or equal to MAX_SECTION_SIZE + MAX_OPTION_VALUE + 3 */
#define MAX_SECTION_SIZE 128
#define MAX_OPTION_SIZE 128
#define MAX_VALUE_SIZE 256
#define MAX_LINE_SIZE (MAX_OPTION_SIZE + MAX_VALUE_SIZE + 128)

/* config_api main struct */
typedef struct {
	/* main stream */
	FILE *fp;

	/* descriptor of fp */
	int fd;

	/* misc timeout */
	unsigned int timeout;
	
	/* handled by config_open */
	char *filename;
	
	/* set this for having debug on config_* functions */
	int debug;

	/* func called when config file is changed */
	void (*func)(void *);
	void *func_arg;

	unsigned int openned;
} config_t;

/* config_open() flags */
enum {
	C_READ,
	C_WRITE
};

/* open config file 
   returns 0 on error and 1 on success */
extern int config_open(config_t *conf, const char *filename, int flags);

/* check syntax of the file 
   set output to 0 for silent mode and 1 for print syntax error reporting
   returns 0 on error and 1 on success */
extern int config_check(config_t *conf, int output);

/* read an option and store value in dest 
   returns 0 on error and 1 on succes */
extern int config_read(config_t *conf, const char *section, const char *option,
		       char *dest, size_t dest_size);

/* read an option by order store value in dest 
   returns 0 on error and 1 on succes */
int config_read_num(config_t *conf, const char *section, 
		    unsigned int option_num, char *dest, size_t dest_size);


/* write or change an option/value 
   returns 0 on error and 1 on success */
extern int config_write(config_t *conf, const char *section, const char *option,
		    	const char *value);

/* erase a whole section or an option 
   set option to NULL for erase a whole section 
   returns 0 on error and 1 on success */
extern int config_erase(config_t *conf, const char *section, 
			const char *option);


/* returns the number of sections or -1 on error */
extern int config_get_num_sections(config_t *conf);

/* returns the number of options in a section of -1 on error */
extern int config_get_num_options(config_t *conf, char *section);


/* returns number of sections on success or -1 on error */
extern char **config_get_sections(config_t *conf);


/* returns number of options on success or -1 on error */
extern char **config_get_options(config_t *conf, char *section);


/* run *func(func_arg) on a new thread when conf->fp changes */
extern void config_on_change(config_t *conf, void (*func)(void *arg), 
			     void *func_arg, unsigned int timeout);

/* close config file */
extern int config_close(config_t *conf);


#endif /* _CONFIG_API_H */
