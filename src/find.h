/*
 * $Id$
 *
 * Copyright (C) 2001, 2002 Flavio de Ayra Mendes <h4@locked.org>
 */

#ifndef _FIND_H
#define _FIND_H

#include <stdio.h>
#ifndef __USE_XOPEN
#define __USE_XOPEN
#endif
#include <unistd.h>
#include <string.h>

extern off_t find_section(FILE *fp, const char *section);
extern off_t find_option(FILE *fp, const char *option, off_t section_offset);

#endif /* _FIND_H */
