
#ifndef MISC_H
#define MISC_H

#include <glib.h>

gint strvlen(gchar **strv);
gchar *strchomp(gchar *string);

gchar *get_local_addresses();

#endif
