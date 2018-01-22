#ifndef __DEBUG_H__
#define __DEBUG_H__

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "logging.h"

#ifdef DEBUGP
#undef DEBUGP
#endif

#ifdef __cplusplus
#include <cstdio>
#ifdef WIN32
#include <cstring>
#endif
#else
#include <stdio.h>
#ifdef WIN32
#include <string.h>
#endif
#endif

static void debugprintf(char *file, int line, const char *format, ...)
{
    char *file_details = NULL;
    int len = asprintf(&file_details, "[%s:%d] ", file, line);
    if (len != -1) {
        char *new_fmt = (char*) calloc(sizeof(char), len + strlen(format) + 1);
        strcpy(new_fmt, file_details);
        free(file_details);
        strcpy((char*) &new_fmt[len], format);

        va_list ap;
        va_start(ap, format);
        cmph_logger.logger(cmph_log_level_DEBUG, new_fmt, ap);
        va_end(ap);

        free(new_fmt);
    }
}
#endif

#ifdef DEBUG
#define DEBUGP(args...) do { debugprintf(__FILE__, __LINE__, ## args); } while(0)
#else
#define DEBUGP(args...) do { } while(0)
#endif
