#include "logging.h"

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

void _cmph_vfprintf_log(enum cmph_log_level verbosity, char *fmt, va_list args)
{
    if (cmph_logger.verbosity >= verbosity) {
        vfprintf(stderr, fmt, args);
    }
}

void __cmph_log_debug(char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    cmph_logger.logger(cmph_log_level_DEBUG, fmt, args);
    va_end(args);
}

void __cmph_log_info(char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    cmph_logger.logger(cmph_log_level_INFO, fmt, args);
    va_end(args);
}

void __cmph_log_warn(char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    cmph_logger.logger(cmph_log_level_WARN, fmt, args);
    va_end(args);
}

void __cmph_log_error(char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    cmph_logger.logger(cmph_log_level_ERROR, fmt, args);
    va_end(args);
}

void cmph_logger_set_level(enum cmph_log_level verbosity)
{
    cmph_logger.verbosity = verbosity;
}

void cmph_logger_set_log_func(void (*logfunc)(enum cmph_log_level verbosity, char *fmt, va_list args))
{
    cmph_logger.logger = logfunc;
}

