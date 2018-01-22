#ifndef __LOGGING_H__
#define __LOGGING_H__

#include <stdlib.h>
#include <stdarg.h>

enum cmph_log_level {
    cmph_log_level_ERROR = 0,
    cmph_log_level_WARN  = 1,
    cmph_log_level_INFO  = 2,
    cmph_log_level_DEBUG = 3
};

// Forward declarations
void cmph_logger_set_log_func(void (*logfunc)(enum cmph_log_level verbosity, char *fmt, va_list args));
void cmph_logger_set_level(enum cmph_log_level verbosity);
void _cmph_vfprintf_log(enum cmph_log_level verbosity, char *fmt, va_list args);
void __cmph_log_debug(char *fmt, ...);
void __cmph_log_info(char *fmt, ...);
void __cmph_log_warn(char *fmt, ...);
void __cmph_log_error(char *fmt, ...);

/** Logging specific functionality
*/
typedef struct {
    void (*logger)(enum cmph_log_level verbosity, char *str, va_list args);
    void (*error)(char *str, ...);
    void (*warn)(char *str, ...);
    void (*info)(char *str, ...);
    void (*debug)(char *str, ...);
    enum cmph_log_level verbosity;
} cmph_log_t;

static cmph_log_t cmph_logger = {
    .logger = _cmph_vfprintf_log,
    .debug  = __cmph_log_debug,
    .info   = __cmph_log_info,
    .warn   = __cmph_log_warn,
    .error  = __cmph_log_error,
    .verbosity = cmph_log_level_INFO
};
#endif
