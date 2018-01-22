#include <stdarg.h>
#include <stdio.h>
#include "logging.h"
#include "pylogging.h"

static void __cmph_python_logfunc(enum cmph_log_level verbosity,
                           char *fmt, va_list args) {

    char *output;
    int alloc = vasprintf(&output, fmt, args);
    if (alloc == -1) {
        _py_log.log_fn(0, "VASNPRINTF FAIL !");
        return;
    }
    _py_log.log_fn(verbosity, output);
    free(output);
}

extern void _cmph_setup_py_logger(void (*_log_fn)(int, char*)) {
    _py_log.log_fn = _log_fn;
    cmph_logger_set_log_func(__cmph_python_logfunc);
    cmph_logger.info("Installed python logging hooks");
}
