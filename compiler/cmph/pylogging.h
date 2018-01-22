#ifndef __PYLOGGING_H__
#define __PYLOGGING_H__

typedef struct {
    void (*log_fn)(int verbosity, char* to_log);
} _py_log_t;

static _py_log_t _py_log = {
    .log_fn = NULL
};

extern void _cmph_setup_py_logger(void (*_py_logger)(int, char*));

#endif
