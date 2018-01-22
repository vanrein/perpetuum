#include "pyadapter.h"
#include "logging.h"

typedef struct {
    char*(*readfn)();
    cmph_uint32(*keylenfn)();
    void(*rewindfn)();
    void(*destroyfn)();
} cmph_io_func_adapter_t;

int _cmph_io_function_adapter_read(void *data, char **key, cmph_uint32 *keylen) {
    cmph_io_func_adapter_t *func_bag = (cmph_io_func_adapter_t *)data;
    *key = func_bag->readfn();
    int len = func_bag->keylenfn();
    *keylen = len;
    return (int)(*keylen);
}

void _cmph_io_function_adapter_rewind(void *data) {
    cmph_io_func_adapter_t *func_bag = (cmph_io_func_adapter_t *)data;
    func_bag->rewindfn();
}

void _cmph_io_function_adapter_dispose(void *data, char *key, cmph_uint32 keylen) {
    // Currently the adapter code should ‟lose” this ref
    // causing a GC of the box. 
}

cmph_io_adapter_t *cmph_io_function_adapter(char*(*readfn)(),
                                                   void(*rewindfn)(),
                                                   void(*destroyfn)(),
                                                   cmph_uint32(*keylenfn)(),
                                                   cmph_uint32 nkeys) {
    cmph_io_func_adapter_t *func_bag = 
        (cmph_io_func_adapter_t*) calloc(1, sizeof(cmph_io_adapter_t));
    if (func_bag == NULL) return NULL;

    func_bag->readfn = readfn;
    func_bag->rewindfn = rewindfn;
    func_bag->keylenfn = keylenfn;
    func_bag->destroyfn = destroyfn;

    cmph_io_adapter_t *adapter = 
        (cmph_io_adapter_t*) calloc(1, sizeof(cmph_io_adapter_t));
    if (adapter == NULL) return NULL;

    adapter->data = (void*)func_bag;
    adapter->nkeys = nkeys;
    adapter->read = _cmph_io_function_adapter_read;
    adapter->rewind = _cmph_io_function_adapter_rewind;
    adapter->dispose = _cmph_io_function_adapter_dispose;

    return adapter;
}

void cmph_io_function_adapter_destroy(cmph_io_adapter_t *adapter) {
    cmph_io_func_adapter_t *func_bag = (cmph_io_func_adapter_t *)adapter->data;
    func_bag->destroyfn();
    free(func_bag);
    free(adapter);
}
