#ifndef __PYADAPTER_H__
#define __PYADAPTER_H_

#include "cmph.h"

// The functions that are provided to this adapter are a bridge
// and hold their own data elsewhere
cmph_io_adapter_t *cmph_io_function_adapter(char*(*readfn)(),
                                            void(*rewindfn)(),
                                            void(*destroyfn)(),
                                            cmph_uint32(*keylenfn)(),
                                            cmph_uint32 nkeys);
void cmph_io_function_adapter_destroy(cmph_io_adapter_t * adapter);

#endif
