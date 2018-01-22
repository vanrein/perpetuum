from collections import Sequence
from ._utils import is_file, is_file_location, convert_to_bytes
import logging


# pylint: disable=invalid-name
logger = logging.getLogger(__name__)
# pylint: enable=invalid-name


# pylint: disable=too-many-instance-attributes
class _PythonListAdapter(object):

    def __init__(self, data, encoding='utf8'):
        self.data = data
        self.cursor = 0
        self.current = None
        self.box = None
        self.encoding = encoding

    def read(self):
        if self.cursor < len(self.data):
            index = self.cursor
            self.cursor = index + 1
            self.current = convert_to_bytes(self.data[index])
        else:
            self.current = None
        return self.current

    def rewind(self):
        self.cursor = 0

    def destroy(self):
        pass


class _AdapterCxt(object):

    def __init__(self, adapter, destructor):
        self.adapter = adapter
        self.destructor = destructor

    def __enter__(self):
        return self.adapter

    def __exit__(self, *args):
        if self.destructor:
            self.destructor()


def _create_pyobj_adapter(cmph, ffi, obj):
    nkeys = len(obj)

    py_adapter = _PythonListAdapter(obj)

    @ffi.callback('char*()')
    def read_fn():
        cstr = py_adapter.read()
        if cstr is not None:
            py_adapter.box = ffi.new('char[]', cstr)
            return py_adapter.box
        else:
            return None

    @ffi.callback('cmph_uint32()')
    def keylen_fn():
        # It is important to tell C that the len might be 0
        # CMPH rather irritatingly does not always check the pointer
        # that comes back
        if py_adapter.current is not None:
            return ffi.sizeof('char') * (len(py_adapter.current))
        else:
            return 0

    @ffi.callback('void()')
    def rewind_fn():
        py_adapter.rewind()

    @ffi.callback('void()')
    def destroy_fn():
        pass

    adapter = cmph.cmph_io_function_adapter(read_fn, rewind_fn,
                                            destroy_fn, keylen_fn,
                                            nkeys)
    dtor = lambda: cmph.cmph_io_function_adapter_destroy(adapter)
    # THIS IS VITAL - without this you are going to
    # accidentally GC the callbacks and freak C out
    # pylint: disable=attribute-defined-outside-init
    to_ret = _AdapterCxt(adapter, dtor)
    to_ret.rd_cb = read_fn
    to_ret.rw_cb = rewind_fn
    to_ret.dt_cb = destroy_fn
    to_ret.kl_cb = keylen_fn
    # pylint: enable=attribute-defined-outside-init
    return to_ret


def create_adapter(cmph, ffi, obj):
    """ Generates a wrapped adapter for the given object

    Parameters
    ----------
    obj : list, buffer, array, or file

    Raises
    ------
    ValueError
        If presented with an object that cannot be adapted

    Returns
    -------
    CMPH capable adapter
    """

    # if arraylike and fixed unit size
    # if file
    # if buffer

    if is_file_location(obj):
        # The FP is captured for GC reasons inside the dtor closure
        # pylint: disable=invalid-name
        fd = open(obj)
        adapter = cmph.cmph_io_nlfile_adapter(fd)

        def dtor():
            cmph.cmph_io_nlfile_adapter_destroy(adapter)
            fd.close()

        # pylint: enable=invalid-name
        return _AdapterCxt(adapter, dtor)
    elif is_file(obj):
        adapter = cmph.cmph_io_nlfile_adapter(obj)
        dtor = lambda: cmph.cmph_io_nlfile_adapter_destroy(adapter)
        return _AdapterCxt(adapter, dtor)
    elif isinstance(obj, Sequence):
        if len(obj) == 0:
            raise ValueError("An empty sequence is already a perfect hash!")
        return _create_pyobj_adapter(cmph, ffi, obj)
    else:
        raise ValueError("data cannot have a cmph wrapper generated")
