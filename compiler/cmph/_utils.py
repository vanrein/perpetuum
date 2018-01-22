import six
from os.path import abspath, exists

unicode = str if six.PY3 else unicode # noqa


unicode = str if six.PY3 else unicode  # noqa


def is_file_location(obj):
    """
    Check to see if the presented object could represent a file location
    """
    return isinstance(obj, six.string_types) and exists(abspath(obj))


def is_file(obj):
    """
    Check to see if the presented object could represent a file
    """
    return hasattr(obj, 'fileno')


def convert_to_bytes(key):
    is_unicrud = (six.PY3 and isinstance(key, six.string_types)) or \
                 (six.PY2 and isinstance(key, unicode))

    if is_unicrud:
        return bytes(key.encode('utf8'))
    elif isinstance(key, six.string_types):
        return bytes(key)
    elif isinstance(key, six.integer_types):
        return bytes([key])
    else:
        return bytes(key)
