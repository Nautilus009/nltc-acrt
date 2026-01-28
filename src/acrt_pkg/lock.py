import contextlib


@contextlib.contextmanager
def advisory_lock(_path):
    yield
