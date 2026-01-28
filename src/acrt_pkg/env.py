import os


class EnvError(Exception):
    pass


def require_env(name):
    value = os.environ.get(name)
    if not value:
        raise EnvError(f"Missing required env var: {name}")
    return value


def get_optional_env(name, default=None):
    return os.environ.get(name, default)
