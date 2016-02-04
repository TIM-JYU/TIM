from flask.ext.cache import Cache

cache = Cache(config={'CACHE_TYPE': 'filesystem',
                      'CACHE_DIR': '/tmp/tim_cache',
                      'CACHE_DEFAULT_TIMEOUT': 3600})
