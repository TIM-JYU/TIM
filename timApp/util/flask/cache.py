from flask_caching import Cache

cache = Cache(config={
    'CACHE_TYPE': 'redis',
    'CACHE_DEFAULT_TIMEOUT': 3600,
    'CACHE_REDIS_HOST': 'redis',
    'CACHE_REDIS_DB': 1,  # Must be other than 0 because Celery uses db 0
})
