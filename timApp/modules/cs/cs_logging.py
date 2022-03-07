import logging
from logging.handlers import RotatingFileHandler

_logger = None


def get_logger():
    global _logger
    if _logger:
        return _logger
    # Create a rotating logger in /cs_logs
    _logger = logging.getLogger("csplugin")
    _logger.setLevel(logging.DEBUG)

    formatter = logging.Formatter(
        "[%(asctime)s: %(levelname)s] %(user_id)s: %(message)s - UA: %(useragent)s"
    )
    # Max size is 100MB
    max_size = 100 * 1024 * 1024
    handler = RotatingFileHandler("/logs/cs_log.log", maxBytes=max_size, backupCount=5)
    handler.setFormatter(formatter)
    _logger.addHandler(handler)
    return _logger


def log_warning(message: str, user_id: str = "--", useragent: str = ""):
    get_logger().warning(message, extra={"user_id": user_id, "useragent": useragent})
