import os
import shutil
import time
from datetime import datetime
from logging.handlers import TimedRotatingFileHandler
from pathlib import Path
from typing import Any

import filelock
import isodate


class TIMRotatingLogFileHandler(TimedRotatingFileHandler):
    """
    Extended TimedRotatingFileHandler that tracks the last rotation time via an additional .log_timestamp file.
    """

    def __init__(self, filename: str, *args: Any, **kwargs: Any) -> None:
        self.log_timestamp_file = None
        _, ext = os.path.splitext(filename)
        super().__init__(filename, *args, **kwargs)
        base_dir = Path(filename).parent
        self.log_timestamp_file = base_dir / ".log_timestamp"
        t = self._get_log_time()
        self.rolloverAt = self.computeRollover(t)

    def _get_log_time(self) -> int:
        assert self.log_timestamp_file is not None
        if self.log_timestamp_file.exists():
            # noinspection PyBroadException
            try:
                return int(
                    isodate.parse_datetime(
                        self.log_timestamp_file.read_text().strip()
                    ).timestamp()
                )
            except Exception:
                pass
        return int(time.time())

    # noinspection PyPep8Naming
    def computeRollover(self, currentTime: int) -> int:
        result = super().computeRollover(currentTime)
        if self.log_timestamp_file:
            self.log_timestamp_file.write_text(
                isodate.datetime_isoformat(datetime.fromtimestamp(currentTime))
            )
        return result

    # noinspection PyPep8Naming
    def doRollover(self) -> None:
        """
        Modified rollover logic from Python to account for multiple workers writing to the same log file.
        """
        if self.stream:
            self.stream.close()
            self.stream = None  # type: ignore
        # get the time that this sequence started at and make it a TimeTuple
        currentTime = int(time.time())
        dstNow = time.localtime(currentTime)[-1]
        t = self.rolloverAt - self.interval
        if self.utc:
            timeTuple = time.gmtime(t)
        else:
            timeTuple = time.localtime(t)
            dstThen = timeTuple[-1]
            if dstNow != dstThen:
                if dstNow:
                    addend = 3600
                else:
                    addend = -3600
                timeTuple = time.localtime(t + addend)
        dfn = self.rotation_filename(
            self.baseFilename + "." + time.strftime(self.suffix, timeTuple)
        )
        # Ensure only one process can handle rotation at a time
        with filelock.FileLock("/tmp/tim_log_rotate.lock"):
            # Don't rotate if file already exists, it means it was already rotated
            if not os.path.exists(dfn):
                # Copy file so that current writing is not stopped
                shutil.copyfile(self.baseFilename, dfn)
                # Empty the current file without closing the streams
                open(self.baseFilename, "w").close()
                # Handle rotation normally
            if self.backupCount > 0:
                for s in self.getFilesToDelete():
                    os.remove(s)
        if not self.delay:
            self.stream = self._open()
        newRolloverAt = self.computeRollover(currentTime)
        while newRolloverAt <= currentTime:
            newRolloverAt = newRolloverAt + self.interval
        # If DST changes and midnight or weekly rollover, adjust for this.
        if (self.when == "MIDNIGHT" or self.when.startswith("W")) and not self.utc:
            dstAtRollover = time.localtime(newRolloverAt)[-1]
            if dstNow != dstAtRollover:
                if (
                    not dstNow
                ):  # DST kicks in before next rollover, so we need to deduct an hour
                    addend = -3600
                else:  # DST bows out before next rollover, so we need to add an hour
                    addend = 3600
                newRolloverAt += addend
        self.rolloverAt = newRolloverAt
