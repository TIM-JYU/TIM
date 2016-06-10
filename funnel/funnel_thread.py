import logging
import threading
import time

from typing import Optional


class Updatable:
    def update(self, dt: float):
        logging.getLogger().error('Method not implemented: {}.update'.format(self.__class__.__name__))


class FunnelThread (threading.Thread):
    INSTANCES = {}

    def __init__(self, thread_name: str, thread_obj: Updatable, update_interval: float = 0):
        threading.Thread.__init__(self)
        self.name = thread_name
        self.obj = thread_obj
        self.interval = update_interval
        self.INSTANCES[thread_name] = self
        self.running = False

    @classmethod
    def get(cls, thread_name: str) -> Optional['FunnelThread']:
        return cls.INSTANCES.get(thread_name)

    def run(self):
        if self.running:
            logging.getLogger().info("Tried to start {} thread but it's already running".format(self.name))
            return

        logging.getLogger().info('Starting {} thread'.format(self.name))
        self.running = True
        t0 = time.time()

        while self.running:
            t = time.time()
            self.obj.update(t - t0)
            time.sleep(self.interval)
            t0 = t

        logging.getLogger().info('Exiting {} thread'.format(self.name))

    def stop(self):
        self.running = False

