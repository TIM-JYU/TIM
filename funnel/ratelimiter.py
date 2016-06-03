class RateLimited:
    def __init__(self, f, window_size: float, calls_per_window: int, default_value=None):
        self.f = f
        self.default = default_value

        self.window_size = window_size
        self.calls_per_window = calls_per_window

        self.window_age = 0
        self.calls_remaining = calls_per_window

    def __call__(self, *args, **kwargs):
        if self.calls_remaining <= 0:
            return self.default

        self.calls_remaining -= 1
        return self.f(*args, **kwargs)

    def update(self, dt: float):
        self.window_age += dt
        while self.window_age >= self.window_size:
            self.window_age -= self.window_size
            self.calls_remaining = self.calls_per_window

