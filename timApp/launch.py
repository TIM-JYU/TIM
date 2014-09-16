import subprocess
import os
import tim

EPHEMERAL_PATH = os.path.join("..", "Ephemeral", "dist", "build", "Ephemeral")
LOG_PATH = os.path.join(EPHEMERAL_PATH, "log")

if not os.path.exists(LOG_PATH):
    os.mkdir(LOG_PATH)

subprocess.Popen([os.path.join(EPHEMERAL_PATH, "Ephemeral"), "-p", "8001"], cwd=EPHEMERAL_PATH)

tim.startApp()
