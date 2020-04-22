#!/usr/bin/env bash
gunicorn --config /py/plugin_gunicorn.py drag:app
