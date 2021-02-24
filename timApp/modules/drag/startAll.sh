#!/usr/bin/env bash
gunicorn --config python:tim_common.plugin_gunicorn drag.drag_main:app
