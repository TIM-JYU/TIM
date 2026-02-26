#!/usr/bin/env bash
gunicorn --config python:tim_common.plugin_gunicorn chattim.chattim_main:app
