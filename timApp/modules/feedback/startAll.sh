#!/usr/bin/env bash
gunicorn --config python:tim_common.plugin_gunicorn feedback.feedback_main:app
