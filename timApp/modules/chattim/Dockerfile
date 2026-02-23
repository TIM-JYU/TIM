FROM python:3.11-slim AS base

RUN mkdir -p /tmp

COPY requirements.txt /tmp/requirements.txt

RUN python -m pip install -U pip setuptools \
    && python -m pip install --no-cache-dir -r /tmp/requirements.txt

EXPOSE 5000
EXPOSE 5001
