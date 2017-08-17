@echo off

for /f %%i in ('docker-machine ip default') do set IP=%%i

start "" http://%IP%
