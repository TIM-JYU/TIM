@echo off

setlocal EnableDelayedExpansion

set VBOX_MOUNT=c:\users
set REL_PATH=%~dp0..
set ABS_PATH=

pushd %REL_PATH%
set ABS_PATH=%CD%
popd

set TIM_REL_PATH=!ABS_PATH:*%VBOX_MOUNT%\=!
set TIM_REL_PATH_LINUX=%TIM_REL_PATH:\=/%
set TIM_ABS_PATH_LINUX=/c/Users/%TIM_REL_PATH_LINUX%

(echo cd %TIM_ABS_PATH_LINUX% & echo ./docker-compose.sh up -d & echo exit) | docker-machine ssh default

rem This style works too, but the output is narrower for some reason.
rem docker-machine ssh default "cd %TIM_ABS_PATH_LINUX% && ./docker-compose.sh"

pause
