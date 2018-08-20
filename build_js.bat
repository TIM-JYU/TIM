@echo off
cd timApp\static\scripts && npm install && jspm install && npm run fixLibs && npm run build && tsc
