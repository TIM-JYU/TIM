@xcopy t:\tim\timApp\static\stylesheet.css e:\tim\timApp\static\ /d /Y
@pushd E:\tim\timApp\modules\cs
@cd t:\cs
@t:
@xcopy *.py e:    /d  /Y
@xcopy Dockerf*.* e:    /d  /Y
@xcopy js\*.js e:js\*.*    /d  /Y
@xcopy *.html e:  /d  /Y
@xcopy css\*.css e:css\*.*:   /d /s  /Y
@xcopy doxygen\*.* e:doxygen\*.*:   /d /s  /Y
@xcopy *.bat e:   /d  /Y
@xcopy *. e:      /d  /Y
@xcopy *.sh e:    /d  /Y
@xcopy r.* e:     /d  /Y
@xcopy dr*.* e:   /d  /Y
@xcopy  ptauno\index.html E:\kurssit\pauno\taulukko.html /d /Y
@xcopy  ptauno\taulukko.* E:\kurssit\pauno\  /d /Y
cd E:\tim\timApp\modules\cs\templates
cd templates
@xcopy *.* e:     /d /s  /Y
cd ..
@popd
