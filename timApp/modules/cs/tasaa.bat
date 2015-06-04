@xcopy t:\tim\timApp\static\stylesheet.css e:\kurssit\tim\timApp\static\ /d /Y
@pushd E:\kurssit\tim\timApp\modules\cs
@cd t:\cs
@t:
@kopioi *.py e:    /q /s
@kopioi Dockerf*.* e:    /q /s
@kopioi *.js e:    /q /s
@kopioi *.html e:  /q /s
@kopioi *.css e:   /q /s
@kopioi *.bat e:   /q
@kopioi *. e:      /q
@kopioi *.sh e:    /q /s
@kopioi r.* e:     /q
@kopioi dr*.* e:   /q /s
@xcopy  ptauno\index.html E:\kurssit\pauno\taulukko.html /d /Y
@xcopy  ptauno\taulukko.* E:\kurssit\pauno\  /d /Y
@popd
