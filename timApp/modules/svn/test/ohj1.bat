:curl -H "Content-Type: application/json" --data-binary @ohj1Vid.json http://localhost:5000/video/multihtml >nul
:curl -H "Content-Type: application/json" --data-binary @ohj1Img.json http://localhost:5000/image/multihtml >nul
curl -H "Content-Type: application/json" --data-binary @ohj1Vid.json http://tim3/svn/video/multihtml >nul
curl -H "Content-Type: application/json" --data-binary @ohj1Img.json http://tim3/svn/image/multihtml >nul