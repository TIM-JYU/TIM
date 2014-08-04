#!/bin/bash

cat << EOF
<!DOCTYPE html> 
 <html lang="en"> 
 <head> <meta charset="utf-8"> 
             <title>HTML5 boilerplate – all you really need…</title> 
             <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.17/angular.min.js"></script>
             <script src="script.js"></script>
</head> 

 <body id="home" ng-app="MCQ"> 
 <h1>Test</h1> 
 <div data-plugin="http://localhost:8080"
      data-state="null"
      data-markup='{"stem":"Valitse kissa","ident":"ekatehtävä","choices":[{"reason":"Piti valita kissa","text":"Koira","correct":false},{"reason":"Kissat Rulez","text":"Kissa","correct":true},{"reason":"Kissa voi syödä kanin","text":"Kani","correct":false}]}'>
  `curl -X POST --data-binary @$1 localhost:8080/html`
 </div> 
 </body> 
</html>
EOF
