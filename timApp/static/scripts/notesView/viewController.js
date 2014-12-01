var controls = angular.module('controller');

controls.controller('ViewCtrl', function($scope, $controller, $http) {

    // inherit properties from ParCtrl
    $controller('ParCtrl', {
        $scope : $scope
    });

    $scope.getNotes = function() {
        var rn = "?rand=" + Math.floor(Math.random()*100001);
        $http.get('/notes/' + $scope.docId + rn).success(function(data, status, headers, config) {
            var len = $scope.paragraphs.length;
            var noteCount = data.length;

            for (var i = 0; i < len; i++) {
                $scope.paragraphs[i].notes = [];
            }

            for (var i = 0; i < noteCount; i++) {
                if (data[i].par_index < $scope.paragraphs.length) {
                    var par = $scope.paragraphs[data[i].par_index];
                    par.notes.push(data[i]);
                    $scope.$apply();
                    MathJax.Hub.Queue(["Typeset", MathJax.Hub, "par-" + data[i].par_index]);
                }
            }
        }).error(function(data, status, headers, config) {
            alert("Could not fetch notes.");
        });
    };
    
    $scope.getReadPars = function() {
        var rn = "?rand=" + Math.floor(Math.random()*100001);
        $http.get('/read/' + $scope.docId+rn).success(function(data, status, headers, config) {
            var len = $scope.paragraphs.length;
            var readCount = data.length;
 
            for (var i = 0; i < len; i++) {
                $scope.paragraphs[i].readStatus = "unread";
            }
 // alert(readCount);
            for (var i = 0; i < readCount; i++) {
                var readPar = data[i];
                if (readPar.par_index < $scope.paragraphs.length) {
                    var par = $scope.paragraphs[readPar.par_index];
                    par.readStatus = readPar.status;
                }
            }
        }).error(function(data, status, headers, config) {
            alert("Could not fetch reading info.");
        });
    };
	
	$scope.tolink = function(str) {
		return "#" + str.replace(/^(\d)+(\.\d+)*\.? /, "").replace(/[\?\#\\]/g, "").replace(/ /g, '-').toLowerCase()
	}

    $scope.getIndex = function() {
        $http.get('/index/' + $scope.docId).success(function(data, status, headers, config) {
            var entryCount = data.length;
            $scope.indexTable = [];

            for (var i = 0; i < entryCount; i++) {
            	if (data[i].charAt(0) != '#') {
            		// Not a heading
            		continue;
            	}
            	if (data[i].charAt(1) != '#') {
            		// Level 1 heading
            		astyle = "a1";
            		txt = data[i].substr(1).trim();
            	}
            	else if (data[i].length > 2 && data[i].charAt(2) != '#') {
            		// Level 2 heading
            		astyle = "a2";
            		txt = data[i].substr(2).trim();
            	}
            	else {
            		// Ignore after this level
            		continue;
            	}
            	
                $scope.indexTable.push({text: txt, target: $scope.tolink(txt), style: astyle});
            }

            $scope.$apply();
        }).error(function(data, status, headers, config) {
            alert("Could not fetch index entries.");
        });
    };

    $scope.indexTable = [];

    $scope.getIndex();
    $scope.getNotes();
    $scope.getReadPars();
});