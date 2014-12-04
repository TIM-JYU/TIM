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

	$scope.totext = function(str) {
		if (str.indexOf('{') > 0) {
			return str.substring(0, str.indexOf('{')).trim();
		}
		return str;
	}
	
	$scope.tolink = function(str) {
		if (str.indexOf('{') >= 0 && str.indexOf('}') > 0) {
			ob = str.indexOf('{') 
			cb = str.indexOf('}')
			return str.substring(ob + 1, cb);
		}
		return "#" + str.replace(/^(\d)+(\.\d+)*\.? /, "").replace(/[^\d\wåäö\.\- ]/g, "").trim().replace(/ +/g, '-').toLowerCase()
	}

    $scope.getIndex = function() {
        $http.get('/index/' + $scope.docId).success(function(data, status, headers, config) {
            var entryCount = data.length;
            $scope.indexTable = [];
            parent = null;

            for (var i = 0; i < entryCount; i++) {
            	if (data[i].charAt(0) != '#') {
            		// Not a heading
            		continue;
            	}
            	if (data[i].charAt(1) != '#') {
            		// Level 1 heading
            		astyle = "a1";
            		txt = data[i].substr(1);
            		lvl = 1;
            	}
            	else if (data[i].length > 2 && data[i].charAt(2) != '#') {
            		// Level 2 heading
            		astyle = "a2";
            		txt = data[i].substr(2);
            		lvl = 2;
            	}
            	else {
            		// Ignore after this level
            		continue;
            	}
            	
            	txt = txt.trim().replace(/\\#/g, "#")

            	item = {text: $scope.totext(txt), target: $scope.tolink(txt), style: astyle, level: lvl, items: [], state: ''};

                if (lvl == 1) {
                    if (parent != null) {
                        if (parent.items.length > 0)
                            parent.state = 'col';
                        $scope.indexTable.push(parent);
                    }

                    parent = item;
                }
                else if (parent != null) {
                    parent['items'].push(item)
                }
            }

            if (parent != null) {
                if (parent.items.length > 0)
                    parent.state = 'col';
                $scope.indexTable.push(parent);
            }

            $scope.$apply();
        }).error(function(data, status, headers, config) {
            alert("Could not fetch index entries.");
        });
    };

    $scope.invertState = function(state) {
        if (state == 'exp')
            return 'col';
        if (state == 'col')
            return 'exp';
        return state;
    };

    $scope.clearSelection = function() {
        if (document.selection)
            document.selection.empty();
        else if (window.getSelection)
            window.getSelection().removeAllRanges();
    };

    $scope.invertStateClearSelection = function(event, state) {
        if (event.target.className == 'a2') {
            // Do not collapse/expand if a subentry is clicked
            return state;
        }

        newState = $scope.invertState(state);
        if (newState != state)
            $scope.clearSelection();
        return newState;
    }

    $scope.indexTable = [];

    $scope.getIndex();
    $scope.getNotes();
    $scope.getReadPars();
});
