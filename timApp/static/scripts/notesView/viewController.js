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

    $scope.getNotes();
    $scope.getReadPars();
});