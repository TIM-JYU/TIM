var controls = angular.module('controller');

controls.controller('ViewCtrl', function($scope, $controller, $http) {

    // inherit properties from ParCtrl
    $controller('ParCtrl', {
        $scope : $scope
    });

    $scope.getNotes = function() {
        $http.get('/notes/' + $scope.docId).success(function(data, status, headers, config) {
            var len = $scope.paragraphs.length;
            var noteCount = data.length;

            for (var i = 0; i < len; i++) {
                $scope.paragraphs[i].notes = [];
            }

            for (var i = 0; i < noteCount; i++) {
                if (data[i].specifier < $scope.paragraphs.length) {
                    var par = $scope.paragraphs[data[i].specifier];
                    par.notes.push(data[i]);
                    $scope.$apply();
                    MathJax.Hub.Queue(["Typeset", MathJax.Hub, "par-" + data[i].specifier]);
                }
            }
        }).error(function(data, status, headers, config) {
            alert("Could not fetch notes.");
        });
    };

    $scope.getNotes();
});