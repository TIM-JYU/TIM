var controls = angular.module('controller');

controls.controller('ViewCtrl', function($scope, $controller, $http) {

    // inherit properties from ParCtrl
        $controller('ParCtrl', {
            $scope : $scope
        });

        $scope.getNotes = function() {
            $http.get('/notes/' + $scope.docId).success(
                    function(data, status, headers, config) {
                        var len = $scope.paragraphs.length;
                        $scope.pars = [];
                        var noteCount = data.length;
                        for ( var i = 0; i < len; i++) {
                            var par = $scope.paragraphs[i];
                            par.notes = [];

                            // TODO: This is not efficient; the server should
                            // probably group notes by paragraph
                            for ( var j = 0; j < noteCount; j++) {
                                if (data[j].specifier === i) {
                                    par.notes.push(data[j]);
                                }
                            }
                        }
                    }).error(function(data, status, headers, config) {
                alert("Could not fetch notes.");
            });
        };
        
        $scope.getNotes();
    });