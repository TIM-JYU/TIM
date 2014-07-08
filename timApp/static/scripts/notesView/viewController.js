var controls = angular.module('controllers');

controls.controller('ViewCtrl', function($scope, $controller, $http) {

    // inherit properties from ParCtrl
        $controller('ParCtrl', {
            $scope : $scope
        });

        // add view-specific functions here
        $scope.getDocumentAndNotes = function(docID) {
            $scope.docID = docID;
            $scope.getDocument(docID);
            $scope.getNotes();
        };

        $scope.getNotes = function() {
            $http.get('/notes/' + $scope.docID).success(
                    function(data, status, headers, config) {
                        var len = $scope.paragraphs.length;
                        $scope.pars = [];
                        var noteCount = data.length;
                        for ( var i = 0; i < len; i++) {
                            var par = {};
                            par.html = $scope.paragraphs[i];
                            par.notes = [];

                            // TODO: This is not efficient; the server should
                            // probably group notes by paragraph
                            for ( var j = 0; j < noteCount; j++) {
                                if (data[j].specifier === i) {
                                    par.notes.push(data[j]);
                                }
                            }
                            $scope.pars.push(par);
                        }
                    }).error(function(data, status, headers, config) {
                alert("Could not fetch notes.");
            });
        }
    });