var controls = angular.module('controllers');

controls.controller('ViewCtrl', function($scope, $controller, $http) {

	// inherit properties from ParCtrl
		$controller('ParCtrl', {
			$scope : $scope
		});

		// add view-specific functions here
		$scope.getDocumentAndNotes = function(docID) {
			$scope.getDocument(docID);
			
			//TODO: Get notes for each paragraph as follows:
			//1. Get all notes from server related to this document (and current user).
			
			$scope.getNotes(docID);
			
			//2. Group notes by paragraph so that each paragraph has "notes" attribute.
			//3. AngularJS will render them using the template automagically.
			
			
			
			 
		};
		
		$scope.getNotes = function(docID) {
			var result;
            http.get('/notes/' + docID).
            success(function(data, status, headers, config) {
                     console.log(data);
            }).
            error(function(data, status, headers, config) {
                     return "[Could not fetch notes]";
            });
            return result;
		}
	});