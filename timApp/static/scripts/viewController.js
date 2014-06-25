var controls = angular.module('controllers');

controls.controller('ViewCtrl', function($scope, $controller) {

	// inherit properties from ParCtrl
		$controller('ParCtrl', {
			$scope : $scope
		});

		// add view-specific functions here
		$scope.getDocumentAndNotes = function(docID) {
			$scope.getDocument(docID);
			
			//TODO: Get notes for each paragraph as follows:
			//1. Get all notes from server related to this document (and current user).
			//2. Group notes by paragraph so that each paragraph has "notes" attribute (and MAYBE hasNotes, but this can be deduced from the number of notes).
			//3. AngularJS will render them using the template automagically.
			  
			 
		};
	});