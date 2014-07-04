var controls = angular.module('controllers');

controls.controller('ViewCtrl', function($scope, $controller, $http) {

	// inherit properties from ParCtrl
		$controller('ParCtrl', {
			$scope : $scope
		});

		// add view-specific functions here
		$scope.getDocumentAndNotes = function(docID) {
			$scope.getDocument(docID);
			console.log('got document');
			// TODO: Get notes for each paragraph as follows:
			// 1. Get all notes from server related to this document (and
			// current user).

			$scope.getNotes(docID);
			$scope.showNoteEditor = false;
			console.log('got notes');
			// 2. Group notes by paragraph so that each paragraph has "notes"
			// attribute.
			// 3. AngularJS will render them using the template automagically.

		};

		$scope.getNotes = function(docID) {
			$http.get('/notes/' + docID).success(
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
		
		$scope.openNoteEditor = function(parIndex) {
			
		}
	});