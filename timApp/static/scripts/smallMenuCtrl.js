timApp.controller("SmallMenuCtrl", ['$scope', '$window',
    function ($scope, $window) {
		
		var ready = function() {
			$('#currentList').hide();
			$('#futureList').hide();
		}
		
		$scope.openCurrentLectureMenu = function() {
			$('#currentList').slideToggle();
			$('#futureList').hide();
		};
		
		$scope.openFutureLectureMenu = function() {
			$('#futureList').slideToggle();
			$('#currentList').hide();
		};
		
		var w = angular.element($window);
		w.bind('resize', function () {
			$('#currentList').hide();
			$('#futureList').hide();
		});
    }
]);



