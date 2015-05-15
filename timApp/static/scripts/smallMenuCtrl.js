timApp.controller("SmallMenuCtrl", ['$scope', '$window', '$http',
    function ($scope, $window, $http) {
		$scope.currentLecturesList = [];
        $scope.futureLecturesList = [];
		
		var ready = function() {
			$('#currentList').hide();
			$('#futureList').hide();
		}
		
		$scope.openCurrentLectureMenu = function() {
			$('#currentList').slideToggle();
			$('#futureList').hide();
			$('.menu').hide();
			
			 $http({
                    url: '/getAllLecturesFromDocument',
                    method: 'GET',
                    params: {'doc_id': $scope.docId}
                })
                    .success(function (lectures) {
                        $scope.currentLecturesList = lectures.currentLectures;

                    })
                    .error(function () {
                        console.log("Couldn't fetch the lectures");
                    })
			
		};
		
		$scope.openFutureLectureMenu = function() {
			$('#futureList').slideToggle();
			$('#currentList').hide();
			$('.menu').hide();
			
			 $http({
                    url: '/getAllLecturesFromDocument',
                    method: 'GET',
                    params: {'doc_id': $scope.docId}
                })
                    .success(function (lectures) {
                        $scope.futureLecturesList = lectures.futureLectures;
                    })
                    .error(function () {
                        console.log("Couldn't fetch the lectures");
                    })
			
		};
		
		$scope.selectCurrentLecture = function() {
			
			
		}
		
		var w = angular.element($window);
		w.bind('resize', function () {
			$('#currentList').hide();
			$('#futureList').hide();
		});
    }
]);



