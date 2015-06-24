/**
 * FILL WITH SUITABLE TEXT
 * @module smallMenuCtrl
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

timApp.controller("SmallMenuCtrl", ['$scope', '$window', '$http',
    function ($scope, $window, $http) {
		$scope.currentLecturesList = [];
        $scope.futureLecturesList = [];

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:smallMenuCtrl
         */
        var ready = function() {
			$('#currentList').hide();
			$('#futureList').hide();
		};

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:smallMenuCtrl
         */
		$scope.openCurrentLectureMenu = function() {
			$('#currentList').slideToggle();
			$('#futureList').hide();
			$('.menu').hide();
			
			 $http({
                    url: '/getAllLecturesFromDocument',
                    method: 'GET',
                    params: {'doc_id': $scope.docId, 'buster': Date.now()}
                })
                    .success(function (lectures) {
                        $scope.currentLecturesList = lectures.currentLectures;

                    })
                    .error(function () {
                        console.log("Couldn't fetch the lectures");
                    })
			
		};

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:smallMenuCtrl
         */
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

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:smallMenuCtrl
         */
		$scope.selectCurrentLecture = function() {

		};
		
		var w = angular.element($window);
		w.bind('resize', function () {
			$('#currentList').hide();
			$('#futureList').hide();
		});
    }
]);



