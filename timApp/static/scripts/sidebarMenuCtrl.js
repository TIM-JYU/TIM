/**
 * FILL WITH SUITABLE TEXT
 * @module sidebarMenuCtrl
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

timApp.controller("SidebarMenuCtrl", ['$scope', "$http", "$window",

    function ($scope, $http, $window) {
        $scope.currentLecturesList = [];
        $scope.futureLecturesList = [];
        $scope.pastLecturesList = [];
        $scope.lectureQuestions = [];
        $scope.materialQuestions = [];
        $scope.indexSidebarState = 'autohidden';
        $scope.lecturesSidebarState = 'autohidden';
        $scope.questionSidebarState = 'autohidden';
        $scope.peopleSidebarState = 'autohidden';
        $scope.settingsSidebarState = 'autohidden';
		$scope.indexIconState = 'noClick';
		$scope.lectureIconState = 'noClick';
		$scope.questionIconState = 'noClick';
		$scope.peopleIconState = 'noClick';
		$scope.settingsIconState = 'noClick';

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:sidebarMenuCtrl
         */
        $scope.showSidebar = function () {
            $('.menu').slideToggle();
            $('.sideMenu').slideToggle();
            $('#futureList').hide();
            $('#currentList').hide();
            $scope.indexSidebarState = 'hidden';
            $scope.lecturesSidebarState = 'hidden';
            $scope.questionSidebarState = 'hidden';
            $scope.peopleSidebarState = 'hidden';
            $scope.settingsSidebarState = 'hidden';
        };

        var w = angular.element($window);

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:sidebarMenuCtrl
         */
        w.bind('resize', function () {
            $scope.indexSidebarState = 'hidden';
            $scope.lecturesSidebarState = 'hidden';
            $scope.questionSidebarState = 'hidden';
            $scope.peopleSidebarState = 'hidden';
            $scope.settingsSidebarState = 'hidden';
			$scope.indexIconState = 'noClick';
			$scope.lectureIconState = 'noClick';
			$scope.questionIconState = 'noClick';
			$scope.peopleIconState = 'noClick';
			$scope.settingsIconState = 'noClick';
        });

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:sidebarMenuCtrl
         */
        $scope.toggleIndex = function () {
            var visible = angular.element('.index-sidebar').is(":visible");
            if (visible) {
                $scope.indexSidebarState = 'hidden';
				$scope.indexIconState = 'noClick';
            } else {
                $scope.indexSidebarState = 'open';
                $scope.lecturesSidebarState = 'hidden';
                $scope.questionSidebarState = 'hidden';
                $scope.peopleSidebarState = 'hidden';
                $scope.settingsSidebarState = 'hidden';
				$scope.indexIconState = 'clicked';
				$scope.lectureIconState = 'noClick';
				$scope.questionIconState = 'noClick';
				$scope.peopleIconState = 'noClick';
				$scope.settingsIconState = 'noClick';
            }
        };

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:sidebarMenuCtrl
         */
        $scope.toggleLectures = function () {
            var visible = angular.element('.lectures-sidebar').is(":visible");
            if (visible) {
                $scope.lecturesSidebarState = 'hidden';
				$scope.lectureIconState = 'noClick';

            } else {
                $scope.indexSidebarState = 'hidden';
                $scope.lecturesSidebarState = 'open';
                $scope.questionSidebarState = 'hidden';
                $scope.peopleSidebarState = 'hidden';
                $scope.settingsSidebarState = 'hidden';
				$scope.indexIconState = 'noClick';
				$scope.lectureIconState = 'clicked';
				$scope.questionIconState = 'noClick';
				$scope.peopleIconState = 'noClick';
				$scope.settingsIconState = 'noClick';

                $http({
                    url: '/getAllLecturesFromDocument',
                    method: 'GET',
                    params: {'doc_id': $scope.docId}
                })
                    .success(function (lectures) {
                        $scope.currentLecturesList = lectures.currentLectures;
                        $scope.futureLecturesList = lectures.futureLectures;
                        $scope.pastLecturesList = lectures.pastLectures;
                    })
                    .error(function () {
                        console.log("Couldn't fetch the lectures");
                    })


            }
        };

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:sidebarMenuCtrl
         */
        $scope.toggleQuestions = function () {
            var visible = angular.element('.questions-sidebar').is(":visible");

            $scope.lectureQuestions = [];
            if (visible) {
                $scope.questionSidebarState = 'hidden';
				$scope.questionIconState = 'noClick';

            } else {
                $scope.indexSidebarState = 'hidden';
                $scope.lecturesSidebarState = 'hidden';
                $scope.questionSidebarState = 'open';
                $scope.peopleSidebarState = 'hidden';
                $scope.settingsSidebarState = 'hidden';
				$scope.indexIconState = 'noClick';
				$scope.lectureIconState = 'noClick';
				$scope.questionIconState = 'clicked';
				$scope.peopleIconState = 'noClick';
				$scope.settingsIconState = 'noClick';

                $http({
                    url: '/questions/' + $scope.docId,
                    method: 'GET'
                })
                    .success(function (questions) {
                        for (var i = 0; i < questions.length; i++) {
                            var question = {
                                "questionId": questions[i].question_id,
                                "questionTitle": (JSON.parse(questions[i].questionJson)).TITLE
                            };
                            $scope.lectureQuestions.push(question);
                        }
                    })
                    .error(function () {
                        console.log("Couldn't fetch the questions");
                    })
            }
        };

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:sidebarMenuCtrl
         */
        $scope.togglePeople = function () {
            var visible = angular.element('.people-sidebar').is(":visible");
            if (visible) {
                $scope.peopleSidebarState = 'hidden';
				$scope.peopleIconState = 'noClick';

            } else {
                $scope.indexSidebarState = 'hidden';
                $scope.lecturesSidebarState = 'hidden';
                $scope.questionSidebarState = 'hidden';
                $scope.peopleSidebarState = 'open';
                $scope.settingsSidebarState = 'hidden';
				$scope.indexIconState = 'noClick';
				$scope.lectureIconState = 'noClick';
				$scope.questionIconState = 'noClick';
				$scope.peopleIconState = 'clicked';
				$scope.settingsIconState = 'noClick';
            }
        };

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:sidebarMenuCtrl
         */
        $scope.toggleSettings = function () {
            var visible = angular.element('.settings-sidebar').is(":visible");
            if (visible) {
                $scope.settingsSidebarState = 'hidden';
				$scope.settingsIconState = 'noClick';
            } else {
                $scope.indexSidebarState = 'hidden';
                $scope.lecturesSidebarState = 'hidden';
                $scope.questionSidebarState = 'hidden';
                $scope.peopleSidebarState = 'hidden';
                $scope.settingsSidebarState = 'open';
				$scope.indexIconState = 'noClick';
				$scope.lectureIconState = 'noClick';
				$scope.questionIconState = 'noClick';
				$scope.peopleIconState = 'noClick';
				$scope.settingsIconState = 'clicked';
            }
        };

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:sidebarMenuCtrl
         */
        $scope.autoHideSidebar = function () {
            if ($scope.indexSidebarState === 'open') {
                $scope.indexSidebarState = 'autohidden';
            }
            if ($scope.lecturesSidebarState === 'open') {
                $scope.lecturesSidebarState = 'autohidden';
            }
            if ($scope.questionSidebarState === 'open') {
                $scope.questionSidebarState = 'autohidden';
            }
            if ($scope.peopleSidebarState === 'open') {
                $scope.peopleSidebarState = 'autohidden';
            }
            if ($scope.settingsSidebarState === 'open') {
                $scope.settingsSidebarState = 'autohidden';
            }
        };
    }
])
;