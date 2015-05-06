timApp.controller("SidebarMenuCtrl", ['$scope', 

    function ($scope) {
		$scope.indexSidebarState = 'autohidden';
		$scope.lecturesSidebarState = 'autohidden';
		$scope.questionSidebarState = 'autohidden';
		$scope.peopleSidebarState = 'autohidden';
		$scope.settingsSidebarState = 'autohidden';

		$scope.toggleIndex = function () {
			var visible = angular.element('.index-sidebar').is(":visible");
			if (visible) {
				$scope.indexSidebarState = 'hidden';
			} else {
				$scope.indexSidebarState = 'open';
				$scope.lecturesSidebarState = 'hidden';
				$scope.questionSidebarState = 'hidden';
				$scope.peopleSidebarState = 'hidden';
				$scope.settingsSidebarState = 'hidden';
			}
		};
		
		$scope.toggleLectures = function () {
			var visible = angular.element('.lectures-sidebar').is(":visible");
			if (visible) {
				$scope.lecturesSidebarState = 'hidden';
			} else {
				$scope.indexSidebarState = 'hidden';
				$scope.lecturesSidebarState = 'open';
				$scope.questionSidebarState = 'hidden';
				$scope.peopleSidebarState = 'hidden';
				$scope.settingsSidebarState = 'hidden';
			}
		};
		
		$scope.toggleQuestions = function () {
			var visible = angular.element('.questions-sidebar').is(":visible");
			if (visible) {
				$scope.questionSidebarState = 'hidden';
			} else {
				$scope.indexSidebarState = 'hidden';
				$scope.lecturesSidebarState = 'hidden';
				$scope.questionSidebarState = 'open';
				$scope.peopleSidebarState = 'hidden';
				$scope.settingsSidebarState = 'hidden';
			}
		};
		
		$scope.togglePeople = function () {
			var visible = angular.element('.people-sidebar').is(":visible");
			if (visible) {
				$scope.peopleSidebarState = 'hidden';
			} else {
				$scope.indexSidebarState = 'hidden';
				$scope.lecturesSidebarState = 'hidden';
				$scope.questionSidebarState = 'hidden';
				$scope.peopleSidebarState = 'open';
				$scope.settingsSidebarState = 'hidden';
			}
		};
		
		$scope.toggleSettings = function () {
			var visible = angular.element('.settings-sidebar').is(":visible");
			if (visible) {
				$scope.settingsSidebarState = 'hidden';
			} else {
				$scope.indexSidebarState = 'hidden';
				$scope.lecturesSidebarState = 'hidden';
				$scope.questionSidebarState = 'hidden';
				$scope.peopleSidebarState = 'hidden';
				$scope.settingsSidebarState = 'open';
			}
		};
		
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