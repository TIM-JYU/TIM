timApp.controller("SidebarMenuCtrl", ['$scope',
function (sc) {
	sc.sidebarState = 'autohidden';
	
	var indexChosen = false;
	var lecturesChosen = false;
	var questionChosen = false;
	var peopleChosen = false;
	var settingsChosen = false;

	sc.toggleSidebar = function () {
		var visible = angular.element('.index-sidebar').is(":visible");
		if (visible) {
			sc.sidebarState = 'hidden';
		} else {
			sc.sidebarState = 'open';
		}
    };

    sc.autoHideSidebar = function () {
        if (sc.sidebarState === 'open') {
            sc.sidebarState = 'autohidden';
        }
    };
	
 }]);



