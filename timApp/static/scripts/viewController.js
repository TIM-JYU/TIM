var controls = angular.module('controllers');

controls.controller('ViewCtrl', function($scope, $controller) {
	
	//inherit properties from ParCtrl
	$controller('ParCtrl', {
		$scope : $scope
	});

	//add view-specific functions here
	
});