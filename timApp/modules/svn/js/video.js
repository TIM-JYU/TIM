var videoApp = angular.module('videoApp', ['ngSanitize']);
videoApp.directive('videoRunner',['$sanitize', function ($sanitize) {	videoApp.sanitize = $sanitize; return videoApp.directiveFunction('video'); }]);


videoApp.nr = 0;
 
videoApp.getHeading = function(a,key,$scope,deftype) {
	if ( !a ) return "";
	var h = a[key];
	if ( !h ) return "";
	// if ( h.toLowerCase().indexOf("script") >= 0 ) return "";
	var st = h.split("!!"); // h4 class="h3" width="23"!!Tehtava 1
	var elem = deftype;
	var val = st[0];
	var attributes = "";
	if ( st.length >= 2 ) { elem = st[0]; val = st[1]; }
	var i = elem.indexOf(' ');
	ea = [elem];
	if ( i >= 0 ) ea = [elem.substring(0,i),elem.substring(i)];
	// var ea = elem.split(" ",2);
	if ( ea.length > 1 ) { elem = ea[0]; attributes = " " + ea[1] + " "; }
	// if ( elem.toLowerCase().indexOf("script") >= 0 ) return "";
	// attributes = "";  // ei laiteta näitä niin on vähän turvallisempi
    var html = "<" + elem + attributes + ">" + val + "</" + elem + ">";
	html = videoApp.sanitize(html);
	return html;
} 

videoApp.directiveFunction = function(t) {
	return {
		link: function (scope, element, attrs) {
			scope.file = attrs.file;
			scope.width = attrs.width;
			scope.height = attrs.height;
			scope.start = attrs.start;
			scope.end = attrs.end;
			scope.videoHtml = element[0].childNodes[1]
			head = videoApp.getHeading(attrs,"header",scope,"h4");
			element[0].childNodes[0].outerHTML = head;
			var n = element[0].childNodes.length;
			if ( n > 1 ) element[0].childNodes[n-1].outerHTML = videoApp.getHeading(attrs,"footer",scope,"p");
		},		
		scope: {},				
		controller: videoApp.Controller,
		restrict: 'AE',
		/*
		compile: function(tElement, attrs) {
			var content = tElement.children();
		},
		*/
		transclude: true,
		replace: 'true',
		template: '<div class="videoRunDiv">' +
				  '<p>Here comes header</p>' +
				  '<a ng-click="showVideo()">Click here to show the video</a>' +
				  '<p>Here comes footer</p>'+
				  '</div>'
		// templateUrl: 'csTempl.html'
	}; 
} 

videoApp.ifIs = function(value,name) {
	if ( !value ) return "";
	return name+'="'+value+'" ';
}
		
videoApp.Controller = function($scope,$http,$transclude) {
	$scope.byCode ="";
	$transclude(function(clone, scope) {
		if ( clone[0] )
			$scope.byCode = clone[0].textContent;
	});
	$scope.header = "";
	$scope.rows = 5;
	$scope.errors = [];
	
	$scope.runVideo = function() {
		// $scope.viewCode = false;
	};

	$scope.showVideo = function() {
		videoApp.nr++;
		var vid = 'vid'+videoApp.nr;
		var w = videoApp.ifIs($scope.width,"width");
		var h = videoApp.ifIs($scope.height,"height");
		$scope.videoHtml.outerHTML = '<video id="'+vid+'" src="'+ $scope.file + '" type="video/mp4" controls autoplay="true" ' + w + h +'/>';
		$scope.myvid = document.getElementById(vid);
		$scope.myvid.addEventListener('loadedmetadata', function() {
			this.currentTime = $scope.start || 0;
			}, false);
			
		$scope.myvid.addEventListener('timeupdate', function() {
			if ( this.currentTime > $scope.end ) {
				this.pause();
				$scope.end = 1000000;
			}
			}, false);
		/*
		$scope.videoHtml.addEventListener('loadedmetadata', function() {
			this.currentTime = 50;
			}, false);
		*/	
	}
};
