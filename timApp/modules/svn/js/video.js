// Workaround for lazy loading: the directive definition functions below are 
// not re-run when reloading the module
// so xxApp.sanitize 
if (angular.isDefined(window.videoApp)) {
    var videoAppSanitize = videoApp.sanitize;
}


var videoApp = angular.module('videoApp', ['ngSanitize']);
videoApp.directive('videoRunner',['$sanitize', function ($sanitize) {	timHelper.sanitize = $sanitize; videoApp.sanitize = $sanitize; return videoApp.directiveFunction('video'); }]);
videoApp.directive('smallVideoRunner',['$sanitize', function ($sanitize) {	timHelper.sanitize = $sanitize; videoApp.sanitize = $sanitize; return videoApp.directiveFunction('smallvideo'); }]);
videoApp.directive('listVideoRunner',['$sanitize', function ($sanitize) {
   timHelper.sanitize = $sanitize;
   videoApp.sanitize = $sanitize; return videoApp.directiveFunction('listvideo');
}]);

// redefine csApp.sanitize and csApp.compile if needed
if (angular.isDefined(window.videoAppSanitize) ) {
    videoApp.sanitize = videoAppSanitize;
}

var TESTWITHOUTPLUGINS = true && false;

videoApp.nr = 0;
 
videoApp.muunna = function(value) {
  if ( !value ) return value;
//  var s = "0 0 0 " + value.replace(/,/g," ").replace(/\//g," ").replace(/;/g," ").replace(/\./g," ").replace(/:/g," ");
//  var s = "0 0 0 " + value.replace(/s/g,"").value.replace(/[,\/;:\.hm]/g," ");
  var s = "0 0 0 " + value.replace(/s/g,"").replace(/[,\/;:.hm]/g," "); // loppu s unohdetaan muodosta 1h3m2s
  s = s.trim();
  var sc = s.split(" ");
  var n = sc.length;
  var h = sc[n-3];
  var m = sc[n-2];
  var ss = sc[n-1];
  return  h*3600.0 + m*60.0 + ss*1.0;
};


videoApp.directiveTemplateVideo = function(t) {
   var zoomVideo =  '<p ng-if="videoOn" class="pluginShow" >' +
		             '<a ng-click="zoom(0)" title="Reset to original size">[r]</a> ' +
		             '<a ng-click="zoom(1.0/1.4)" title="Zoom out">[-]</a> ' +
		             '<a ng-click="zoom(1.4)" title="Zoom in">[+]</a> ' +
	                 '<a ng-click="hideVideo()">{{hidetext}}</a>' +
	               '</p>';

   if ( t == "smallvideo" ) return '<div class="smallVideoRunDiv ng-cloak" ng-cloak>' +
				  '<p>Here comes header</p>' +
				  '<p>' +
                  '<span class="stem" ng-bind-html="stem"></span> ' +
                  '<a ng-if="videoname" class="videoname" ng-click="showVideo()">' +
                  '<span ng-if="videoicon"><img ng-src="{{videoicon}}" alt="Click here to show" /> </span>' +
                  '{{videoname}} {{duration}} {{span}}</a> ' +
                  '<a href="{{doclink}}" ng-if="doctext" target="timdoc"><span ng-if="docicon"><img ng-src="{{docicon}}"  alt="Go to doc" /> </span>' +
                  '{{doctext}}</a>'+
                  '</p>'+
				  '<div ><p></p></div>' + 
				  zoomVideo +
				  '<p class="plgfooter">Here comes footer</p>'+
				  '</div>';
   if ( t == "listvideo" ) return '<div class="listVideoRunDiv ng-cloak" ng-cloak>' +
				  '<p>Here comes header</p>' +
				  '<ul><li><span class="stem" ng-bind-html="stem"></span> '+
                  '<a ng-if="videoname" class="videoname" ng-click="showVideo()">'+
                  '<span ng-if="videoicon"><img ng-src="{{videoicon}}" alt="Click here to show" /> </span>' +
                  '{{videoname}}{{startt}} {{duration}} {{span}}</a> '+
                  '<a href="{{doclink}}" ng-if="doclink" target="timdoc"><span ng-if="docicon"><img ng-src="{{docicon}}"  alt="Go to doc" /> </span>' +
                  '{{doctext}}</a>'+
                  '</li></ul>' +
				  '<div ><p></p></div>' +
				  zoomVideo +
				  '<p class="plgfooter">Here comes footer</p>'+
				  '</div>';
   return '<div class="videoRunDiv ng-cloak" ng-cloak>' +
				  '<p>Here comes header</p>' +
				  '<p ng-if="stem" class="stem" ng-bind-html="stem"></p>' +
				  '<div ><p></p></div>' + 
				  //'<p ng-if="!videoOn" class="pluginHide"><a ng-click="showVideo()">Click here to show the video</a></p>' +
                  '<div class="no-popup-menu">' +
				  '<img src="/csimages/video.png" ng-if="!videoOn" ng-click="showVideo()" width="200" alt="Click here to show the video" />' +
                  '</div>' + 
                  '<a href="{{doclink}}" ng-if="doclink" target="timdoc"><span ng-if="docoicon"><img ng-src="{{docicon}}"  alt="Go to doc" /> </span>' +
                  '{{doctext}}</a>'+
				  zoomVideo +
				  '<p class="plgfooter">Here comes footer</p>'+
				  '</div>';
};

videoApp.time2String = function(t) {
  if ( !t ) return "";
  var h = Math.floor(t / 3600);
  t = (t - h*3600);
  var m = Math.floor(t/60);
  var s = (t - m*60);
  if ( !h ) h = ""; else h =h+"h";
  if ( !h && !m ) m = ""; else m =m+"m";
  s = s+"s";
  return h + m + s;
};


videoApp.isYoutube = function(file) {
    if ( !file ) return false;
    if ( file.indexOf("youtube") >= 0 ) return true;
    if ( file.indexOf("youtu.be") >= 0 ) return true;
    return false;
}

videoApp.directiveFunction = function(t) {
	return {
		link: function (scope, element, attrs) {
            timHelper.set(scope,attrs,".file");
            timHelper.set(scope,attrs,".width");
            timHelper.set(scope,attrs,".height");
            timHelper.set(scope,attrs,".videoname");
            timHelper.set(scope,attrs,".doctext");
            timHelper.set(scope,attrs,".doclink");
            timHelper.set(scope,attrs,".hidetext","hide video");
            timHelper.set(scope,attrs,".videoicon","/csimages/video_small.png");
            timHelper.set(scope,attrs,".docicon","/csimages/book.png");
			timHelper.set(scope,attrs,".open",false);
            if ( scope.videoicon == "False" ) scope.videoicon = "";
            if ( scope.docicon == "False" ) scope.docicon = "";
			scope.start = videoApp.muunna(scope.attrs.start);
			scope.end = videoApp.muunna(scope.attrs.end);
            scope.duration = videoApp.time2String(scope.end - scope.start);
            if ( scope.duration != "" ) scope.duration = "(" + scope.duration + ") ";
            scope.limits = "(" + videoApp.time2String(scope.start) + "-" + videoApp.time2String(scope.end) + ")";
            if ( scope.limits == "(-)" ) scope.limits = "";
            scope.span = "";
            scope.startt = videoApp.time2String(scope.start);
            if ( scope.startt ) scope.startt = ", " + scope.startt;
			if ( scope.attrs.stem ) scope.stem = scope.attrs.stem;
			if ( scope.attrs.iframe ) scope.iframe = true;
            if ( videoApp.isYoutube(scope.file) )  scope.iframe = true; // youtube must be in iframe
			scope.videoHtml = element[0].childNodes[2];
			var head = timHelper.getHeading(scope,attrs,".header","h4");
			element[0].childNodes[0].outerHTML = head;
			var n = element[0].childNodes.length;
			if ( n > 1 ) element[0].childNodes[n-1].outerHTML = timHelper.getHeading(scope,attrs,".footer",'p class="plgfooter"');
            scope.getPrevZoom();
            if ( scope.open ) scope.showVideo();
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
        template: videoApp.directiveTemplateVideo(t)
		// templateUrl: 'csTempl.html'
	}; 
};

videoApp.ifIs = function(value,name) {
	if ( !value ) return "";
	return name+'="'+value+'" ';
};
		
videoApp.Controller = function($scope,$http,$transclude) {
    $transclude(function(clone,scope) { timHelper.initAttributes(clone,$scope);  });
	$scope.header = "";
	$scope.rows = 5;
	$scope.errors = [];
	$scope.videoOn = false;
	
	$scope.runVideo = function() {
		// $scope.viewCode = false;
	};

	$scope.hideVideo = function() {
		$scope.videoOn = false;
		$scope.videoHtml.innerHTML = "<p></p>";
        $scope.span = "";
        return true;
	};

    $scope.getCurrentZoom = function() {
        if ( localStorage[$scope.origSize+".width"] )  $scope.width = localStorage[$scope.origSize+".width"];
        if ( localStorage[$scope.origSize+".height"] )  $scope.height = localStorage[$scope.origSize+".height"];
    }


	$scope.getPrevZoom = function () {
        var name = "z";

        if ( $scope.width ) name += $scope.width;
        $scope.origWidth = $scope.width;

        name += "x";

        if ( $scope.height ) name += $scope.height;
        $scope.origHeight = $scope.height;

        $scope.origSize = name;
        if ( typeof(localStorage) === "undefined" )  return;
    }


 	$scope.zoom = function(mult) {
        if ( mult == 0 ) {
            $scope.width = $scope.origWidth;
            $scope.height = $scope.origHeight;
            delete localStorage[$scope.origSize + ".width"];
            delete localStorage[$scope.origSize + ".height"];
        } else {
            if ($scope.width) {
                $scope.width *= mult;
                localStorage[$scope.origSize + ".width"] = "" + $scope.width;
            }
            if ($scope.height) {
                $scope.height *= mult;
                localStorage[$scope.origSize + ".height"] = "" + $scope.height;
            }
        }
        $scope.hideVideo();
        $scope.showVideo();
    }

	$scope.showVideo = function() {
        if ( $scope.videoOn ) return $scope.hideVideo();
        $scope.getCurrentZoom();

		videoApp.nr++;
        $scope.span = $scope.limits;
		var vid = 'vid'+videoApp.nr;
		var w = videoApp.ifIs($scope.width,"width");
		var h = videoApp.ifIs($scope.height,"height");
		var moniviestin = $scope.file.indexOf("m3.jyu.fi") >= 0;
		var t = "?";
		if ( $scope.start )
		    if ( moniviestin ) t = "#position="+$scope.start;
		    else t = "?start="+$scope.start+"&end="+$scope.end;
		if ( $scope.iframe ) {
            var file = $scope.file;
            if (  videoApp.isYoutube(file) && file.indexOf("embed") < 0 ) {
                var yname = "youtu.be/";  // could be also https://youtu.be/1OygRiwlAok
                var yembed = "//www.youtube.com/embed/"
                var iy = file.indexOf(yname);
                var parts = file.split("=");
                if ( parts.length > 1 )
                    file = yembed + parts[1];
                else if ( iy >= 0 ) file = yembed + file.substring(iy+yname.length);
            }
			$scope.videoHtml.innerHTML = '<iframe id="'+vid+'" class="showVideo" src="' + file + t +  '" ' + w + h + '  frameborder="0" allowfullscreen></iframe>';
			// '&rel=0'+
			// youtube: <iframe width="480" height="385" src="//www.youtube.com/embed/RwmU0O7hXts" frameborder="0" allowfullscreen></iframe>
		} else  { 
            t = "";
            if ( $scope.start ) {
                t = "#t="+$scope.start; // iPad ei tottele 'loadedmetadata'
                if ( $scope.end ) t += "," + $scope.end;
            }    
			$scope.videoHtml.innerHTML = '<video class="showVideo" id="'+vid+'" src="'+ $scope.file + t + '" type="video/mp4" controls autoplay="true" ' + w + h +'/>';
        }    
        // IE ei tietenkään taas tottele t-attribuuttia...            
		$scope.videoOn = true;	
		$scope.myvid = document.getElementById(vid);
		if ( !$scope.myvid ) return;
		$scope.myvid.addEventListener('loadedmetadata', function() {
			this.currentTime = $scope.start || 0;
			}, false);
			
        $scope.watchEnd = $scope.end;    
		$scope.myvid.addEventListener('timeupdate', function() {
			if ( this.currentTime > $scope.watchEnd ) {
				this.pause();
				$scope.watchEnd = 1000000;
			}
			}, false);
            
		/*
		$scope.videoHtml.addEventListener('loadedmetadata', function() {
			this.currentTime = 50;
			}, false);
		*/	
	}
};
