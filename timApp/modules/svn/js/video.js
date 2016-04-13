// Workaround for lazy loading: the directive definition functions below are 
// not re-run when reloading the module
// so xxApp.sanitize 
if (angular.isDefined(window.videoApp)) {
    var videoAppSanitize = videoApp.sanitize;
}


var videoApp = angular.module('videoApp', ['ngSanitize']);
videoApp.directive('videoRunner',['$sanitize', function ($sanitize) {	videoApp.sanitize = $sanitize; return videoApp.directiveFunction('video'); }]);
videoApp.directive('smallVideoRunner',['$sanitize', function ($sanitize) {	videoApp.sanitize = $sanitize; return videoApp.directiveFunction('smallvideo'); }]);
videoApp.directive('listVideoRunner',['$sanitize', function ($sanitize) {
	videoApp.sanitize = $sanitize; return videoApp.directiveFunction('listvideo');
}]);

// redefine csApp.sanitize and csApp.compile if needed
if (angular.isDefined(window.videoAppSanitize) ) {
    videoApp.sanitize = videoAppSanitize;
}

  
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
	try {
	  val = decodeURIComponent(escape(val))
	} catch(err) {}
    var html = "<" + elem + attributes + ">" + val + "</" + elem + ">";
	html = videoApp.sanitize(html);
	return html;
} 

videoApp.muunna = function(value) {
  if ( !value ) return value;
//  var s = "0 0 0 " + value.replace(/,/g," ").replace(/\//g," ").replace(/;/g," ").replace(/\./g," ").replace(/:/g," ");
//  var s = "0 0 0 " + value.replace(/s/g,"").value.replace(/[,\/;:\.hm]/g," ");
  var s = "0 0 0 " + value.replace(/s/g,"").replace(/[,\/;:\.hm]/g," "); // loppu s unohdetaan muodosta 1h3m2s
  s = s.trim();
  var sc = s.split(" ");
  var n = sc.length;
  var h = sc[n-3];
  var m = sc[n-2];
  var s = sc[n-1];
  return  h*3600.0 + m*60.0 + s*1.0;
}


videoApp.directiveTemplateVideo = function(t) {
   if ( t == "smallvideo" ) return '<div class="smallVideoRunDiv ng-cloak" ng-cloak>' +
				  '<p>Here comes header</p>' +
				  '<p>' +
                  '{{stem}} ' + 
                  '<a ng-if="videoname" class="videoname" ng-click="showVideo()">' +
                  '<span ng-if="videoicon"><img ng-src="{{videoicon}}" alt="Click here to show" /> </span>' +
                  '{{videoname}} {{duration}} {{span}}</a> ' +
                  '<a href="{{doclink}}" ng-if="doctext" target="timdoc"><span ng-if="docicon"><img ng-src="{{docicon}}"  alt="Go to doc" /> </span>' +
                  '{{doctext}}</a>'+
                  '</p>'+
				  '<div ><p></p></div>' + 
				  '<p ng-if="videoOn" class="pluginShow" ><a ng-click="hideVideo()">{{hidetext}}</a></p>'+
				  '<p class="plgfooter">Here comes footer</p>'+
				  '</div>';
   if ( t == "listvideo" ) return '<div class="listVideoRunDiv ng-cloak" ng-cloak>' +
				  '<p>Here comes header</p>' +
				  '<ul><li>{{stem}} '+
                  '<a ng-if="videoname" class="videoname" ng-click="showVideo()">'+
                  '<span ng-if="videoicon"><img ng-src="{{videoicon}}" alt="Click here to show" /> </span>' +
                  '{{videoname}}{{startt}} {{duration}} {{span}}</a> '+
                  '<a href="{{doclink}}" ng-if="doclink" target="timdoc"><span ng-if="docicon"><img ng-src="{{docicon}}"  alt="Go to doc" /> </span>' +
                  '{{doctext}}</a>'+
                  '</li></ul>' +
				  '<div ><p></p></div>' + 
				  '<p ng-if="videoOn" class="pluginShow" ><a ng-click="hideVideo()">{{hidetext}}</a></p>'+
				  '<p class="plgfooter">Here comes footer</p>'+
				  '</div>';
   return '<div class="videoRunDiv ng-cloak" ng-cloak>' +
				  '<p>Here comes header</p>' +
				  '<p ng-if="stem" class="stem" >{{stem}}</p>' +
				  '<div ><p></p></div>' + 
				  //'<p ng-if="!videoOn" class="pluginHide"><a ng-click="showVideo()">Click here to show the video</a></p>' +
                  '<div class="no-popup-menu">' +
				  '<img src="/csimages/video.png" ng-if="!videoOn" ng-click="showVideo()" width="200" alt="Click here to show the video" />' +
                  '</div>' + 
                  '<a href="{{doclink}}" ng-if="doclink" target="timdoc"><span ng-if="docoicon"><img ng-src="{{docicon}}"  alt="Go to doc" /> </span>' +
                  '{{doctext}}</a>'+
				  '<p ng-if="videoOn" class="pluginShow" ><a ng-click="hideVideo()">{{hidetext}}</a></p>'+
				  '<p class="plgfooter">Here comes footer</p>'+
				  '</div>';
}

videoApp.time2String = function(t) {
  if ( !t ) return "";
  h = Math.floor(t / 3600);
  t = (t - h*3600);
  m = Math.floor(t/60);
  s = (t - m*60);
  if ( !h ) h = ""; else h =h+"h";
  if ( !h && !m ) m = ""; else m =m+"m";
  s = s+"s";
  return h + m + s;
}

videoApp.set = function(scope,attrs,name,def) {
    scope[name] = def;
    if ( attrs && attrs[name] ) scope[name] = attrs[name];
    if ( scope.attrs && scope.attrs[name] ) scope[name] = scope.attrs[name];
    if ( scope[name] == "None" ) scope[name] = "";
    return scope[name];
}


videoApp.directiveFunction = function(t) {
	return {
		link: function (scope, element, attrs) {
            videoApp.set(scope,attrs,"file");
            videoApp.set(scope,attrs,"width");
            videoApp.set(scope,attrs,"height");
            videoApp.set(scope,attrs,"videoname");
            videoApp.set(scope,attrs,"doctext");
            videoApp.set(scope,attrs,"doclink");
            videoApp.set(scope,attrs,"hidetext","hide video");
            videoApp.set(scope,attrs,"videoicon","/csimages/video_small.png");
            videoApp.set(scope,attrs,"docicon","/csimages/book.png");
			videoApp.set(scope,attrs,"open",false);
            if ( scope.videoicon == "False" ) scope.videoicon = "";
            if ( scope.docicon == "False" ) scope.docicon = "";
			scope.start = videoApp.muunna(attrs.start);
			scope.end = videoApp.muunna(attrs.end);
            scope.duration = videoApp.time2String(scope.end - scope.start);
            if ( scope.duration != "" ) scope.duration = "(" + scope.duration + ") ";
            scope.limits = "(" + videoApp.time2String(scope.start) + "-" + videoApp.time2String(scope.end) + ")";
            if ( scope.limits == "(-)" ) scope.limits = "";
            scope.span = "";
            scope.startt = videoApp.time2String(scope.start);
            if ( scope.startt ) scope.startt = ", " + scope.startt;
			if ( attrs.stem ) scope.stem = attrs.stem;
			if ( attrs.iframe ) scope.iframe = true;
            if ( scope.file && scope.file.indexOf("youtube") >= 0 )  scope.iframe = true; // youtube must be in iframe
			scope.videoHtml = element[0].childNodes[2]
			head = videoApp.getHeading(attrs,"header",scope,"h4");
			element[0].childNodes[0].outerHTML = head;
			var n = element[0].childNodes.length;
			if ( n > 1 ) element[0].childNodes[n-1].outerHTML = videoApp.getHeading(attrs,"footer",scope,'p class="plgfooter"');
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
        template: videoApp.directiveTemplateVideo(t),
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
	$scope.videoOn = false;
	
	$scope.runVideo = function() {
		// $scope.viewCode = false;
	};

	$scope.hideVideo = function() {
		$scope.videoOn = false;
		$scope.videoHtml.innerHTML = "<p></p>";
        $scope.span = "";
        return true;
	}
	
	$scope.showVideo = function() {
        if ( $scope.videoOn ) return $scope.hideVideo(); 
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
            if ( file.indexOf("youtube") >= 0 && file.indexOf("embed") < 0 ) {
                var parts = file.split("=");
                if ( parts.length > 1 )
                    file = "//www.youtube.com/embed/" + parts[1];
            }
			$scope.videoHtml.innerHTML = '<iframe id="'+vid+'" class="showVideo" src="' + file + t +  '" ' + w + h + 'autoplay="true"  frameborder="0" allowfullscreen></iframe>';
			// '&rel=0'+
			// youtube: <iframe width="480" height="385" src="//www.youtube.com/embed/RwmU0O7hXts" frameborder="0" allowfullscreen></iframe>
		} else  { 
            t = ""
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
