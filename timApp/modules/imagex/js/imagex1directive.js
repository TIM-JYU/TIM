"use strict";
var imagexApp = angular.module('imagexApp', ['ngSanitize']);
imagexApp.TESTWITHOUTPLUGINS = false; // if one wants to test without imagex plugins

imagexApp.directive('imagexRunner',['$sanitize','$compile',
                  function ($sanitize,$compile1) {"use strict";
                      // Tätä kutsutaan yhden kerran kun plugin otetaan käyttöön
                      timHelper.sanitize = $sanitize;
imagexApp.sanitize = $sanitize;
                      imagexApp.compile = $compile1;
                      return imagexApp.directiveFunction(); }]
);


imagexApp.directiveFunction = function() {
"use strict";
// Koska tätä kutsutaan direktiivistä, tätä kutsutaan yhden kerran
    return {
		scope: {},
		controller: imagexApp.Controller,
		link: imagexApp.initScope,
		restrict: 'AE',
		/*
		compile: function(tElement, attrs) {
			var content = tElement.children();
		},
		*/
		transclude: true,
		replace: 'true',
		template: imagexApp.directiveTemplate()
		// templateUrl: 'html/paliTempl.html'
	};
};


imagexApp.directiveTemplate = function () {
"use strict";
// Koska tätä kutsutaan directiveFunction-metodista, tätä kutsutaan yhden kerran
    if ( imagexApp.TESTWITHOUTPLUGINS ) return '';
    return '<div class="csRunDiv no-popup-menu">' +
	'<h2>{{stem}}</h2>' +
	'<div>'+
	'<canvas id="ctx" width="800" height="600" ></canvas>'+
	'<div class="content">'+
	'</div>'+
	'</div>' +
	'<button ng-if="button"  ng-disabled="isRunning" ng-click="imagexScope.save();">{{button}}</button>&nbsp&nbsp' +
	'<a href="" ng-if="muokattu" ng-click="imagexScope.initCode()">{{resetText}}</a>' +
        '<span class="tries" ng-if="max_tries"> Tries: {{tries}}/{{max_tries}}</span>' +
	'<pre class="" ng-if="error">{{error}}</pre>' +
	'<pre  class="" ng-show="result">{{result}}</pre>' +
    	'<p class="plgfooter">Here comes footer</p>' +
	'</div>'
};


imagexApp.initDrawing = function(scope, canvas) {


function getPos(e, evt) {
    var rect = e.getBoundingClientRect(); 
    if (evt.touches) {
	var posX = evt.touches[0].clientX;
	var posY = evt.touches[0].clientY;
    }	
    else {
	var posX = evt.clientX;
	var posY = evt.clientY;
    }
    return {
	x: posX - rect.left,
	y: posY - rect.top
    };
}
/*	
function isObjectOnTopOf(position, object, name) {
    if (object.name === "target") {
	if (object.shape === "rectangle") {
	    var objx = object.x - object.r1 /2;
	    var objy = object.y - object.r2 /2;
	    var sina = Math.sin(Math.abs(object.a * to_radians));
	    var cosa = Math.cos(Math.abs(object.a * to_radians));
	    var tana = Math.tan(Math.abs(object.a * to_radians));
	    var atana = Math.atan(Math.abs(object.a * to_radians));
	    var x1is = objx - tana * objy;
	    var x2is = objx + object.r1 / cosa - tana * objy;
	    var y1is = objy + tana * objx;
	    var y2is = objy + object.r2 / cosa + tana * objx;
	    var pkx = x1is + tana * position.y;
	    var skx = x2is + tana * position.y;
	    var pky = y1is - tana * position.x;
	    var sky = y2is - tana * position.x;
	    if (position.x >= pkx && position.x <= skx && position.y >= pky && position.y <= sky) {
		if (name && object.name === name) return true;
		else if (!name) return true;
	    }
	}
	else if (object.shape === "ellipse") {
	    if (Math.pow((position.x - object.x), 2) / Math.pow(object.r1, 2) + 
		(Math.pow(position.y - object.y, 2)) / Math.pow(object.r2, 2) <= 1) {
		if (name && object.name === name) {
		    return true;
		} else if (!name) {
		    return true;
		}
	    }
	}
    }
    else if (position.x >= object.x && position.x <= object.x + object.r1 &&
	     position.y >= object.y && position.y <= object.y + object.r2) {
	if (name && object.name === name) {
	    return true;
	} else if (!name) {
	    return true;
	}
    }
    else return false;
}
*/

function isObjectOnTopOf(position, object, name) {
    if (object.name === "target") {
	if (object.shape === "rectangle") {
	    var topLeft = {x: object.x - object.r1 / 2, y: object.y - object.r2 / 2};
	    var topRight = {x: object.x + object.r1 / 2, y: object.y - object.r2 / 2};
	    var bottomLeft = {x: object.x - object.r1 / 2, y: object.y + object.r2 / 2};
	    var bottomRight = {x: object.x + object.r1 / 2, y: object.y + object.r2 / 2};

	    var objx = object.x - object.r1 /2;
	    var objy = object.y - object.r2 /2;
	    var sina = Math.sin(-object.a * to_radians);
	    var cosa = Math.cos(-object.a * to_radians);
	    var tana = Math.tan(Math.abs(object.a * to_radians));
	    var atana = Math.atan(Math.abs(object.a * to_radians));

	    //var x1is = objx - tana * objy;
	    //var x2is = objx + object.r1 / cosa - tana * objy;
	    //var y1is = objy + tana * objx;
	    //var y2is = objy + object.r2 / cosa + tana * objx;
	    //var pkx = x1is + tana * position.y;
	    //var skx = x2is + tana * position.y;
	    //var pky = y1is - tana * position.x;
	    //var sky = y2is - tana * position.x;

	    
	    var rotatedX = cosa * (position.x - objx) - sina * (position.y - objy);
	    var rotatedY = cosa * (position.y - objy) + sina * (position.x - objx);


	    if (rotatedX + objx >= topLeft.x && rotatedX + objx <= topRight.x &&
		rotatedY + objy >= topLeft.y && rotatedY + objy <= bottomRight.y) {
		if (name && object.name === name) return true;
		else if (!name) return true;
	    }
	}
	else if (object.shape === "ellipse") {
	    if (Math.pow((position.x - object.x), 2) / Math.pow(object.r1, 2) + 
		(Math.pow(position.y - object.y, 2)) / Math.pow(object.r2, 2) <= 1) {
		if (name && object.name === name) {
		    return true;
		} else if (!name) {
		    return true;
		}
	    }
	}
    }
    else if (position.x >= object.x && position.x <= object.x + object.r1 &&
	     position.y >= object.y && position.y <= object.y + object.r2) {
	if (name && object.name === name) {
	    return true;
	} else if (!name) {
	    return true;
	}
    }
    else return false;
}

function areObjectsOnTopOf(position, objects, name) {
    for (var i = 0; i < objects.length; i++) {
	var collision = isObjectOnTopOf(position, objects[i], name);
	if (collision) {
	    return objects[i];
	}
    }
    return null;
}

function DragTask(canvas) {
    this.canvas = canvas;
    // this.ctx = canvas.getContext('2d');
    this.drawObjects = [];
    this.activeDragObject = null;
    this.mousePosition = { x: 0, y: 0 };
    this.draw = function () {
        this.ctx = canvas.getContext('2d');
        this.ctx.clearRect(0, 0, 5000, 5000);
        for (var i = 0; i < this.drawObjects.length; i++) {
            this.drawObjects[i].ctx = this.ctx;
            if (this.activeDragObject) {
            this.activeDragObject.x = this.mousePosition.x - this.activeDragObject.xoffset;
            this.activeDragObject.y = this.mousePosition.y - this.activeDragObject.yoffset; }
            this.drawObjects[i].draw(this.drawObjects[i]); // kutsutaan objektin omaa piirtoa
            if (this.drawObjects[i].name == 'dragobject') {
            var onTopOf = areObjectsOnTopOf(this.drawObjects[i].pinPointLocation,
                            this.drawObjects, 'target'); 
            if (onTopOf) onTopOf.color = onTopOf.snapColor; }
            else if (this.drawObjects[i].name === 'target')
            this.drawObjects[i].color = this.drawObjects[i].origColor;
	}}.bind(this);

	this.interval = setInterval(this.draw, 500);
    //return;
	
    var moveEvent = function(event) {
	if (event.touches) {
	    var touch = event.touches[0] || event.changedTouches[0];
	    this.mousePosition = getPos(this.canvas, touch);
	}
	else {this.mousePosition = getPos(this.canvas, event);}
    }.bind(this);
    
    var downEvent = function(event) {
	if (event.touches) {
	    var touch = event.touches[0] || event.changedTouches[0];
	    this.mousePosition = getPos(this.canvas, touch);
	}
	else this.mousePosition = getPos(this.canvas, event);
	this.activeDragObject = areObjectsOnTopOf(this.mousePosition,
						  this.drawObjects, 'dragobject');
	if (this.activeDragObject) {
        this.activeDragObject.xoffset = this.mousePosition.x - this.activeDragObject.x; 
        this.activeDragObject.yoffset = this.mousePosition.y - this.activeDragObject.y; 
	    event.preventDefault();
//	    this.activeDragObject.currentTarget = null;
	}
    }.bind(this);
    
    var upEvent = function(event) {
	if (event.touches) {
	    var touch = event.touches[0] || event.changedTouches[0];
	    this.mousePosition = getPos(this.canvas, touch);
	}
	else {this.mousePosition = getPos(this.canvas, event);}
	var isTarget = areObjectsOnTopOf(this.activeDragObject.pinPointLocation,
					 this.drawObjects, 'target');
	// jos hiiri vapautetaan targetin päällä, siirretään paikalleen
	if (isTarget && isTarget.snap === true) {
	    this.activeDragObject.x = isTarget.x - this.activeDragObject.pinPointOffset.x;
	    this.activeDragObject.y = isTarget.y - this.activeDragObject.pinPointOffset.y;
	    //this.activeDragObject.currentTarget = target;
	}
	this.activeDragObject = null;
    }.bind(this);
    
    this.canvas.addEventListener('mousemove', moveEvent);
    this.canvas.addEventListener('touchmove', moveEvent);
    this.canvas.addEventListener('mousedown', downEvent);
    this.canvas.addEventListener('touchstart', downEvent);
    this.canvas.addEventListener('mouseup', upEvent);
    this.canvas.addEventListener('touchend', upEvent);
   // this.draw();
};

function Empty(objectValues) {};

function DragObject(dt, values) {
    this.draw = Empty;
    if (!values.type) return;
    this.name = 'dragobject';
    this.ctx = dt.ctx;
//    console.log(scope.drags);
//    console.log(values);
    if (values.state === 'state') {
	console.log(values);
	this.position = values.position;
	this.x = this.position[0] - values.pinpointoffsetx;
	this.y = this.position[1] - values.pinpointoffsety;
    }
    else {
	this.position = getValue(values.position, [0, 0]);
	this.x = this.position[0];
	this.y = this.position[1];
    }
    
    this.type = values.type;
    this.target = null;
    this.currentTarget = null;
    this.id = values.id;
    this.pinPoint = values.pin;
    this.imgproperties = values.imgproperties;
    this.textboxproperties = getValue(values.textboxproperties, {});
    this.draw = shapeFunctions[this.type].draw;
};

function Target(dt, values) {
    this.name = 'target';
    this.ctx = dt.ctx;
    this.position = getValue(values.target.position, [0, 0]);
    this.x = this.position[0];
    this.y = this.position[1];
    this.a = getValue(values.target.a, 0);
    if (values.target.snap == false) this.snap = false;
    else this.snap = true;
    this.shape = getValue(values.target.shape, 'rectangle');
    this.size = getValue(values.target.size, []);
    this.r1 = getValue(this.size[0], 10);
    this.r2 = getValue(this.size[1], this.r1)
    this.color =  getValue(values.target.color, 'Blue');
    this.origColor =  getValue(values.target.color, 'Blue');
    this.snapColor = getValue(values.target.snapColor, 'Cyan');
    this.draw = shapeFunctions[this.shape].draw;
};

function background(dt, imageSource) {
    this.ctx = dt.ctx;
    this.image = new Image();
    this.image.src = imageSource;
    this.draw = function () {
	this.ctx.drawImage(this.image, 0, 0);
    }.bind(this);
};

function getValue(value, defaultValue) {
    if (value === null || value === undefined || value === "" || 
	(value.constructor === Array && value.length == 0))
	return defaultValue;
    else return value;
}

function getValue1(value, key, defaultValue) {
    if (value === null || value === undefined || value === "" || 
	(value.constructor === Array && value.length == 0))
	return defaultValue;
    return getValue(value[key], defaultValue);
}

function getValue2(value, key1, key2, defaultValue) {
    if (value === null || value === undefined || value === "" || 
	(value.constructor === Array && value.length == 0))
	return defaultValue;
    return getValue1(value[key1],key2, defaultValue);
}

var prevPos = null;
var mouseDown = false;
var to_radians = Math.PI / 180;
var doc_ctx = canvas;
var dt = new DragTask(doc_ctx);

var shapeFunctions = {
    "ellipse": {
	"draw":
	function (objectValues) {
	    this.ctx = objectValues.ctx;
	    this.ctx.strokeStyle = this.color;
	    this.ctx.lineWidth = 2;
	    this.ctx.save();
	    this.ctx.beginPath();
	    this.ctx.translate(this.x, this.y);
	    this.ctx.rotate(this.a * to_radians); 
	    this.ctx.scale(this.r1, this.r2);
	    this.ctx.arc(0, 0, 1, 0, 2 * Math.PI, false);
	    this.ctx.restore(); 
	    this.ctx.stroke();}
    },
    "rectangle": {
	"draw":
	function (objectValues) {
	    this.ctx = objectValues.ctx;
	    this.ctx.strokeStyle = this.color;
	    this.ctx.lineWidth = 2;
	    this.ctx.save(); 
	    this.ctx.translate(this.x, this.y); 
	    this.ctx.rotate(this.a * to_radians); 
	    this.ctx.strokeRect(-this.r1 / 2, -this.r2 / 2, this.r1, this.r2);
	    this.ctx.restore(); } // stroke vasta tämän jälkeen?
    },
    "img": {
	"draw":
	function (objectValues) {
	    this.ctx = objectValues.ctx;
	    var image = new Image();
	    image.src = getValue1(objectValues.imgproperties, "src", "");
	    this.r1 = getValue(image.width, 20);
	    this.r2 = getValue(image.height, 20);
	    this.x = getValue(objectValues.x, 0);
	    this.y = getValue(objectValues.y, 0);
	    // this.ctx.drawImage(image, this.x, this.y, this.r1, this.r2);
	    this.object = {x: this.x, y: this.y, r1: this.r1, r2: this.r2};
	    if (getValue1(objectValues.imgproperties, "textbox", false)) {
		this.borderColor = shapeFunctions["textbox"].draw(objectValues); }
	    this.pinPointOffset = shapeFunctions["pin"].draw(objectValues, this);
	    this.pinPointLocation = {x: objectValues.x + this.pinPointOffset.x,
				     y: objectValues.y + this.pinPointOffset.y};
	}
    },
    "textbox": {
	"draw":
	function (objectValues) {
	    this.ctx = objectValues.ctx;
	    this.ctx.font = getValue(objectValues.textboxproperties.font, '14px Arial');
	    this.ctx.textBaseline = 'top';
	    this.borderGap = 3;
	    if (objectValues.type === 'img') {
		this.textBoxOffset = getValue(objectValues.textboxproperties.positionOnImage.coord,
					      [0, 0]);
		this.x = getValue(objectValues.x + this.textBoxOffset[0], 0);
		this.y = getValue(objectValues.y + this.textBoxOffset[1], 0);
	    }
	    else {
		this.x = objectValues.x;
		this.y = objectValues.y; }
	    this.text = getValue(objectValues.textboxproperties.text, objectValues.id);
	    this.textwidth = this.ctx.measureText(this.text).width;
	    this.textHeight = parseInt(this.ctx.font, 10);
	    this.textBoxSize = getValue(objectValues.textboxproperties.size, []);
	    this.r1 = getValue(this.textBoxSize[0], this.textwidth + 2 * this.borderGap);
	    this.r2 = getValue(this.textBoxSize[1], this.textHeight + 2 * this.borderGap);
	    this.borderColor = getValue(objectValues.textboxproperties.borderColor, 'Black');
	    this.fillColor = getValue(objectValues.textboxproperties.fillColor, 'White');
	    this.textColor = getValue(objectValues.textboxproperties.textColor, 'Black');
	    this.ctx.fillStyle = this.fillColor;
	    this.ctx.fillRect(this.x, this.y, this.r1, this.r2);
	    this.ctx.strokeStyle = this.borderColor;
	    this.ctx.lineWidth = getValue(objectValues.textboxproperties.borderWidth, 2);
	    this.ctx.strokeRect(this.x, this.y,	this.r1, this.r2);
	    this.ctx.fillStyle = this.textColor;
	    this.ctx.fillText(this.text, this.x + this.borderGap, this.y + this.borderGap);
	    if (objectValues.type === 'textbox') {
		this.object = {x: this.x, y: this.y, r1: this.r1, r2: this.r2, name: 'textbox'};
		this.pinPointOffset = shapeFunctions["pin"].draw(objectValues);
		this.pinPointLocation = {x: this.x + this.pinPointOffset.x,
					 y: this.y + this.pinPointOffset.y};
	    }
	    return this.borderColor;
	}
    },
    "pin": {
	"draw":
	function (objectValues) {
	    this.ctx = objectValues.ctx;
	    this.r1 = objectValues.r1;
	    this.r2 = objectValues.r2;
	    this.x = objectValues.x;
	    this.y = objectValues.y;
	    this.object = objectValues.object;
	    this.pinProperties = getValue(objectValues.pinPoint, {});
	    this.pinProperties.position = getValue(objectValues.pinPoint.position, {});
	    this.pinLength = getValue(this.pinProperties.length, 10); 
	    this.pinPositions = {
		"west": {x: 0, y: this.r2 / 2, off: {x: -this.pinLength, y: 0}},
		"east": {x: this.r1, y: this.r2 / 2, off: {x:this.pinLength, y: 0}},
		"north": {x: this.r1 / 2, y: 0, off: {x:0, y: -this.pinLength}},
		"south": {x: this.r1 / 2, y: this.r2, off: {x:0, y: this.pinLength}},
		"southeast": {x: this.r1, y: this.r2, 
			      off: {x: this.pinLength, y: this.pinLength}},
		"northeast": {x: this.r1, y: 0, 
			      off: {x: this.pinLength, y: -this.pinLength}},
		"southwest": {x: 0, y: this.r2, 
			      off: {x: -this.pinLength, y: this.pinLength}},
		"northwest": {x: 0, y: 0, off: {x: -this.pinLength, y: -this.pinLength}},
		"center": {x: this.r1 / 2, y: this.r2 / 2, off:{x: 0, y: 0}}
	    };
	    this.pinPositionAlign = getValue(this.pinProperties.position.align,	'northwest');
	    this.pinPosition = this.pinPositions[this.pinPositionAlign];
	    this.pinProperties.position.coord = getValue(this.pinProperties.position.coord, []);
	    this.pinPosition.off.x = getValue(this.pinProperties.position.coord[0],
					      this.pinPositions[this.pinPositionAlign].off.x);
	    this.pinPosition.off.y = getValue(this.pinProperties.position.coord[1],
					      this.pinPositions[this.pinPositionAlign].off.y);
	    if (this.pinProperties && this.pinProperties.visible === true) {
		this.dotPosition = {x: this.pinPosition.x + this.pinPosition.off.x,
				    y: this.pinPosition.y + this.pinPosition.off.y}
		this.ctx.beginPath();
		this.ctx.moveTo(this.x + this.pinPosition.x, this.y + this.pinPosition.y);
		this.ctx.strokeStyle = getValue(this.pinProperties.color,
						getValue(objectValues.borderColor, 'blue'));
		this.ctx.lineWidth = getValue(objectValues.textboxproperties.borderWidth, 2);
		this.ctx.lineTo(this.x + this.dotPosition.x, this.y + this.dotPosition.y);
		this.ctx.stroke();
		this.ctx.beginPath();
		this.ctx.arc(this.x + this.dotPosition.x, 
			     this.y + this.dotPosition.y, 1.5, 0, 2 * Math.PI, false);
		this.ctx.stroke();
	    }
	    else this.dotPosition = {x: this.pinPosition.x, y: this.pinPosition.y}
	    return this.dotPosition;
	}
    }
};

if (scope.attrs.state) {
//    console.log(scope.drags);
    var userObjects = scope.attrs.state.markup.objects.objects;
//    console.log(userObjects);
    var i;
    for (i = 0; i < userObjects.length; i++) {
	userObjects[i].state = "state"; }	
    }
    

else var userObjects = scope.attrs.markup.objects; 

var userTargets = scope.attrs.markup.targets;
var background = new background(dt, scope.attrs.markup.background);
var targets = [];
var objects = [];

if(userTargets) {
    for (i = 0; i < userTargets.length; i++) {
	targets[i] = new Target(dt, userTargets[i]);
    } }

if (userObjects){
    for (i = 0; i < userObjects.length; i++) {
	objects[i] = new DragObject(dt, userObjects[i]);
	if(!scope.drags){
	    scope.drags = [];
	}
	//scope.drags.push({"id":objects[i].id,"x":objects[i].x,"y":objects[i].y});
	scope.drags.push(objects[i]);
	scope.$watch('drags', function() { scope.imagexScope.watchDrags(); }, true);
    }
}
dt.drawObjects.push(background);

for (i = 0; i < targets.length; i++) {
    dt.drawObjects.push(targets[i]);
}
	
for (i = 0; i < objects.length; i++) {
    dt.drawObjects.push(objects[i]);
}
	
    };



imagexApp.Controller = function($scope, $http, $transclude, $interval) {
"use strict";
// Tätä kutsutaan kerran jokaiselle pluginin esiintymälle.
// Angular kutsuu tätä koska se on sanottu direktiivifunktiossa Controlleriksi.
// Tähän tullaan ensin ja sitten initScope-metodiin
// Siitä ei ole mitään hajua mistä se keksii tälle nuo parametrit???
    if (imagexApp.TESTWITHOUTPLUGINS) return;
    $scope.imagexScope = new ImagexScope($scope);
    $scope.attrs = {};
    $scope.http = $http; 
    $scope.interval = $interval;

    // Luodaan $scope.attrs joka on avattuna sisällössä olev JSON tai HEX
    $transclude(function(clone,scope) { timHelper.initAttributes(clone,$scope);  });

    $scope.errors = [];
	$scope.muokattu = false;
    $scope.result = "";
};


imagexApp.initScope = function (scope, element, attrs) {
"use strict";
// Tätä kutsutaan kerran jokaiselle pluginin esiintymälle.
// Angular kutsuu tätä koska se on sanottu direktiivifunktiossa Link-metodiksi.
    scope.cursor = "\u0383"; //"\u0347"; // "\u02FD";
    scope.plugin = element.parent().attr("data-plugin");
    scope.taskId = element.parent().attr("id");

    // Etsitään kullekin attribuutille arvo joko scope.attrs tai attrs-parametrista. Jos ei ole, käytetään oletusta.
    timHelper.set(scope, attrs, "stem");
    timHelper.set(scope, attrs, "inputstem");
    timHelper.set(scope, attrs, "inputplaceholder", "Write your input here");
    timHelper.set(scope, attrs, "user_id");
    timHelper.set(scope, attrs, "button", "Save");
    timHelper.set(scope, attrs, "resetText", "Reset");
    timHelper.set(scope, attrs, "state.tries", 0);
    timHelper.set(scope, attrs, "max_tries");
    timHelper.set(scope, attrs, "cols", 20);
    timHelper.set(scope, attrs, "autoupdate", 500);
    timHelper.setn(scope, "tid", attrs, ".taskID"); // vain kokeilu että "juuresta" ottaminen toimii

    // TODO: oikeat muuttujat.

    timHelper.set(scope,attrs,"background");
    // Tässä on nyt kaikki raahattavat objektit
    timHelper.set(scope,attrs,"objects","http://localhost/static/images/jyulogo.png");
    // Tässä pitäisi olla kaikki targetit
    timHelper.set(scope,attrs,"targets");
 //   console.log(scope.attrs);

    // Otsikot.  Oletetaan että 1. elementti korvaatan header-otsikolla ja viimeinen footerilla
    element[0].childNodes[0].outerHTML = timHelper.getHeading(scope, attrs, "header", "h4");
    var n = element[0].childNodes.length;
    if (n > 1) element[0].childNodes[n - 1].outerHTML = timHelper.getHeading(scope, attrs, "footer", 'p class="plgfooter"');
    imagexApp.initDrawing(scope, element[0].childNodes[1].childNodes[0]);
    scope.attrs = {}; // not needed any more
};


// Tehdään kaikista toiminnallisista funktoista oma luokka, jotta
// niitä ei erikseen lisätä jokaisen pluginin esiintymän kohdalla uudelleen.
function ImagexScope(scope) {
"use strict";
    this.scope = scope;
}

ImagexScope.prototype.watchDrags = function() {
"use strict";
    var $scope = this.scope;
};



ImagexScope.prototype.initCode = function() {
"use strict";
    var $scope = this.scope;
    $scope.error = "";
    $scope.result = "";
};


ImagexScope.prototype.save = function() {
"use strict";
    this.doSave(false);
	//$scope.evalAsync( $scope.drags);
};

// Get the important stuff from dragobjects
ImagexScope.prototype.getDragObjectJson = function() {
    var $scope = this.scope;
    var dragtable = $scope.drags;
    var json = [];
    for(var i = 0; i < dragtable.length ; i++) {
	json.push({"id":dragtable[i].id, 
		   "position":[dragtable[i].pinPointLocation.x, dragtable[i].pinPointLocation.y],
	 	   "pinpointoffsetx":dragtable[i].pinPointOffset.x,
		   "pinpointoffsety":dragtable[i].pinPointOffset.y});
    }
    return json;
};


ImagexScope.prototype.doSave = function(nosave) {
"use strict";
    var $scope = this.scope;
	// These break the whole javascript if used, positions are updated somehow anyways.
	//$scope.$digest();
	//$scope.$apply();

    $scope.error = "... saving ...";
    $scope.isRunning = true;
    $scope.result = "";

    var params = {
        'input': {
            'markup': {'taskId': $scope.taskId, 'user_id': $scope.user_id},
			'drags' : this.getDragObjectJson()
        }
    };

    if (nosave) params.input.nosave = true;
	// Todo: pitäisiköhän tätä korjata jossain muualla
    var url = "/imagex/answer";
    if ($scope.plugin) {
        url = $scope.plugin;
        var i = url.lastIndexOf("/");
        if (i > 0) url = url.substring(i);
        url += "/" + $scope.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
    }

    $scope.http({method: 'PUT', url: url, data: params, headers: {'Content-Type': 'application/json'}, timeout: 20000}
    ).success(function (data, status, headers, config) {
        $scope.isRunning = false;
        $scope.error = data.web.error;
        $scope.result = data.web.result;
        $scope.tries = data.web.tries;
    }).error(function (data, status) {
        $scope.isRunning = false;
        $scope.errors.push(status);
        $scope.error = "Ikuinen silmukka tai jokin muu vika?";
        // $scope.error = data;
    });
};

