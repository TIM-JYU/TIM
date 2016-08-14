"use strict";
var imagexApp = angular.module('imagexApp', ['ngSanitize']);
imagexApp.TESTWITHOUTPLUGINS = false; // if one wants to test without imagex plugins

imagexApp.directive('imagexRunner',
    ['$sanitize','$compile',
        function ($sanitize,$compile1) {
            "use strict";
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
        '<canvas id="ctx" width={{canvaswidth}} height={{canvasheight}} ></canvas>'+
        '<div class="content">'+
        '</div>'+
        '</div>'+
        '<button ng-if="button" ng-disabled="isRunning" ng-click="imagexScope.save();">{{button}}</button>&nbsp&nbsp' +
        '<button ng-if="button" ng-disabled="isRunning" ng-click="imagexScope.showAnswer();">Showanswer</button>&nbsp&nbsp' +
        '<a ng-if="button" ng-disabled="isRunning" ng-click="imagexScope.resetExercise();">Reset</a>&nbsp&nbsp' +
        '<a href="" ng-if="muokattu" ng-click="imagexScope.initCode()">{{resetText}}</a>' +
        '<input ng-show="preview" id="coords" />' +
        '<span class="tries" ng-if="max_tries"> Tries: {{tries}}/{{max_tries}}</span>' +
        '<pre class="" ng-if="error">{{error}}</pre>' +
        '<pre class="" ng-show="result">{{result}}</pre>' +
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
            posX = evt.clientX;
            posY = evt.clientY;
        }
        return {
            x: posX - rect.left,
            y: posY - rect.top
        };
    }

    function isObjectOnTopOf(position, object, name) {
        var sina = Math.sin(-object.a * to_radians);
        var cosa = Math.cos(-object.a * to_radians);
        var rotatedX = cosa * (position.x - object.x) - sina * (position.y - object.y);
        var rotatedY = cosa * (position.y - object.y) + sina * (position.x - object.x);

        if (object.name === "target") {
            if (object.type === "rectangle") {
                if (rotatedX >= -object.r1 / 2 && rotatedX <= object.r1 / 2 &&
                    rotatedY >= -object.r2 / 2 && rotatedY <= object.r2 / 2) {
                    if (name && object.name === name) return true;
                    else if (!name) return true;
                } }

            else if (object.type === "ellipse") {
                if ((Math.pow(rotatedX, 2) / Math.pow(object.r1/2, 2)) +
                    (Math.pow(rotatedY, 2) / Math.pow(object.r2/2, 2)) <= 1) {
                    if (name && object.name === name) return true;
                    else if (!name) return true;
                }
            } }
        else if (object.name === "dragobject") {
            if (object.type === "img" || object.type === "textbox") {
                if (rotatedX >= - object.pinPosition.off.x - object.pinPosition.x &&
                    rotatedX <= object.r1 - object.pinPosition.off.x - object.pinPosition.x &&
                    rotatedY >= - object.pinPosition.off.y - object.pinPosition.y &&
                    rotatedY <= object.r2 -object.pinPosition.off.y- object.pinPosition.y) {
                    if (name && object.name === name) return true;
                    else if (!name) return true; }
            }
            if (object.type === "vector") {
                if (rotatedX >= 0 && rotatedX <= object.r1 &&
                    rotatedY >= -object.r2 / 2 - object.arrowHeadWidth &&
                    rotatedY <= object.r2 / 2 + object.arrowHeadWidth) {
                    if (name && object.name === name) return true;
                    else if (!name) return true; }
            }
        }
    }

    function areObjectsOnTopOf(position, objects, name) {
        for (var i = objects.length-1; i >=0; i--) {
            var collision = isObjectOnTopOf(position, objects[i], name);
            if (collision) {
                return objects[i];
            }
        }
        return null;
    }

    function DragTask(canvas) {
        this.canvas = canvas;
        this.drawObjects = [];
        this.activeDragObject = null;
        this.mousePosition = { x: 0, y: 0 };
        var topmostIndex;
        var topmostElement;
        this.draw = function () {
            this.ctx = canvas.getContext('2d');
            this.ctx.clearRect(0, 0, canvas.width, canvas.height);
            var activeDragObjectId = "";

            scope.incompleteImages = 0;

            for (var i = 0; i < this.drawObjects.length; i++) {
                this.drawObjects[i].ctx = this.ctx;
                if (this.activeDragObject) {
                    activeDragObjectId = this.activeDragObject.id;
                    this.activeDragObject.x = this.mousePosition.x - this.activeDragObject.xoffset;
                    this.activeDragObject.y = this.mousePosition.y - this.activeDragObject.yoffset;
                    if (this.drawObjects[i].id == activeDragObjectId) {
                        topmostIndex = i;
                        topmostElement = this.drawObjects[i]; }
                }
                if (activeDragObjectId != this.drawObjects[i].id) {
                    this.drawObjects[i].draw(this.drawObjects[i]); // kutsutaan objektin omaa piirtoa
                }
                if (this.drawObjects[i].name == 'dragobject') {
                    var onTopOf = areObjectsOnTopOf(this.drawObjects[i],
                        this.drawObjects, 'target');
                    if (onTopOf) onTopOf.color = onTopOf.snapColor; }
                else if (this.drawObjects[i].name === 'target')
                    this.drawObjects[i].color = this.drawObjects[i].origColor;
            }

            if (scope.incompleteImages !== 0) {
                setTimeout(this.draw, 500);

                //      tee tÃ¤hÃ¤n rajoitus
            }

            if (this.activeDragObject) this.activeDragObject.draw(this.activeDragObject);
        }.bind(this);
        this.interval = setTimeout(this.draw, 20);
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
            }
            this.draw();

            if ( this.canvas.coords && scope.preview) {
                //&& scope.preview) { // if there is palce to put coords
                this.canvas.coords.value =
                    "[" + Math.round(this.mousePosition.x) + ", " + Math.round(this.mousePosition.y) + "]";
                //scope.coords.select();
                //document.execCommand('copy');
                if ( typeof(editorChangeValue) !== 'undefined' )
                    editorChangeValue(["position:"], this.canvas.coords.value);
            }
        }.bind(this);

        var moveEvent = function(event) {
            if (event.touches) {
                var touch = event.touches[0] || event.changedTouches[0];
                this.mousePosition = getPos(this.canvas, touch);
            }
            else {this.mousePosition = getPos(this.canvas, event);}
            this.draw();
        }.bind(this);

        var upEvent = function(event) {
            if (event.touches) {
                var touch = event.touches[0] || event.changedTouches[0];
                this.mousePosition = getPos(this.canvas, touch);
            }
            else {this.mousePosition = getPos(this.canvas, event);}
            if(this.activeDragObject) {
                var isTarget = areObjectsOnTopOf(this.activeDragObject,
                    this.drawObjects, 'target');
                this.drawObjects.splice(topmostIndex, 1);
                this.drawObjects.splice(this.drawObjects.length, 0, topmostElement);
            }

            if (isTarget && isTarget.snap === true) {
                this.activeDragObject.x = isTarget.x;
                this.activeDragObject.y = isTarget.y;
            }
            this.activeDragObject = null;
            this.draw();
        }.bind(this);

        this.canvas.addEventListener('mousemove', moveEvent);
        this.canvas.addEventListener('touchmove', moveEvent);
        this.canvas.addEventListener('mousedown', downEvent);
        this.canvas.addEventListener('touchstart', downEvent);
        this.canvas.addEventListener('mouseup', upEvent);
        this.canvas.addEventListener('touchend', upEvent);

        // Lisätty eventlistenereiden poistamiseen.
        this.removeEventListeners = function() {
            this.canvas.removeEventListener('mousemove', moveEvent);
            this.canvas.removeEventListener('touchmove', moveEvent);
            this.canvas.removeEventListener('mousedown', downEvent);
            this.canvas.removeEventListener('touchstart', downEvent);
            this.canvas.removeEventListener('mouseup', upEvent);
            this.canvas.removeEventListener('touchend', upEvent);
        }
    }
    function Empty(objectValues) {
    }
    function DragObject(dt, values) {
        this.draw = Empty;
        if (!values.type) return;
        this.name = 'dragobject';
        this.ctx = dt.ctx;
        if (values.state === 'state') {
            this.position = values.position;
            this.x = this.position[0] - values.pinpointoffsetx;
            this.y = this.position[1] - values.pinpointoffsety; }
        else {
            this.position = getValue(values.position, [0, 0]);
            this.x = this.position[0];
            this.y = this.position[1]; }
        this.size = getValue(values.size, [10, 10]);
        this.r1 = getValue(this.size[0], 10);
        this.r2 = getValue(this.size[1], this.r1);
        this.type = values.type;
        this.target = null;
        this.currentTarget = null;
        this.id = values.id;
        this.pin = values.pin;
        this.a = getValue(values.a, 0);
        this.imgproperties = values.imgproperties;
        this.textboxproperties = getValue(values.textboxproperties, {});
        this.vectorproperties = getValue(values.vectorproperties, {});

        this.init = shapeFunctions[this.type].init;
        this.pinInit = shapeFunctions['pin'].init;
        this.textbox = {};
        this.textbox.init = shapeFunctions['textbox'].init;
        this.textbox.init(values);

        if (this.type === 'vector') {
            this.vectorInit = shapeFunctions['vector'].init;
            this.vectorInit(values);
            this.vectorDraw = shapeFunctions['vector'].draw;
        }

        if (this.type === 'img') {
            this.init2 = shapeFunctions['img'].init2;
            this.imageDraw = shapeFunctions['img'].draw;
        }

        this.pinInit(values);
        // this.pinDraw = shapeFunctions['pin'].draw;
        this.textbox.draw = shapeFunctions['textbox'].draw;

        this.init(values);
        this.draw = shapeFunctions['pin'].draw;
        // this.draw = shapeFunctions[this.type].draw;
    }
    function Target(dt, values) {
        this.name = 'target';   
        this.ctx = dt.ctx;
        this.position = getValue(values.position, [0, 0]);
        this.x = this.position[0];
        this.y = this.position[1];
        this.a = getValue(values.a, 0);
        this.snap = getValue(values.snap,true);
        this.type = getValue(values.type, 'rectangle');
        // this.type = getValue(values, 'target.type', default, 'rectangle');
        this.size = getValue(values.size, [10, 10]);
        this.imgproperties = values.imgproperties;
        this.textboxproperties = getValue(values.textboxproperties, {});
        this.vectorproperties = getValue(values.vectorproperties, {});
        this.r1 = getValue(this.size[0], 10);
        this.r2 = getValue(this.size[1], this.r1);
        this.color =  getValue(values.color, 'Blue');
        this.origColor =  getValue(values.color, 'Blue');
        this.snapColor = getValue(values.snapColor, 'Cyan');
        this.init = shapeFunctions[this.type].init;
        this.init(values);
        //this.textboxInit = shapeFunctions['textbox'].init;
        //this.textBoxDraw = shapeFunctions['textbox'].draw;

        this.draw = shapeFunctions[this.type].draw;

    }
    function FixedObject(dt, values) {
        if (values.name === 'background') {
            this.type = 'img';
            this.name = 'background'; }
        else {
            this.type = getValue(values.type, 'rectangle');
            this.name = 'fixedobject'; }

        this.ctx = dt.ctx;
        this.position = getValue(values.position, [0, 0]);
        this.x = this.position[0];
        this.y = this.position[1];
        this.a = getValue(values.a, 0);
        this.color = getValue(values.color, 'Blue');
        this.size = getValue(values.size, [10, 10]);
        this.r1 = getValue(this.size[0], 10);
        this.r2 = getValue(this.size[1], this.r1);
        this.imgproperties = getValue(values.imgproperties, {});
        this.textboxproperties = getValue(values.textboxproperties, {});
        this.vectorproperties = getValue(values.vectorproperties, {});

        this.init = shapeFunctions[this.type].init;
        if (this.name === 'fixedobject') {
            this.textbox = {};
            this.textbox.init = shapeFunctions['textbox'].init;
            this.textbox.init(values);
            this.textbox.draw = shapeFunctions['textbox'].draw; }

        if (this.type === 'img') {
            this.init2 = shapeFunctions['img'].init2;
        }

        this.imageDraw = shapeFunctions['img'].draw;
        this.vectorDraw = shapeFunctions['vector'].draw;

        this.init(values);
        this.draw = shapeFunctions[this.type].draw;
    }
// Kutsuu viivaa piirtÃ¤vÃ¤Ã¤ funktiota.
    function Line(dt, values){
        this.ctx = dt.ctx;
        this.position = getValue(values.position, [0, 0]);
        this.endposition = getValue(values.endposition, [0, 0]);
        this.draw = shapeFunctions["line"].draw;
    }
    //this.pinProperties.position = getValue(initValues, "pinPoint.position", {});
    /*
     function getValueTest(value, key, defaultValue) {
     scope.previousValue = {};
     var keys = key.split(".");
     var returnValue = value;
     for (var i = 0; i < keys.length; i++) {
     if (returnValue[keys[i]]) scope.previousValue[keys[i]] = returnValue[keys[i]];
     else if (scope.previousValue[keys[i]]) returnValue[keys[i]] =
     scope.previousValue[keys[i]];
     else {scope.previousValue[keys[i]] = defaultValue;
     returnValue[keys[i]] = defaultValue; }
     returnValue = returnValue[keys[i]];
     }
     return returnValue;
     }
     var testValue = {pinni: {paikka: {x:3, y:7}}};
     var pinnin_paikka = getValueTest(testValue, "pinni.paikka", {});
     */

    function getValue(value, defaultValue) {
        // function getValue(value, key, defaultValue) {
        // scope.default...
        // var keys = key.split["."];
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

    var to_radians = Math.PI / 180;
    var doc_ctx = canvas;
    var dt = new DragTask(doc_ctx);
    scope.dt = dt;

    //var canvas = element[0];

    var shapeFunctions = {
        //TÃ¤mÃ¤ piirtÃ¤Ã¤ viivan, mutta vain kerran.
        line: {
            init:
                function (initValues) {

                },
            draw:
                function (objectValues) {
                    this.ctx = objectValues.ctx;
                    //attribuuteista vÃ¤ri ja leveys sekÃ¤ aseta dash
                    this.ctx.beginPath();
                    this.ctx.moveTo(this.position[0],this.position[1]);
                    this.ctx.lineTo(this.endposition[0],this.endposition[1]);
                    this.ctx.lineWidth = 2;
                    this.ctx.strokeStyle = "green";
                    this.ctx.stroke();
                }
        },

        ellipse: {
            init:
                function (initValues) {

                },
            draw:
                function (objectValues) {
                    this.ctx = objectValues.ctx;
                    this.ctx.strokeStyle = this.color;
                    this.ctx.lineWidth = 2;
                    this.ctx.save();
                    this.ctx.beginPath();
                    this.ctx.translate(this.x, this.y);
                    this.ctx.rotate(this.a * to_radians);
                    this.ctx.scale(this.r1/2, this.r2/2);
                    this.ctx.arc(0, 0, 1, 0, 2 * Math.PI, false);
                    this.ctx.restore();
                    this.ctx.stroke();}
        },

        rectangle: {
            init:
                function (initValues) {
                    this.cornerRadius = getValue(initValues.cornerradius, 0);
                    if (this.cornerRadius > this.r1 / 2 || this.cornerRadius > this.r2 / 2) {
                        if (this.cornerRadius > this.r1 / 2) this.cornerRadius = this.r1 / 2;
                        if (this.cornerRadius > this.r2 / 2) this.cornerRadius = this.r2 / 2; }
                },
            draw:
                function (objectValues) {
                    this.ctx = objectValues.ctx;
                    this.ctx.strokeStyle = this.color;
                    this.ctx.lineWidth = 2;
                    this.ctx.save();
                    this.ctx.translate(this.x, this.y);
                    this.ctx.rotate(this.a * to_radians);
                    this.ctx.beginPath();
                    this.ctx.moveTo(-this.r1 / 2 - 1 + this.cornerRadius, -this.r2 / 2);
                    this.ctx.lineTo(this.r1 / 2 - this.cornerRadius, -this.r2 / 2);
                    this.ctx.arc(this.r1 / 2 - this.cornerRadius, -this.r2 / 2 + this.cornerRadius,
                        this.cornerRadius, 1.5 * Math.PI, 0);
                    this.ctx.lineTo(this.r1 / 2, this.r2 / 2 - this.cornerRadius);
                    this.ctx.arc(this.r1 / 2 - this.cornerRadius, this.r2 / 2 - this.cornerRadius,
                        this.cornerRadius, 0, 0.5 * Math.PI);
                    this.ctx.lineTo(-this.r1 / 2 + this.cornerRadius, this.r2 / 2);
                    this.ctx.arc(-this.r1 / 2 + this.cornerRadius, this.r2 / 2 - this.cornerRadius,
                        this.cornerRadius, 0.5 * Math.PI, Math.PI);
                    this.ctx.lineTo(-this.r1 / 2, -this.r2 / 2 + this.cornerRadius);
                    this.ctx.arc(-this.r1 / 2 + this.cornerRadius, -this.r2 / 2 + this.cornerRadius,
                        this.cornerRadius, Math.PI, 1.5 * Math.PI);
                    this.ctx.restore();
                    this.ctx.stroke(); }
        },

        vector: {
            init:
                function (initValues) {
                    this.r1 = this.size[0];
                    this.r2 = this.size[1];
                    this.arrowHeadWidth = this.r2 / 3;
                    this.arrowHeadLength = this.r2 / 1.5;
                },
            draw:
                function (objectValues) {
                    this.ctx = objectValues.ctx;
                    this.ctx.strokeStyle = getValue(objectValues.vectorproperties.color, 'Black');
                    this.ctx.fillStyle = this.ctx.strokeStyle;
                    if (objectValues.name === 'fixedobject') {
                        this.ctx.save();
                        this.ctx.translate(this.x, this.y);
                        this.ctx.rotate(this.a * to_radians);}
                    this.ctx.beginPath();
                    this.ctx.lineTo(0, this.r2 / 6);
                    this.ctx.lineTo(this.r1 - this.arrowHeadLength, this.r2 / 6);
                    this.ctx.lineTo(this.r1 - this.arrowHeadLength, this.r2 / 6 +
                        this.arrowHeadWidth);
                    this.ctx.lineTo(this.r1, 0);
                    this.ctx.lineTo(this.r1 - this.arrowHeadLength, - this.r2 / 6 -
                        this.arrowHeadWidth);
                    this.ctx.lineTo(this.r1 - this.arrowHeadLength, - this.r2 / 6);
                    this.ctx.lineTo(0, - this.r2 / 6);
                    this.ctx.lineTo(0, 0);
                    this.ctx.fill();
                    if (getValue1(objectValues.vectorproperties, "textbox", false)) {
                        this.textbox.draw(objectValues); }
                    if (objectValues.name === 'fixedobject') this.ctx.restore();
                }
        },

        img: {
            init:
                function (initValues) {
                    this.ready = false;  // to prevent to use wrong values
                    this.image = new Image();
                    if (initValues.name === 'background') this.image.src = getValue(initValues.src,"");
                    else this.image.src = getValue1(initValues.imgproperties, "src", "");
                    this.initValues = initValues;
                    if ( !this.image.complete ) return;
                    this.init2();
                },
            init2:
                function () {
                    var initValues = this.initValues;
                    var r1 = getValue(this.image.width, 0);
                    var r2 = getValue(this.image.height, 0);
                    if ( r1 == 0 ) return;
                    // Look if size attribute overrides the image size
                    this.size = getValue(initValues.size, [null, null]);
                    this.r1 = getValue(this.size[0], r1);
                    if ( this.size[0] && !this.size[1] ) r2 = this.r1/r1*r2;
                    this.r2 = getValue(this.size[1], r2);
                    this.size[0] = this.r1;
                    this.size[1] = this.r2;
                    this.ready = true;
                    if (initValues.name === 'background')  return;
                    initValues.r1 = this.r1;
                    initValues.r2 = this.r2;
                    initValues.x = getValue(initValues.position[0], 0);
                    initValues.y = getValue(initValues.position[1], 0);
                    if (this.pin) this.pinInit(initValues);
                    if (this.imgproperties.textbox) this.textbox.init(initValues);
                },

            draw:
                function (objectValues) {
                    if (!this.image.complete) {
                        scope.incompleteImages =+ 1;
                        return;
                    }
                    if ( !this.ready ) this.init2();

                    this.ctx = objectValues.ctx;
                    if (objectValues.pinPosition) {
                        this.imageX = - objectValues.pinPosition.off.x - objectValues.pinPosition.x;
                        this.imageY = - objectValues.pinPosition.off.y - objectValues.pinPosition.y;
                    }
                    else {
                        this.imageX = getValue(objectValues.position[0], 0);
                        this.imageY = getValue(objectValues.position[1], 0);
                    }
                    this.ctx.save();
                    this.ctx.translate(this.imageX, this.imageY);
                    if (objectValues.name === 'fixedobject' || objectValues.name === 'background')
                        this.ctx.rotate(this.a * to_radians);
                    this.ctx.drawImage(this.image, 0, 0, this.r1, this.r2);
                    if (getValue1(objectValues.imgproperties, "textbox", false))
                        this.textbox.draw(objectValues);
                    this.ctx.restore();
                }
        },

        textbox: {
            init:
                function (initValues) {
                    this.borderGap = 3;
                    initValues.textboxproperties = getValue(initValues.textboxproperties, {});
                    this.type = initValues.type;
                    if (this.type === 'img' || this.type === 'vector') {
                        this.textBoxOffset = getValue1(initValues.textboxproperties.positionOnImage,
                            "coord", [0, 0]);
                        this.x = getValue( this.textBoxOffset[0], 0);
                        this.y = getValue( this.textBoxOffset[1], 0); }
                    var fontDraw = document.createElement("canvas");
                    this.font = getValue1(initValues.textboxproperties, "font", '14px Arial');
                    this.auxctx = fontDraw.getContext('2d');
                    this.auxctx.font = this.font;
                    this.text = getValue(initValues.textboxproperties.text, initValues.id);
                    this.textwidth = this.auxctx.measureText(this.text).width;
                    this.textHeight = parseInt(this.font, 10);
                    this.textBoxSize = getValue(initValues.textboxproperties.size, []);
                    this.r1 = getValue(this.textBoxSize[0], this.textwidth + 2 * this.borderGap);
                    this.r2 = getValue(this.textBoxSize[1], this.textHeight + 2 * this.borderGap);
                    this.borderColor = getValue(initValues.textboxproperties.borderColor, 'Black');
                    this.fillColor = getValue(initValues.textboxproperties.fillColor, 'White');
                    this.textColor = getValue(initValues.textboxproperties.textColor, 'Black');
                    this.cornerRadius = getValue(initValues.textboxproperties.cornerradius, 0);
                    if (this.cornerRadius > this.r1 / 2 || this.cornerRadius > this.r2 / 2) {
                        if (this.cornerRadius > this.r1 / 2) this.cornerRadius = this.r1 / 2;
                        if (this.cornerRadius > this.r2 / 2) this.cornerRadius = this.r2 / 2; }
                    initValues.r1 = this.r1;
                    initValues.r2 = this.r2;
                    initValues.x = this.x;
                    initValues.y = this.y;
                    if (this.pin) {
                        this.pinInit(initValues);
                    }

                },

            draw:
                function (objectValues) {
                    this.ctx = objectValues.ctx;
                    this.ctx.font = getValue(this.font, '14px Arial');
                    this.ctx.textBaseline = 'top';
                    if (objectValues.type === 'textbox' && objectValues.name === 'dragobject') {
                        this.textBoxX = - objectValues.pinPosition.off.x - objectValues.pinPosition.x;
                        this.textBoxY = - objectValues.pinPosition.off.y - objectValues.pinPosition.y;
                    }
                    else if (objectValues.name === 'fixedobject') {
                        this.textBoxX = objectValues.x;
                        this.textBoxY = objectValues.y;
                    }
                    else {
                        this.textBoxX = objectValues.textbox.textBoxOffset[0];
                        this.textBoxY = objectValues.textbox.textBoxOffset[1]; }

                    this.ctx.save();
                    this.ctx.translate(this.textBoxX, this.textBoxY);
                    if (objectValues.name === 'fixedobject') this.ctx.rotate(this.a * to_radians);
                    this.ctx.beginPath();
                    this.ctx.moveTo(this.cornerRadius - 1, 0);
                    this.ctx.lineTo(this.r1 - this.cornerRadius, 0);
                    this.ctx.arc(this.r1 - this.cornerRadius, this.cornerRadius,
                        this.cornerRadius, 1.5 * Math.PI, 0);
                    this.ctx.lineTo(this.r1, this.r2 - this.cornerRadius);
                    this.ctx.arc(this.r1 - this.cornerRadius, this.r2 - this.cornerRadius,
                        this.cornerRadius, 0, 0.5 * Math.PI);
                    this.ctx.lineTo(this.cornerRadius, this.r2);
                    this.ctx.arc(this.cornerRadius, this.r2 - this.cornerRadius,
                        this.cornerRadius, 0.5 * Math.PI, Math.PI);
                    this.ctx.lineTo(0, this.cornerRadius);
                    this.ctx.arc(this.cornerRadius, this.cornerRadius,
                        this.cornerRadius, Math.PI, 1.5 * Math.PI);
                    this.ctx.fillStyle = this.fillColor;
                    this.ctx.fill();
                    this.ctx.fillStyle = this.textColor;
                    this.ctx.fillText(this.text, this.borderGap, this.borderGap);
                    this.ctx.lineWidth = getValue(objectValues.textboxproperties.borderWidth, 2);
                    this.ctx.strokeStyle = this.borderColor;
                    this.ctx.stroke();
                    this.ctx.restore();
                }
        },

        pin: {
            init:
                function (initValues) {
                    this.pinProperties = getValue(initValues.pinPoint, {});
                    this.pinProperties.visible = getValue1(initValues.pinPoint, "visible", true);
                    this.pinProperties.position = getValue1(initValues.pinPoint, "position", {});

                    this.pinLength = getValue(this.pinProperties.length, 10);
                    this.pinPositions = {
                        west: {x: 0, y: this.r2 / 2, off: {x: -this.pinLength, y: 0}},
                        east: {x: this.r1, y: this.r2 / 2, off: {x: this.pinLength, y: 0}},
                        north: {x: this.r1 / 2, y: 0, off: {x: 0, y: -this.pinLength}},
                        south: {x: this.r1 / 2, y: this.r2, off: {x: 0, y: this.pinLength}},
                        southeast: {x: this.r1, y: this.r2,
                            off: {x: this.pinLength, y: this.pinLength}},
                        northeast: {x: this.r1, y: 0,
                            off: {x: this.pinLength, y: -this.pinLength}},
                        southwest: {x: 0, y: this.r2,
                            off: {x: -this.pinLength, y: this.pinLength}},
                        northwest: {x: 0, y: 0, off: {x: -this.pinLength, y: -this.pinLength}},
                        center: {x: this.r1 / 2, y: this.r2 / 2, off:{x: 0, y: 0}}
                    };
                    this.pinPositionAlign = getValue2(initValues.pin, "position", "align", 'northwest');
                    this.pinPosition = getValue(this.pinPositions[this.pinPositionAlign],
                        this.pinPositions.northwest);
                    this.pinProperties.position.coord = getValue(this.pinProperties.position.coord, []);
                    this.pinPosition.off.x = getValue(this.pinProperties.position.coord[0],
                        this.pinPosition.off.x);
                    this.pinPosition.off.y = getValue(this.pinProperties.position.coord[1],
                        this.pinPosition.off.y);
                    if (this.pinProperties && this.pinProperties.visible) {
                        this.dotPosition = {x: this.pinPosition.x + this.pinPosition.off.x,
                            y: this.pinPosition.y + this.pinPosition.off.y}}
                    else this.dotPosition = {x: this.pinPosition.x, y: this.pinPosition.y}
                },

            draw:
                function (objectValues) {
                    this.ctx = objectValues.ctx;
                    this.ctx.save();
                    this.ctx.translate(objectValues.x, objectValues.y);
                    this.ctx.rotate(this.a * to_radians);
                    if (this.type === 'vector') {
                        this.vectorDraw(objectValues);
                        this.ctx.restore();
                        return;
                    }
                    this.ctx.strokeStyle = getValue(this.pinProperties.color,
                        getValue(objectValues.borderColor, 'blue'));
                    this.ctx.beginPath();
                    this.ctx.arc(0, 0, 1.5, 0, 2 * Math.PI, false);
                    this.ctx.stroke();
                    this.ctx.beginPath();
                    this.ctx.moveTo(0, 0);
                    this.ctx.lineWidth = getValue(objectValues.textboxproperties.borderWidth, 2);
                    this.ctx.lineTo(-this.pinPosition.off.x, -this.pinPosition.off.y);
                    this.ctx.stroke();
                    if (this.type === 'textbox') this.textbox.draw(objectValues);
                    if (this.type === 'img') this.imageDraw(objectValues);
                    this.ctx.restore();
                }
        }
    };

    var userObjects = scope.attrs.markup.objects;
    if (scope.attrs.state) {
        // lisätty oikeiden vastausten lukemiseen ja piirtämiseen.
        for (var i = 0; i < userObjects.length; i++) {
            if ( !userObjects[i] ) continue; // looks like the first may be null
            for (var j = 0; j < scope.attrs.state.markup.objects.objects.length; j++) {
                if (userObjects[i].id === scope.attrs.state.markup.objects.objects[j].id) {
                    userObjects[i].position[0] =
                        scope.attrs.state.markup.objects.objects[j].position[0];
                    userObjects[i].position[1] =
                        scope.attrs.state.markup.objects.objects[j].position[1]
                }
            }
        }
    }

    var userTargets = scope.attrs.markup.targets;
    var userFixedObjects = scope.attrs.markup.fixedobjects;
    var fixedobjects = [];
    var targets = [];
    var objects = [];



    if (scope.attrs.markup.background) {
        scope.attrs.markup.background.name = 'background';
        var background = new FixedObject(dt, scope.attrs.markup.background);
        dt.drawObjects.push(background);
    }

    if (userFixedObjects) {
        for (i = 0; i < userFixedObjects.length; i++) {
            fixedobjects[i] = new FixedObject(dt, userFixedObjects[i]);
        } }

    if (userTargets) {
        for (i = 0; i < userTargets.length; i++) {
            targets[i] = new Target(dt, userTargets[i]);
        } }

    if (userObjects) {
        for (i = 0; i < userObjects.length; i++) {
            objects[i] = new DragObject(dt, userObjects[i]);
            if(!scope.drags){
                scope.drags = [];
            }
            scope.drags.push(objects[i]);
        } }




    for (i = 0; i < fixedobjects.length; i++) {
        dt.drawObjects.push(fixedobjects[i]);
    }

    for (i = 0; i < targets.length; i++) {
        dt.drawObjects.push(targets[i]);
    }

    for (i = 0; i < objects.length; i++) {
        dt.drawObjects.push(objects[i]);
    }

    dt.draw();

// Katsotaan, onko oikeat vastaukset heitetty markuppiin. Jos on, piirretään oikeat raahaukset.
// pitäisi varmaan ottaa raahaus kokonaan pois päältä tässä tilanteessa.
    if(scope.attrs.state) {
        if (scope.attrs.state.markup.correctanswer) {
            //handle drawing lines.
            var rightdrags = scope.attrs.state.markup.correctanswer.rightanswers;
            var j = 0;
            for (j = 0; j < rightdrags.length; j++) {
                var p = 0;
                for (p = 0; p < userObjects.length; p++) {
                    if (userObjects[p].id === rightdrags[j].id) {
                        var values = {};
                        // get positions for drawing.
                        values.position = [];
                        values.position[0] = getValue(userObjects[p].position[0],0);
                        values.position[1] = getValue(userObjects[p].position[1],0);
                        values.endposition = [];
                        values.endposition[0] = getValue(rightdrags[j].position[0],0);
                        values.endposition[1] = getValue(rightdrags[j].position[1],0);
                        values.ctx = doc_ctx.getContext("2d");
                        //give context and values for draw function.
                        var line = new Line(dt,values);
                        dt.drawObjects.push(line);
                        values = {};
                    }
                }
            }
            // Poistetaan eventlistenerit canvaksesta jos oikea vastaus on annettu opiskelijalle.
            dt.removeEventListeners();
        }
    }
    dt.draw();
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
    $transclude(function(clone,scope) { timHelper.initAttributes(clone,$scope); });
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
    scope.app = imagexApp;

    // Etsitään kullekin attribuutille arvo joko scope.attrs tai attrs-parametrista. Jos ei ole, käytetään oletusta.
    timHelper.set(scope, attrs, "stem");
    timHelper.set(scope, attrs, "user_id");
    timHelper.set(scope, attrs, "button", "Save");
    timHelper.set(scope, attrs, "resetText", "Reset");
    timHelper.set(scope, attrs, "state.tries", 0);
    timHelper.set(scope, attrs, "max_tries");
    timHelper.set(scope, attrs, "cols", 20);
    timHelper.set(scope, attrs, "autoupdate", 500);
    timHelper.setn(scope, "tid", attrs, ".taskID"); // vain kokeilu että "juuresta" ottaminen toimii
    timHelper.set(scope,attrs,"background");
    // Tässä on nyt kaikki raahattavat objektit
    timHelper.set(scope,attrs,"objects","http://localhost/static/images/jyulogo.png");
    // Tässä pitäisi olla kaikki targetit
    timHelper.set(scope,attrs,"targets");
    timHelper.set(scope,attrs,"fixedobjects");

    timHelper.set(scope,attrs,"canvaswidth", 800);
    timHelper.set(scope,attrs,"canvasheight", 600);
    //timHelper.set(scope,attrs,"preview", false);
    timHelper.set(scope,attrs,"preview",scope.attrs.preview);

    // Otsikot.  Oletetaan että 1. elementti korvaatan header-otsikolla ja viimeinen footerilla
    element[0].childNodes[0].outerHTML = timHelper.getHeading(scope, attrs, "header", "h4");
    var n = element[0].childNodes.length;
    if (n > 1) element[0].childNodes[n - 1].outerHTML = timHelper.getHeading(scope, attrs, "footer", 'p class="plgfooter"');
    imagexApp.initDrawing(scope, element[0].childNodes[1].childNodes[0]);
    scope.canvas = element[0].childNodes[1].childNodes[0];
    scope.coords = element.find("#coords")[0];
    scope.canvas.coords = scope.coords;
    /*
     $(scope.canvas).bind('keydown', function(event) {
     if (event.ctrlKey || event.metaKey) {
     switch (String.fromCharCode(event.which).toLowerCase()) {
     case 'c':
     event.preventDefault();
     scope.coords.select();
     document.execCommand('copy');
     break;
     }
     }
     });
     */
//    imagexApp.initDrawing.DragTask(element[0].childNodes[1].childNodes[0]);
    scope.attrs = {}; // not needed any more
};


// Tehdään kaikista toiminnallisista funktioista oma luokka, jotta
// niitä ei erikseen lisätä jokaisen pluginin esiintymän kohdalla uudelleen.
function ImagexScope(scope) {
    "use strict";
    this.scope = scope;
}

ImagexScope.prototype.watchDrags = function() {
    "use strict";
    // var $scope = this.scope;
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

ImagexScope.prototype.showAnswer = function(){
    "use strict";
    this.doshowAnswer();

};

// Get the important stuff from dragobjects
ImagexScope.prototype.getDragObjectJson = function() {
    var $scope = this.scope;
    var dragtable = $scope.drags;
    var json = [];
    for(var i = 0; i < dragtable.length ; i++) {
        json.push({"id":dragtable[i].id,
            "position":[dragtable[i].x, dragtable[i].y]});
    }
    return json;
};


// This is pretty much identical to the normal save except that a query to
// show correct answer is also sent.
ImagexScope.prototype.doshowAnswer = function(){
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
            'drags' : this.getDragObjectJson(),
            'finalanswerquery' : true
        }
    };
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
    });
};

// Reset the positions of dragobjects.
ImagexScope.prototype.resetExercise = function(){
    "use strict";
    // Set scope.
    var $scope = this.scope;
    // Objects dragged by user.
    var dragtable = $scope.drags;
    // Original objects.
    var objects = $scope.objects;
    // Loop
    for(var i = 0; i< dragtable.length;i++) {
        for (var j = 0; j < objects.length; j++) {
            // If ID:s match set x and y for dragobject to be the original x and y.
            if (dragtable[i].id === objects[j].id) {
                dragtable[i].x = objects[j].position[0];
                dragtable[i].y = objects[j].position[1];
            }
        }
    }
    // Draw the excercise so that reset appears instantly.
    $scope.dt.draw();
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
    });

};