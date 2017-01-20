/**
 * Created by vesal on 28.12.2016.
 */
"use strict";
var qstApp = angular.module('qstApp', ['ngSanitize']);
qstApp.TESTWITHOUTPLUGINS = false; // if one wants to test without qst plugins

qstApp.directive('qstRunner',['$sanitize','$compile',
                  function ($sanitize,$compile1) {"use strict";
                      // Tätä kutsutaan yhden kerran kun plugin otetaan käyttöön
                      timHelper.sanitize = $sanitize;
                      qstApp.sanitize = $sanitize;
                      qstApp.compile = $compile1;
                      return qstApp.directiveFunction(); }]
);


qstApp.directiveFunction = function() {
"use strict";
// Koska tätä kutsutaan direktiivistä, tätä kutsutaan yhden kerran
    return {
		scope: {},
		controller: qstApp.Controller,
		link: qstApp.initScope,
		restrict: 'AE',
		/*
		compile: function(tElement, attrs) {
			var content = tElement.children();
		},
		*/
		transclude: true,
		replace: 'true',
		template: qstApp.directiveTemplate()
		// templateUrl: 'html/qstTempl.html'
	};
};


qstApp.directiveTemplate = function () {
"use strict";
// Koska tätä kutsutaan directiveFunction-metodista, tätä kutsutaan yhden kerran
    if ( qstApp.TESTWITHOUTPLUGINS ) return '';
	return  '<div class="csRunDiv qst no-popup-menu">' +
				  '<p>Here comes header</p>' +
				  '<p ng-if="stem" class="stem" >{{stem}}</p>' +
                  '<dynamic-answer-sheet  control="dynamicAnswerSheetControl"></dynamic-answer-sheet>' +
				  '<button class="timButton" ng-if="button"  ng-disabled="isRunning" ng-click="qstScope.saveText();">{{button}}</button>&nbsp&nbsp' +
                  '<a class="questionAddedNew" ng-show="qstScope.checkQstMode()"><span class="glyphicon glyphicon-question-sign" title="Ask question"></span></a>' +
                  '<span ng-show="result">{{result}}</span>' +
    		      '<p class="plgfooter">Here comes footer</p>' +
              '</div>';
};


qstApp.Controller = function($scope, $http, $transclude, $interval) {
"use strict";
// Tätä kutsutaan kerran jokaiselle pluginin esiintymälle.
// Angular kutsuu tätä koska se on sanottu direktiivifunktiossa Controlleriksi.
// Tähän tullaan ensin ja sitten initScope-metodiin
// Siitä ei ole mitään hajua mistä se keksii tälle nuo parametrit???
    if (qstApp.TESTWITHOUTPLUGINS) return;
    $scope.qstScope = new QstScope($scope);
    $scope.attrs = {};
    $scope.http = $http; 
    $scope.interval = $interval;

    // Luodaan $scope.attrs joka on avattuna sisällössä olev JSON tai HEX
    $transclude(function(clone,scope) { timHelper.initAttributes(clone,$scope);  });

    $scope.errors = [];
	$scope.muokattu = false;
    $scope.result = "";
    $scope.dynamicAnswerSheetControl = {};
    $scope.preclass = "qst"

};


qstApp.initScope = function (scope, element, attrs) {
"use strict";
// Tätä kutsutaan kerran jokaiselle pluginin esiintymälle.
// Angular kutsuu tätä koska se on sanottu direktiivifunktiossa Link-metodiksi.
    scope.cursor = "\u0383"; //"\u0347"; // "\u02FD";
    scope.plugin = element.parent().attr("data-plugin");
    scope.taskId = element.parent().attr("id");

    // Etsitään kullekin attribuutille arvo joko scope.attrs tai attrs-parametrista. Jos ei ole, käytetään oletusta.
    timHelper.set(scope, attrs, "stem");
    timHelper.set(scope, attrs, "user_id");
    timHelper.set(scope, attrs, "button", "Save");
    timHelper.set(scope, attrs, "resetText", "Reset");
    timHelper.setn(scope, "tid", attrs, ".taskID"); // vain kokeilu että "juuresta" ottaminen toimii

    // Otsikot.  Oletetaan että 1. elementti korvaatan header-otsikolla ja viimeinen footerilla
    element[0].childNodes[0].outerHTML = timHelper.getHeading(scope, attrs, "header", "h4");
    var n = element[0].childNodes.length;
    if (n > 1) element[0].childNodes[n - 1].outerHTML = timHelper.getHeading(scope, attrs, "footer", 'p class="plgfooter"');

    // scope.stem = scope.attrs.markup.json.questionText;
    var markup = scope.attrs.markup;
    var params = {};
    params.answerTable = scope.attrs.state;
    params.questionId = 1; //args.questionId;
    params.questionParId = 1; // args.questionParId;
    params.questionParIdNext = 2; //args.questionParIdNext;
    params.isLecturer = false;
    params.markup = markup;
    params.questionTitle = markup.json.title;
    params.points = markup.points;
    params.expl = markup.expl;
    // var preview = element.parents('.previewcontent').length > 0;
    params.preview =  false; // scope.$parent.previewUrl; // Released;
    params.result = true;
    params.answclass = "qstAnswerSheet";
    params.noDisable = true;
    scope.dynamicAnswerSheetControl.createAnswer(params);
    scope.attrs = {}; // not needed any more

};


// Tehdään kaikista toiminnallisista funktoista oma luokka, jotta
// niitä ei erikseen lisätä jokaisen pluginin esiintymän kohdalla uudelleen.
function QstScope(scope) {
"use strict";
    this.scope = scope;
}


QstScope.prototype.initCode = function() {
"use strict";
    var $scope = this.scope;
    $scope.muokattu = false;
    $scope.error = "";
    $scope.result = "";
};


QstScope.prototype.saveText = function() {
"use strict";
    this.doSaveText(false);
};


QstScope.prototype.checkQstMode = function(nosave) {
"use strict";
    var $scope = this.scope;
    return window.in_lecture || window.lectureMode; //  $scope.$parent.$parent.wallName; // TODO: better check if in lecture page
}

QstScope.prototype.doSaveText = function(nosave) {
"use strict";
    var $scope = this.scope;
    $scope.error = "... saving ...";
    $scope.isRunning = true;

    $scope.result = "";

    var answers = $scope.dynamicAnswerSheetControl.getAnswers();

    var params = {
        'input': {
            'answers': answers,
            'markup': {'taskId': $scope.taskId, 'user_id': $scope.user_id},
        }
    };

    if (nosave) params.input.nosave = true;
    var url = "/qst/answer";
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
        if (data.web.markup && data.web.show_result) {
            var params = {};
            params.answerTable =data.web.state;
            params.questionId = 1; //args.questionId;
            params.questionParId = 1; // args.questionParId;
            params.questionParIdNext = 2; //args.questionParIdNext;
            params.isLecturer = false;
            params.markup = data.web.markup;
            params.questionTitle = data.web.markup.json.title;
            params.points = data.web.markup.points;
            params.expl = data.web.markup.expl;
            // var preview = element.parents('.previewcontent').length > 0;
            params.preview =  false; // scope.$parent.previewUrl; // Released;
            params.result = true;
            params.answclass = "qstAnswerSheet";
            params.noDisable = true;

            $scope.dynamicAnswerSheetControl.createAnswer(params);

        }
    }).error(function (data, status) {
        $scope.isRunning = false;
        $scope.errors.push(status);
        $scope.error = "Ikuinen silmukka tai jokin muu vika?";
        // $scope.error = data;
    });
};
