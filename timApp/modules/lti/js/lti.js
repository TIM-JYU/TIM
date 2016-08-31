/**
 * Created by pealkasa on 20.7.2016.
 */
"use strict";
var ltiApp = angular.module('ltiApp', ['ngSanitize']);
ltiApp.TESTWITHOUTPLUGINS = false; // if one wants to test without pali plugins

ltiApp.directive('ltiRunner',['$sanitize','$compile',
                  function ($sanitize,$compile1) {"use strict";
                      // Tätä kutsutaan yhden kerran kun plugin otetaan käyttöön
                      timHelper.sanitize = $sanitize;
                      ltiApp.sanitize = $sanitize;
                      ltiApp.compile = $compile1;
                      return ltiApp.directiveFunction(); }]
);

ltiApp.directiveFunction = function() {
"use strict";
// Koska tätä kutsutaan direktiivistä, tätä kutsutaan yhden kerran

    return {
		scope: {},
		controller: ltiApp.Controller,
		link: ltiApp.initScope,
		restrict: 'AE',
		transclude: true,
		replace: 'true',
		template: ltiApp.directiveTemplate()
	};
};


ltiApp.directiveTemplate = function () {
"use strict";
// Koska tätä kutsutaan directiveFunction-metodista, tätä kutsutaan yhden kerran
    if ( ltiApp.TESTWITHOUTPLUGINS ) return '';
	/* return '<div><p>Direktiivifunktion ulosanti on tässä</p></div>'; */

    // Buttons pertinent to the Moodle iframe
    var iframe_buttons = '' +
        '<div class="lti lti-buttons">' +
            // Active state
            '<button ng-if="' + "state === 'active'" + '" class="btn btn-success" ng-click="reload()">' +
                'Aloita alusta' +
            '</button>' +
            // Default state
            '<button ng-if="' + "state === 'default'" + '" class="btn btn-success" ng-click="run()">' +
                'Aloita' +
            '</button>' +
            // Inactive state
            '<button ng-if="' + "state === 'inactive'" + '" class="btn btn-danger" ng-click="forceOpen()">' +
                'Lopeta aiempi ja aloita' +
            '</button>' +
            // Grades
            '<button class="btn btn-info" ng-if="use_js && use_lti" ng-click="updateGrade()">' +
                'Päivitä pisteet' +
            '</button>' +
            // Anchor scroll
            '<button ng-if="' + "state === 'inactive'" + '" class="btn btn-default" ng-click="goBack()">' +
                'Palaa aiempaan' +
            '</button>' +
        '</div>';

    // Buttons pertinent to saving and other answerbrowser things
    var answerbrowser_buttons = '' +
        '<div ng-if="use_js && use_lti" class="lti lti-buttons">' +
            // Save
            '<button class="btn btn-success" ng-if="button" ng-disabled="disableSaving" ng-click="ltiScope.saveGrade();">' +
                '{{button}}' +
            '</button>' +
        '</div>';

    // Buttons to allow user to resize the <iframe>
    var resizing_buttons = '' +
        '<div ng-show="resizingButtonsDiv()" class="lti lti-buttons">' +
            '<button ng-hide="iframeControls" class="btn btn-default" ng-click="toggleIframeControls()">' +
                'Avaa ikkunan asetukset' +
            '</button>' +
            '<button ng-show="iframeControls" class="btn btn-default" ng-click="toggleIframeControls()">' +
                'Sulje ikkunan asetukset' +
            '</button>' +
        '</div>';

    // HTML templating is for frauds
    return '' +
    '<div class="csRunDiv no-popup-menu" id="{{::parentId}}">' +

        '<p>{{header}}</p>' +
        '<p ng-if="stem" class="stem" >{{stem}}</p>' +

        '<iframe ng-show="iframe.src" src="{{::iframe.src}}" class="lti" id="{{::iframe.id}}" name="{{::iframe.name}}" ' +
            'style="width:{{::iframe.size[0]}}; height: {{::iframe.size[1]}};">' +
        '</iframe>' +

        '<div class="lti lti-text">' +
            '<p ng-if="!use_lti" class="login-text">' +
                'Jos et ole kirjautunut Moodleen, ' +
                    '<button ng-click="openLoginPage()">' +
                        'kirjaudu ensin sisään' +
                    '</button>. ' +
                'Tämän jälkeen voit aloittaa tehtävän painamalla "Aloita".</p>' +
                '<p ng-if="!use_lti" class="login-text">Jos jokin muu tehtävä tulee esiin, voit siirtyä oikeean tehtävään "Aloita" tai "Aloita alusta" painikkeilla.' +
            '</p>' +
            '<pre class="lti" ng-if="isInactive()">{{::inactiveText}}</pre>' +
            '<pre ng-if="use_js && use_lti" class="lti">Pisteesi tehtävästä: {{grade}}</pre>' +
        '</div>' +

        '<form novalidate ng-show="iframeControls" ng-submit="resizeIframe([iframe_width, iframe_height])" class="lti lti-resizing">' +
            '<label for="iframe_width">Leveys:</label>' +
            '<input type="text" name="iframe_width" id="iframe_width" ng-model="iframe_width">' +
            '<label for="iframe_height">Korkeus:</label>' +
            '<input type="text" name="iframe_height" id="iframe_height" ng-model="iframe_height">' +
            '<input type="submit" value="Muuta">' +
        '</form>' +

        '<div class="lti lti-button-container">' +
            iframe_buttons +
            resizing_buttons +
            answerbrowser_buttons +
        '</div>' +

        '<span class="tries" ng-if="max_tries"> Tries: {{tries}}/{{max_tries}}</span>' +

        '<pre class="" ng-if="error">{{error}}</pre>' +
        '<pre class="" ng-show="result">{{result}}</pre>' +

        '<p class="plgfooter">Here comes footer</p>' +

    '</div>';
};


ltiApp.container = {
    __scopes: [],
    __activate: function(index) {
        this.__scopes = this.__scopes.map(function(scope, idx, arr) {
            scope.state = ((idx === index) ? 'active' : 'inactive');
            return scope;
        });
    },
    __deactivate: function(index) {
        this.__scopes = this.__scopes.map(function(scope) {
            scope.state = 'default';
            return scope;
        });
    },
    getActiveState: function() {
        for (var idx = 0; idx < this.__scopes.length; idx++) {
            if (this.__scopes[idx].state === 'active') {
                return this.__scopes[idx];
            }
        }
        return undefined;
    },
    forceCloseActive: function() {
        if (this.getActiveState()) {
            return this.getActiveState().close();
        }
        console.log("Error: trying to close active scope, but no scope is active!");
        return;
    },
    register: function(scope) {
        var idx;
        for (idx = 0; idx < this.__scopes.length; idx++) {
            if (this.__scopes[idx] === scope) {
                console.log("Error: already registered scope!");
                return;
            }
        }
        scope.state = (this.getActiveState() ? 'inactive' : 'default');
        scope.index = idx;
        scope.iframe.id = "lti_iframe_" + idx;
        scope.iframe.name = scope.iframe.id;
        scope.parentId = "lti_plugin_" + idx;
        this.__scopes.push(scope);
    },
    update: function(scope, state) {
        state = state || 'default';
        for (var idx = 0; idx < this.__scopes.length; idx++) {
            if (this.__scopes[idx] === scope) {
                if (state === 'active') {
                    // a scope has been activated -> set everything else as 'inactive'
                    this.__activate(idx);
                } else if (state === 'default' && this.__scopes[idx].state === 'active') {
                    // a scope has been deactivated -> set everything as 'default'
                    this.__deactivate(idx);
                } else {
                    this.__scopes[idx].state = state;
                }
                return;
            }
        }
        console.log("Error: called update() with state '" + state + "'. Scope was not registered.");
    }
};

ltiApp.Controller = function($scope, $http, $transclude, $interval, $sce, $anchorScroll, $window) {
"use strict";
    // Tätä kutsutaan kerran jokaiselle pluginin esiintymälle.
    // Angular kutsuu tätä koska se on sanottu direktiivifunktiossa Controlleriksi.
    // Tähän tullaan ensin ja sitten initScope-metodiin
    // Siitä ei ole mitään hajua mistä se keksii tälle nuo parametrit???
    if (ltiApp.TESTWITHOUTPLUGINS) return;
    $scope.ltiScope = new LTIScope($scope);
    $scope.attrs = {};
    $scope.http = $http;
    $scope.interval = $interval;

    // Luodaan $scope.attrs joka on avattuna sisällössä olev JSON tai HEX
    $transclude(function(clone,scope) { timHelper.initAttributes(clone,$scope); });

    $scope.errors = [];
    $scope.muokattu = false;
    $scope.result = "";

    // <iframe> related attributes and CSS styling
    $scope.iframe = {
        'src': '',
        'id': '',
        'name': '',
        'size': []
    };
    $scope.disableSaving = true; // allow saving?
    $scope.lastSaved = undefined; // last saved grade value
    $scope.iframeControls = false; // show <iframe> controls panel
    $scope.parentId = ""; // parent <div> id
    $scope.inactiveText = ""; // what text to display if <iframe> is inactive

    function getLocalIframeSettings() {
        var settings = $window.localStorage.getItem("local_iframe_settings");
        settings = (settings && JSON.parse(settings)) || {};
        return Array.isArray(settings) ? {} : settings; // compat. fix
    }

    function setLocalIframeSettings(dimensions) {
        dimensions = dimensions || $scope.iframe.size;
        var settings = getLocalIframeSettings();
        settings[$scope.iframe.id] = $scope.iframe.size;
        console.log(settings);
        $window.localStorage.setItem("local_iframe_settings", JSON.stringify(settings));
    }

    function forceSettingsUpdate() {
        // Handle localStorage saving of <iframe> dimensions
        // localStorage doesn't store it as an array (instead as a string), so we use split
        var settings = getLocalIframeSettings();
        if (settings && settings[$scope.iframe.id]) {
            $scope.iframe.size = settings[$scope.iframe.id];
            $scope.resizeIframe();
        }
    }

    // whether to display iframe options to user
    $scope.resizingButtonsDiv = function() {
        return $scope.iframe.src !== '';
    };

    // Toggle hide/show for iframe settings panel
    $scope.toggleIframeControls = function() {
        var settings = getLocalIframeSettings();
        var size = settings && settings[$scope.iframe.id];
        $scope.iframeControls = !$scope.iframeControls;
        $scope.iframe.size = size || $scope.iframe_size || []; // iframe_size is a YAML param
        // These are the ng-model versions for the settings form (yes, it's hacky)
        // It will display default values for easier editing.
        $scope.iframe_width = $scope.iframe.size[0] || "";
        $scope.iframe_height = $scope.iframe.size[1] || "";
    };

    // Handle <iframe> resizing via the settings panel
    $scope.resizeIframe = function(dimensions) {
        var settings = getLocalIframeSettings();
        var iframeInScope = document.getElementById($scope.iframe.id);
        $scope.iframe.size = dimensions || $scope.iframe.size;
        // If <iframe> is currently in scope and therefore visible
        if (iframeInScope) {
            iframeInScope.style.width = $scope.iframe.size[0];
            iframeInScope.style.height = $scope.iframe.size[1];
        }
        setLocalIframeSettings($scope.iframe.size);
    };

    // Scroll the user's browser to the location of the active iframe
    $scope.goBack = function() {
        var active = ltiApp.container.getActiveState();
        var active_markup = active.attrs.markup || active;
        var iframe = document.getElementById(active_markup.iframe.id);
        $anchorScroll(iframe.parentNode.id);
    };

    // Validate grade
    // TODO: use max_grade if provided.
    $scope.gradeIsValid = function(grade) {
        grade = grade || $scope.grade; // use $scope.grade if no input given
        var gradeFloat = parseFloat(grade);
        return !isNaN(gradeFloat) && isFinite(gradeFloat) && gradeFloat >= 0;
    };

    $scope.updateGrade = function() {
        $http.get("../lti/grades?hash=" + $scope.hash)
            .then(function(res) {
                var newGrade = res.data;
                var lastSaved = $scope.lastSaved;
                $scope.disableSaving = true;
                if ($scope.gradeIsValid(newGrade) && newGrade !== lastSaved) {
                    $scope.disableSaving = false;
                    newGrade = parseFloat(newGrade).toFixed(2);
                }
                $scope.grade = newGrade;
                console.log($scope.grade);
            });
    };

    // Called when "Aja" button is pressed.
    $scope.run = function() {
        var url = ($scope.use_lti ?
                   '../lti/getform?hash=' + $scope.hash :
                   '../lti/getlink?page=' + $scope.view_url);
        url = $sce.trustAsResourceUrl(url);
        $scope.iframe.src = url;
        var iframeInScope = document.getElementById($scope.iframe.id);
        if (iframeInScope) {
            iframeInScope.src = url;
        }
        ltiApp.container.update($scope, 'active');
        forceSettingsUpdate();
    };

    // Open Moodle login page in <iframe>
    $scope.openLoginPage = function() {
        var url = $sce.trustAsResourceUrl('../lti/getlink?page=' + $scope.login_url);
        if (!$scope.iframe.src) {
            $scope.iframe.src = url;
        } else {
            $scope.reload(url, false); // Forces the <iframe> to load login page always.
        }
        forceSettingsUpdate();
    };

    // Handle <iframe> closing
    $scope.close = function() {
        var is_sure = confirm("Menetät työsi lopettamalla tehtävän. Oletko varma?");
        if (is_sure) {
            $scope.iframe.src = "";
            ltiApp.container.update($scope, 'default');
        }
        return is_sure;
    };

    // Handle <iframe> "reloading"
    $scope.reload = function(url, confirmation) {
        url = url || $scope.iframe.src;
        if (confirmation === true || confirmation === undefined) {
            var is_sure = confirm("Menetät työsi aloittamalla tehtävän uudestaan. Oletko varma?");
        }
        if (!confirmation || is_sure) {
            document.getElementById($scope.iframe.id).src = url;
        } else {
            $scope.iframe.src = url;
        }
    };

    // Forcibly open $scope's <iframe> (typically called via the ltiApp.container)
    $scope.forceOpen = function() {
        if (ltiApp.container.forceCloseActive()) {
            $scope.run();
        }
    };

    // Is $scope's <iframe> inactive?
    $scope.isInactive = function() {
        if ($scope.state === 'active' || $scope.state === 'default') {
            $scope.inactiveText = "";
            return false;
        }
        $scope.inactiveText = "Toinen Moodle ikkuna on jo auki!\n" +
            "Voit sulkea sen ja aloittaa uuden tai palata aiempaan.";
        return true;
    };


    // Register scope with global container object
    // Must be done last since we need to use the various $scope methods and variables.
    ltiApp.container.register($scope);
};


ltiApp.initScope = function (scope, element, attrs) {
"use strict";
// Tätä kutsutaan kerran jokaiselle pluginin esiintymälle.
// Angular kutsuu tätä koska se on sanottu direktiivifunktiossa Link-metodiksi.
    scope.cursor = "\u0383"; //"\u0347"; // "\u02FD";
    scope.plugin = element.parent().attr("data-plugin");
    scope.taskId = element.parent().attr("id");

    // Etsitään kullekin attribuutille arvo joko scope.attrs tai attrs-parametrista. Jos ei ole, käytetään oletusta.
    timHelper.set(scope, attrs, "stem");
    timHelper.set(scope, attrs, "hash", undefined); // if 'use_lti' is true
    timHelper.set(scope, attrs, "view_url", undefined);
    timHelper.set(scope, attrs, "button", "Tallenna");
    //timHelper.set(scope, attrs, "max_grade", 1);
    timHelper.set(scope, attrs, "initgrade", "ei pisteitä");
    timHelper.set(scope, attrs, "state.grade", scope.initgrade);
    //timHelper.set(scope, attrs, "state.oldgrade", -1);
    timHelper.set(scope, attrs, "use_js", false);
    timHelper.set(scope, attrs, "use_lti", true);
    timHelper.set(scope, attrs, "iframe_size", ["100%", "600px"]);
    timHelper.set(scope, attrs, "login_url", "https://moodle.jyu.fi/login/jyulogin.php"); // JY Moodlen login page as default
    timHelper.set(scope, attrs, "state.tries", 0);
    timHelper.set(scope, attrs, "max_tries");
    timHelper.setn(scope, "tid", attrs, ".taskID"); // vain kokeilu että "juuresta" ottaminen toimii

    // Otsikot.  Oletetaan että 1. elementti korvaatan header-otsikolla ja viimeinen footerilla
    element[0].childNodes[0].outerHTML = timHelper.getHeading(scope, attrs, "header", "h4");
    var n = element[0].childNodes.length;
    if (n > 1) element[0].childNodes[n - 1].outerHTML = timHelper.getHeading(scope, attrs, "footer", 'p class="plgfooter"');

   scope.attrs = {}; // not needed any more
};


function LTIScope(scope) {
    "use strict";
    this.scope = scope;
}

LTIScope.prototype.initCode = function() {
    "use strict";
    var $scope = this.scope;
    $scope.muokattu = false;
    $scope.grade = $scope.initgrade;
    $scope.error = "";
    $scope.result = "";
};

LTIScope.prototype.saveGrade = function() {
    "use strict";
    this.doSaveGrade(false);
};

LTIScope.prototype.doSaveGrade = function(nosave) {
    "use strict";

    var $scope = this.scope;

    // Only if grade is a sensible value should we be allowed to save anything
    if ($scope.gradeIsValid() === false) {
        return;
    }

    $scope.error = "... saving ...";
    $scope.isRunning = true;
    $scope.disableSaving = true;

    $scope.result = "";

    var grade = "";

    if ($scope.grade) grade = parseFloat($scope.grade);

    var params = {
        'input': {
            'grade': grade,
            'markup': {
                'taskId': $scope.taskId,
                'user_id': $scope.user_id
            }
        }
    };

    console.log("taskId", $scope.taskId);
    console.log("params", params);

    if (nosave) params.input.nosave = true;
    var url = "/lti/answer";
    if ($scope.plugin) {
        url = $scope.plugin;
        var i = url.lastIndexOf("/");
        if (i > 0) url = url.substring(i);
        url += "/" + $scope.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
    }

    var put_params = {
        method: 'PUT',
        url: url,
        data: params,
        headers: {
            'Content-Type': 'application/json'
        },
        timeout: 20000
    };

    $scope.http(put_params)
        .success(function (data, status, headers, config) {
            $scope.isRunning = false;
            $scope.lastSaved = $scope.grade;
            $scope.error = data.web.error;
            $scope.result = data.web.result;
            $scope.tries = data.web.tries;
            console.log("new tries: " + $scope.tries);
        }).error(function (data, status) {
            $scope.isRunning = false;
            $scope.errors.push(status);
            $scope.error = "Ikuinen silmukka tai jokin muu vika?";
            // $scope.error = data;
        });
};
