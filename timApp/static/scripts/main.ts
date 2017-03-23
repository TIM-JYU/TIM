//noinspection TsLint
requirejs.config({
    baseUrl: "/static/scripts/build",
    paths: {
        // TIM modules
        "tim": ".",

        // 3rd party modules
        "ace": "../node_modules/ace/lib/ace",
        "angular": "../node_modules/angular/angular",
        "angular-eonasdan-datetimepicker": "../node_modules/angular-eonasdan-datetimepicker/dist/angular-eonasdan-datetimepicker",
        "angular-messages": "../node_modules/angular-messages/angular-messages",
        "angular-sanitize": "../node_modules/angular-sanitize/angular-sanitize",
        "angular-timer": "../node_modules/angular-timer/dist/angular-timer",
        "angular-ui-bootstrap": "../node_modules/angular-ui-bootstrap/dist/ui-bootstrap-tpls",
        "chartjs": "../node_modules/chart.js/dist/Chart",
        "eonasdan-bootstrap-datetimepicker": "../node_modules/eonasdan-bootstrap-datetimepicker/build/js/bootstrap-datetimepicker.min",
        "head": "../reveal/lib/js/head.min",
        "humanize-duration": "../node_modules/humanize-duration/humanize-duration",
        "jquery": "../node_modules/jquery/dist/jquery",
        "jqueryui": "../node_modules/jquery-ui-dist/jquery-ui",
        "jqueryui-touch-punch": "../node_modules/jquery-ui-touch-punch-c/jquery.ui.touch-punch",
        "katex": "../node_modules/katex/dist/katex.min",
        "katex-auto-render": "../node_modules/katex/dist/contrib/auto-render.min",
        "ng-file-upload": "../node_modules/ng-file-upload/dist/ng-file-upload",
        "ngstorage": "../node_modules/ngstorage/ngstorage",
        "oclazyload": "../node_modules/oclazyload/dist/oclazyload",
        "rangyinputs": "../node_modules/rangyinputs/rangyinputs-jquery-src",
        "reveal": "reveal/js/reveal",
        "ui-grid": "../node_modules/angular-ui-grid/ui-grid",

        // plugin modules
        "cs": "/cs",
        "mcq": "/mcq",
        "mmcq": "/mmcq",
        "static/scripts/imagex": "imagex",
        "static/scripts/timHelper": "timHelper",
        "svn": "/svn",
    },
    packages: [{
        name: "moment",
        location: "../node_modules/moment",
        main: "moment",
    }],
    shim: {
        // 3rd party modules
        "angular": {exports: "angular", deps: ["jquery"]},
        "angular-eonasdan-datetimepicker": {deps: ["angular", "eonasdan-bootstrap-datetimepicker"]},
        "angular-messages": {deps: ["angular"]},
        "angular-sanitize": {deps: ["angular"]},
        "angular-timer": {deps: ["angular"]},
        "angular-ui-bootstrap": {deps: ["angular"]},
        "head": {exports: "head"},
        "jqueryui": {deps: ["jquery"]},
        "katex-auto-render": {deps: ["katex"]},
        "ng-file-upload": {deps: ["angular"]},
        "oclazyload": {deps: ["angular"]},
        "ui-grid": {deps: ["angular"]},

        // plugin modules
        "cs/js/dir": {deps: ["angular"]},
        "mcq/script2": {deps: ["angular", "mcq/SimpleDirective"]},
        "mcq/SimpleDirective": {exports: "globals"},
        "mmcq/script2": {deps: ["angular", "mmcq/SimpleDirective"]},
        "mmcq/SimpleDirective": {exports: "globals"},
        "svn/video/js/video": {deps: ["angular"]},
    },
});

requirejs([
    "angular",
    "tim/timHelper",
    "tim/controllers/answerToQuestionController",
    "tim/controllers/breadcrumbs",
    "tim/controllers/createLectureCtrl",
    "tim/controllers/indexCtrl",
    "tim/controllers/lectureController",
    "tim/controllers/lectureInfoController",
    "tim/controllers/questionController",
    "tim/controllers/questionPreviewController",
    "tim/controllers/reviewController",
    "tim/controllers/showStatisticsToQuestionController",
    "tim/controllers/sidebarMenuCtrl",
    "tim/controllers/smallMenuCtrl",
    "tim/controllers/startController",
    "tim/controllers/userlistController",
    "tim/controllers/view/viewctrl",
    "tim/directives/annotation",
    "tim/directives/answerbrowser3",
    "tim/directives/bookmarks",
    "tim/directives/bootstrapPanel",
    "tim/directives/createItem",
    "tim/directives/loginMenu",
    "tim/directives/parEditor",
    "tim/directives/popUpDialog",
    "tim/directives/rightsEditor",
    "tim/directives/velpSelection",
    "tim/extramodules",
    "tim/loadMap",
    "tim/manageView/manageCtrl",
    "tim/settingsView/settingsCtrl",
], (angular, timHelper) => {
    // strictDi: true would break plugins until they are fixed
    angular.bootstrap(document, ["timApp"], {strictDi: false});
    timHelper.sanitizeService = angular.element(document).injector().get("$sanitize");
});
