requirejs.config({
    baseUrl: '/static/scripts/node_modules',
    paths: {
        tim: '/static/scripts/build',
        angular: 'angular/angular',
        moment: 'moment/moment',
        'humanize-duration': 'humanize-duration/humanize-duration',
        'angular-messages': 'angular-messages/angular-messages',
        'angular-timer': 'angular-timer/dist/angular-timer',
        'angular-eonasdan-datetimepicker': 'angular-eonasdan-datetimepicker/dist/angular-eonasdan-datetimepicker',
        'angular-ui-bootstrap': 'angular-ui-bootstrap/dist/ui-bootstrap',
        'angular-sanitize': 'angular-sanitize/angular-sanitize',
    },
    shim: {}
});

requirejs(['tim/app'], function (timApp) {

});
