var timApp = angular.module('timApp', [
    'ngSanitize',
    'angularFileUpload',
    'ui.ace'].concat(modules)).config(['$httpProvider', function ($httpProvider) {
    var interceptor = [
        '$q',
        '$rootScope',
        function ($q, $rootScope) {
            var re = /\/[^/]+\/([^/]+)\/answer\/$/;
            var service = {
                'request': function (config) {
                    if (re.test(config.url)) {
                        var match = re.exec(config.url);
                        var taskId = match[1];
                        var ab = angular.element("answerbrowser[task-id='" + taskId + "']");
                        var browserScope = ab.isolateScope();
                        if (ab.scope().teacherMode) {
                            angular.extend(config.data, {abData: browserScope.getTeacherData()});
                        }
                        //console.log(config);
                    }
                    return config;
                },
                'response': function (response) {

                    if (re.test(response.config.url)) {
                        var match = re.exec(response.config.url);
                        var taskId = match[1];
                        $rootScope.$broadcast('answerSaved', {taskId: taskId});
                    }
                    return response;
                }
            };
            return service;
        }
    ];

    $httpProvider.interceptors.push(interceptor);
}]);