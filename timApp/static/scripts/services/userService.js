var angular;
var timApp = angular.module('timApp');

timApp.factory('Users', ['$http', '$window', function ($http, $window) {
    "use strict";
    var userService = {};
    userService.current = $window.current_user; // currently logged in user
    userService.group = []; // any additional users that have been added in the session

    userService.getCurrent = function () {
        return userService.current;
    };

    userService.getSessionUsers = function () {
        return userService.group;
    };

    userService.logout = function (user) {
        $http.post('/logout', {user_id: user.id}).then(function (response) {
            userService.group = response.data.session_users;
            userService.current = response.data.current_user;
            $window.location.reload();
        });
    };

    userService.isLoggedIn = function () {
        return userService.current.id > 0;
    };

    userService.addUser = function () {
        userService.group.push({'id': 0, 'real_name': 'Meikäläinen Matti', 'name': 'mameikal'});
    };

    userService.korppiLogin = function () {
        var target_url = '/korppiLogin';
        var separator = target_url.indexOf('?') >= 0 ? '&' : '?';
        var came_from_raw = $window.came_from || '';
        var came_from = encodeURIComponent(came_from_raw.replace("#", "%23"));
        var anchor_raw = $window.anchor || window.location.hash.replace('#', '');
        var anchor = encodeURIComponent(anchor_raw);
        //$("#anchor").val(anchor_raw);
        window.location.replace(target_url + separator + 'came_from=' + came_from + '&anchor=' + anchor);
    };

    return userService;
}]);
