var angular;
var timApp = angular.module('timApp');

timApp.factory('Users', ['$http', '$window', '$q', '$httpParamSerializer', function ($http, $window, $q, $httpParamSerializer) {
    "use strict";
    var userService = {};
    userService.current = $window.current_user; // currently logged in user
    userService.group = $window.other_users; // any additional users that have been added in the session - this does not include the main user

    userService.getCurrent = function () {
        return userService.current;
    };

    userService.getSessionUsers = function () {
        return userService.group;
    };

    userService.isKorppi = function () {
        return userService.current.name.indexOf('@') < 0;
    };

    userService.logout = function (user, logoutFromKorppi) {
        $http.post('/logout', {user_id: user.id}).then(function (response) {
            userService.group = response.data.other_users;
            userService.current = response.data.current_user;
            if (!userService.isLoggedIn()) {
                if (logoutFromKorppi) {
                    userService.korppiLogout(function () {
                        $window.location.reload();
                    });
                } else {
                    $window.location.reload();
                }
            }
        });
    };

    userService.isLoggedIn = function () {
        return userService.current.id > 0;
    };

    userService.addUser = function () {
        userService.group.push({'id': 0, 'real_name': 'Meikäläinen Matti', 'name': 'mameikal'});
    };

    userService.korppiLogin = function (addUser) {
        var target_url = '/korppiLogin';
        var separator = target_url.indexOf('?') >= 0 ? '&' : '?';
        var came_from_raw = $window.came_from || '';
        var came_from = encodeURIComponent(came_from_raw.replace("#", "%23"));
        var anchor_raw = $window.anchor || window.location.hash.replace('#', '');
        var anchor = encodeURIComponent(anchor_raw);
        var redirectFn = function () {
            $window.location.replace(target_url + separator + $httpParamSerializer({
                    came_from: came_from,
                    anchor: anchor,
                    add_user: addUser
                }));
        };
        if (addUser) {
            userService.korppiLogout(redirectFn);
        } else {
            redirectFn();
        }
    };

    userService.korppiLogout = function (redirectFn) {
        $http.get('https://korppi.jyu.fi/kotka/portal/showLogout.jsp',
            {
                withCredentials: true,
                // the request is disallowed with the default custom headers (see base.html), so we disable them
                headers: {
                    'If-Modified-Since': undefined,
                    'Cache-Control': undefined,
                    'Pragma': undefined
                }
            }).finally(function () {
            $http(
                {
                    withCredentials: true,
                    method: 'POST',
                    url: 'https://openid.korppi.jyu.fi/openid/manage/manage',
                    headers: {
                        'Content-Type': 'application/x-www-form-urlencoded'
                    },
                    data: $httpParamSerializer({logout: 'Logout'})
                }).finally(redirectFn);
        });
    };

    userService.loginWithEmail = function (email, password, addUser, successFn) {
        $http(
            {
                method: 'POST',
                url: '/altlogin',
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded',
                    'X-Requested-With': 'XMLHttpRequest'
                },
                data: $httpParamSerializer({
                    email: email,
                    password: password,
                    add_user: addUser
                })
            }).then(function (response) {
            userService.group = response.data.other_users;
            userService.current = response.data.current_user;
            successFn();
            if (!addUser) {
                $window.location.reload();
            }
        }, function (response) {
            $window.alert(response.data.error);
        });
    };

    return userService;
}]);
