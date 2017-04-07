import {timApp} from "tim/app";

timApp.factory('Users', ['$http', '$window', '$q', '$httpParamSerializer', ($http, $window, $q, $httpParamSerializer) => {
    class UserService {
        current: any; // currently logged in user
        group: any; // any additional users that have been added in the session - this does not include the main user

        constructor(current, group) {
            this.current = current;
            this.group = group;
        }

        getCurrent() {
            return this.current;
        }

        getSessionUsers() {
            return this.group;
        }

        isKorppi(): boolean {
            return this.current.name.indexOf('@') < 0;
        }

        logout(user, logoutFromKorppi) {
            $http.post('/logout', {user_id: user.id}).then((response) => {
                this.group = response.data.other_users;
                this.current = response.data.current_user;
                if (!this.isLoggedIn()) {
                    if (logoutFromKorppi) {
                        this.korppiLogout(function () {
                            $window.location.reload();
                        });
                    } else {
                        $window.location.reload();
                    }
                }
            });
        }

        isLoggedIn() {
            return this.current.id > 0;
        }

        addUser() {
            this.group.push({'id': 0, 'real_name': 'Meikäläinen Matti', 'name': 'mameikal'});
        }

        korppiLogin(addUser) {
            const target_url = '/korppiLogin';
            const separator = target_url.indexOf('?') >= 0 ? '&' : '?';
            const came_from_raw = $window.came_from || '';
            const came_from = encodeURIComponent(came_from_raw.replace("#", "%23"));
            const anchor_raw = $window.anchor || window.location.hash.replace('#', '');
            const anchor = encodeURIComponent(anchor_raw);
            const redirectFn = function () {
                $window.location.replace(target_url + separator + $httpParamSerializer({
                        came_from: came_from,
                        anchor: anchor,
                        add_user: addUser
                    }));
            };
            if (addUser) {
                this.korppiLogout(redirectFn);
            } else {
                redirectFn();
            }
        }

        korppiLogout(redirectFn) {
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
                        url: 'https://korppi.jyu.fi/openid/manage/manage',
                        headers: {
                            'Content-Type': 'application/x-www-form-urlencoded'
                        },
                        data: $httpParamSerializer({logout: 'Logout'})
                    }).finally(redirectFn);
            });
        }

        loginWithEmail(email, password, addUser, successFn) {
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
                }).then((response) => {
                this.group = response.data.other_users;
                this.current = response.data.current_user;
                successFn();
                if (!addUser) {
                    $window.location.reload();
                }
            }, function (response) {
                $window.alert(response.data.error);
            });
        }
    }
    return new UserService($window.current_user, $window.other_users);
}]);
