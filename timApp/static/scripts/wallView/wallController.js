/**
 * Created by hajoviin on 24.2.2015.
 */

timApp.controller("WallController", ['$scope', '$controller', "$http",

    function ($scope, controller, http) {

        $scope.msg = "";
        $scope.newMsg = "";
        $scope.showPoll = true;
        $scope.polling = true;
        $scope.latestID = null;
        $scope.requestOnTheWay = false;
        $scope.showWall = true;

        $scope.hide = function () {
            $scope.showWall = !$scope.showWall;
        }

        $scope.detach = function () {
            console.log("Should detach this window");
        }

        $scope.sendMessageEvent = function (message) {
            if (message.trim() == "") {
                alert("Can't send empty messages");
                return false;
            }

            http({
                url: '/sendMessage',
                method: 'POST',
                params: {'message': message}
            })
                .success(function () {
                    $scope.newMsg = "";
                })
                .error(function () {
                    console.log("Can't send message or something")
                });

        };

        $scope.startLongPolling = function (oldID) {
            function message_longPolling(lastid) {
                var t;

                if (lastid == null) {
                    lastid = -1;
                }
                $scope.requestOnTheWay = true;
                jQuery.ajax({
                        url: '/getMessages',
                        type: 'GET',
                        data: {id: lastid},
                        success: function (payload) {
                            $scope.requestOnTheWay = false;
                            clearInterval(t);
                            if ($scope.polling) {
                                $scope.latestID = payload.lastid;
                                if (payload.status == 'results' || payload.status == 'no-results') {

                                    t = setTimeout(function () {
                                        message_longPolling(payload.lastid);
                                    }, 1000);

                                    if (payload.status == 'results') {
                                        jQuery.each(payload.data, function (i, msg) {
                                            $scope.msg = $scope.msg + msg + "\n";
                                            $scope.$apply();
                                        });
                                        var textarea = document.getElementById('wallArea');
                                        textarea.scrollTop = textarea.scrollHeight;
                                    } else if (payload.status == 'no-results') {
                                        console.log("No new messages. Sending new poll.");
                                    }
                                } else if (payload.status == 'error') {
                                    alert("Something went wrong");
                                }
                            } else {
                                console.log("Got answer but not polling anymore.")
                            }
                        }
                        ,
                        error: function (payload) {
                            $scope.requestOnTheWay = false;
                            clearInterval(t);
                            t = setTimeout(function () {
                                message_longPolling(payload.lastid);
                            }, 15000);
                        }
                    }
                )
                ;
            }

            message_longPolling(oldID);
        };

        $scope.startLongPolling();

        $scope.enterPressed = function (event) {
            if (event.which === 13) {
                $scope.sendMessageEvent($scope.newMsg);
            }
        };

        $scope.pollChanged = function () {
            if (!$scope.requestOnTheWay) {
                if ($scope.polling) {
                    $scope.startLongPolling($scope.latestID);
                }
            }
        }


    }])
;