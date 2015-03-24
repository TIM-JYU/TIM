/**
 * Created by hajoviin on 24.2.2015.
 */

var WallController = angular.module('controller', []);

WallController.controller("WallController", ['$scope', '$controller', "$http",

    function ($scope, controller, http) {

        $scope.test = "Hey!";
        $scope.count = 0;
        $scope.msg = "";
        $scope.newMsg = "";
        $scope.showPoll = true;
        $scope.polling = false;

        $scope.toggle = function () {
            alert("Test1");
        };

        $scope.sendMessageEvent = function (message) {
            var today = new Date();
            if (message.trim() == "") {
                alert("Can't send empty messages");
                return false;
            }

            http({
                url: '/sendMessage',
                method: 'POST',
                params: {'message': message}
            })
                .success(function (payload) {
                    $scope.newMsg = "";
                })
                .error(function () {
                    console.log("Can't send message or something")
                });

        };

        $scope.getMessages = function () {
            var message = "Stupid test message";
            return message;
        };

        $scope.startLongPolling = function () {
            $scope.showPoll = false;
            if (!$scope.polling) {
                $scope.polling = true;
                function message_longPolling(timestamp, lastid) {
                    var t;

                    if (lastid == null) {
                        lastid = -1;
                    }

                    jQuery.ajax({
                        url: '/getMessages',
                        type: 'GET',
                        data: {id: lastid},
                        success: function (payload) {
                            clearInterval(t);

                            if (payload.status == 'results' || payload.status == 'no-results') {
                                t = setTimeout(function () {
                                    message_longPolling(payload.timeStamp, payload.lastid);
                                }, 1000);
                                if (payload.status == 'results') {
                                    jQuery.each(payload.data, function (i, msg) {
                                        $scope.msg = msg + "\n" + $scope.msg;
                                        $scope.$apply();
                                    });
                                } else if (payload.status == 'no-results') {
                                    console.log("No new messages. Sending new poll")
                                }
                            } else if (payload.status == 'error') {
                                alert("Something went wrong");
                            }
                        }
                        ,
                        error: function (payload) {
                            clearInterval(t);
                            t = setTimeout(function () {
                                message_longPolling(payload.timestamp, payload.lastid);
                            }, 15000);
                        }
                    });


                }

                message_longPolling(new Date);
            }
        }

        $scope.enterPressed = function(event) {
            if (event.which === 13) {
                $scope.sendMessageEvent($scope.newMsg);
            }
        };


    }]);