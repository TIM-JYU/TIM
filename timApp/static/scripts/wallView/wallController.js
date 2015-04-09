/**
 * Created by hajoviin on 24.2.2015.
 */

timApp.controller("WallController", ['$scope', '$controller', "$http",

    function ($scope, controller, http) {

        $scope.msg = "";
        $scope.newMsg = "";
        $scope.showPoll = true;
        $scope.polling = true;
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
                .success(function (answer) {
                    $scope.newMsg = "";
                    var textarea = document.getElementById('wallArea');
                    textarea.scrollTop = textarea.scrollHeight;
                })
                .error(function () {
                    console.log("Can't send message or something")
                });

        };

        $scope.getAllMessages = function () {
            var t;
            jQuery.ajax({
                url: '/getAllMessages',
                type: 'GET',
                success: function (answer) {
                    jQuery.each(answer.data, function (i, msg) {
                        $scope.msg = $scope.msg + msg + "\n";
                        $scope.$apply();
                    });
                    $scope.lastID = answer.lastid;
                    var textarea = document.getElementById('wallArea');
                    textarea.scrollTop = textarea.scrollHeight;

                    t = setTimeout(function () {
                        $scope.startLongPolling($scope.lastID);
                    }, 1000);
                }
            });
        };

        $scope.getAllMessages();

        $scope.startLongPolling = function (lastID) {
            function message_longPolling(lastID) {
                var t;

                if (lastID == null) {
                    lastID = -1;
                }

                $scope.requestOnTheWay = true;
                jQuery.ajax({
                        url: '/getMessages',
                        type: 'GET',
                        data: {id: lastID},
                        success: function (answer) {

                            $scope.requestOnTheWay = false;
                            clearInterval(t);
                            if ($scope.polling) {
                                if (answer.status == 'results' || answer.status == 'no-results') {

                                    t = setTimeout(function () {
                                        message_longPolling(answer.lastid);
                                    }, 1000);

                                    if (answer.status == 'results') {
                                        jQuery.each(answer.data, function (i, msg) {
                                            $scope.msg = $scope.msg + msg + "\n";
                                            $scope.$apply();
                                        });
                                        $scope.lastID = answer.lastid;
                                        var textarea = document.getElementById('wallArea');
                                        var areaHeight = $("#wallArea").height();
                                        if (textarea.scrollHeight - textarea.scrollTop - areaHeight < 200) {
                                            textarea.scrollTop = textarea.scrollHeight;
                                        }


                                    } else if (answer.status == 'no-results') {
                                        console.log("No new messages. Sending new poll.");
                                    }
                                } else if (answer.status == 'error') {
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
                                message_longPolling();
                            }, 15000);
                        }
                    }
                )
                ;
            }

            message_longPolling(lastID);
        };

        $scope.enterPressed = function (event) {
            if (event.which === 13) {
                $scope.sendMessageEvent($scope.newMsg);
            }
        };

        $scope.pollChanged = function () {
            if (!$scope.requestOnTheWay) {
                if ($scope.polling) {
                    $scope.startLongPolling($scope.lastID);
                }
            }
        }


    }])
;