/**
 * Created by hajoviin on 24.2.2015.
 */

/* TODO: The correct name might be lecture controller, because wall is just a part of lecture */
timApp.controller("WallController", ['$scope', '$controller', "$http",

    function ($scope, controller, http) {

        $scope.msg = "";
        $scope.newMsg = "";
        $scope.showPoll = true;
        $scope.polling = true;
        $scope.requestOnTheWay = false;
        $scope.showWall = false;
        $scope.canStart = true;
        $scope.canStop = false;
        $scope.lectureId = null;
        $scope.showLecture = false;
        $scope.lectures = [];
        $scope.chosenLecture = "";


        $scope.checkIfInLecture = function () {
            http({
                url: '/checkLecture',
                method: 'GET',
                params: {'doc_id': $scope.docId}
            })
                .success(function (answer) {
                    if (answer.isInLecture) {
                        $scope.showLectureView(answer);
                    } else {
                        $scope.showBasicView(answer);
                    }
                })
        }

        $scope.checkIfInLecture();

        $scope.joinLecture = function () {
            if ($scope.chosenLecture == "") {
                console.log("You must choose");
                return;
            }

            http({
                url: '/joinLecture',
                method: 'POST',
                params: {'lecture_code': $scope.chosenLecture}
            })
                .success(function (answer) {
                    if (answer.inLecture) {
                        console.log(answer);
                        $scope.showLectureView(answer);
                    } else {
                        $scope.showBasicView(answer);
                    }
                })
        }

        $scope.toggleLecture = function () {
            $scope.showLecture = !$scope.showLecture
        }

        $scope.showLectureView = function (answer) {
            $scope.inLecture = true;
            $scope.lectureId = answer.lectureId;
            $scope.canStop = true;
            $scope.polling = true;
            if (!$scope.requestOnTheWay) {
                $scope.msg = "";
                $scope.getAllMessages();
            }
            document.getElementById("lectureName").innerText = answer.lectureCode;
        }

        $scope.showBasicView = function (answer) {
            $scope.canStart = true;
            $scope.canStop = false;
            $scope.polling = false;
            $scope.inLecture = false;
            $scope.lectureId = null;
            document.getElementById("lectureName").innerText = "Not running";
            if (answer.lectures != undefined) {
                jQuery.each(answer.lectures, function (i, lecture) {
                    if ($scope.lectures.indexOf(lecture) == -1) {
                        $scope.lectures.push(lecture);
                    }
                });
            }
        }

        $scope.createLecture = function () {
            var fail = false;

            if (document.getElementById("lCode").value == "") {
                document.getElementById("lCode").style.border = "1px solid red";
                document.getElementById("lCode").title = "You must type in something.";
                fail = true;
            }
            if (document.getElementById("dateChosen").checked == false && document.getElementById("dueChosen").checked == false) {
                document.getElementById("lbstart").style.border = "1px solid red";
                document.getElementById("lbstart").title = "You must select something.";
                fail = true;
            }
            if (document.getElementById("dateChosen2").checked == false && document.getElementById("durationChosen").checked == false) {
                document.getElementById("lbend").style.border = "1px solid red";
                document.getElementById("lbend").title = "You must select something.";
                fail = true;
            }

            if (fail) {
                return;
            }

            $scope.toggleLecture()


            var startDate = "" + $scope.startYear + "." + $scope.startMonth + "." + $scope.startDay + "|" + $scope.startHour + ":" + $scope.startMin;
            var endDate = "" + $scope.endYear + "." + $scope.endMonth + "." + $scope.endDay + "|" + $scope.endHour + ":" + $scope.endMin;

            http({
                url: '/createLecture',
                method: 'POST',
                params: {
                    'doc_id': $scope.docId, 'lecture_code': $scope.lectureCode, 'password': $scope.password,
                    'start_date': startDate, 'end_date': endDate
                }
            })
                .success(function (answer) {
                    $scope.checkIfInLecture();
                    console.log("Lecture created: " + answer.lectureId);
                })
                .error(function () {
                    console.log("Failed to start a lecture");
                })
        };
        $scope.deleteLecture = function () {
            http({
                url: '/deleteLecture',
                method: 'POST',
                params: {'doc_id': $scope.docId, lecture_id: $scope.lectureId}
            })
                .success(function () {
                    $scope.checkIfInLecture();
                    console.log("Lecture deleted");

                })
                .error(function () {
                    console.log("Failed to delete the lecture");
                })
        };

        $scope.leaveLecture = function () {
            http({
                url: '/leaveLecture',
                method: "POST",
                params: {'lecture_id': $scope.lectureId}
            })
                .success(function () {
                    $scope.checkIfInLecture();
                })
        }

        $scope.hide = function () {
            $scope.showWall = !$scope.showWall;
        };

        $scope.detach = function () {
            console.log("Should detach this window");
        };

        $scope.sendMessageEvent = function (message) {
            if (message.trim() == "") {
                alert("Can't send empty messages");
                return false;
            }

            http({
                url: '/sendMessage',
                method: 'POST',
                params: {'message': message, lecture_id: $scope.lectureId}
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
            console.log($scope.lectureId);
            http({
                url: '/getAllMessages',
                type: 'GET',
                params: {lecture_id: $scope.lectureId}
            })
                .success(function (answer) {
                    jQuery.each(answer.data, function (i, msg) {
                        $scope.msg = $scope.msg + msg + "\n";
                    });
                    $scope.lastID = answer.lastid;
                    var textarea = document.getElementById('wallArea');
                    textarea.scrollTop = textarea.scrollHeight;

                    t = setTimeout(function () {
                        $scope.startLongPolling($scope.lastID);
                    }, 1000);
                })
        };

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
                        data: {'client_message_id': lastID, lecture_id: $scope.lectureId},
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
                        error: function () {
                            $scope.requestOnTheWay = false;
                            clearInterval(t);
                            t = setTimeout(function () {
                                message_longPolling();
                            }, 30000);
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