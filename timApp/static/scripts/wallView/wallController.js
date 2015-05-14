/**
 * Created by hajoviin on 24.2.2015.
 */
/* TODO: The correct name might be lecture controller, because wall is just a part of lecture */
timApp.controller("WallController", ['$scope', '$controller', "$http", "$window", 'createDialog', '$rootScope', '$timeout',

    function ($scope, controller, http, $window, createDialog, $rootScope, $timeout) {

        $scope.lectureStartTime = "";
        $scope.lectureEndTime = "";
        $scope.lectureName = "";
        $scope.msg = "";
        $scope.newMsg = "";
        $scope.wallName = "Wall";
        $scope.messageInfo = true;
        $scope.newMessagesAmount = 0;
        $scope.newMessagesAmountText = "";
        $scope.showPoll = true;
        $scope.polling = true;
        $scope.requestOnTheWay = false;
        $scope.showWall = false;
        $scope.canStart = true;
        $scope.canStop = false;
        $scope.showLectureCreation = false;
        $scope.lectures = [];
        $scope.futureLectures = [];
        $scope.futureLecture = "";
        $scope.chosenLecture = "";
        $scope.passwordQuess = "";
        $scope.pollingLectures = [];
        $scope.useDate = false;
        $scope.useDuration = false;
        $scope.dateChosen = false;
        $scope.durationChosen = false;
        $scope.durationHour = "";
        $scope.durationMin = "";
        $scope.isLecturer = false;
        $scope.lectureId = null;
        $scope.showAnswerDummy = false;
        $scope.showStudentAnswers = false;
        $scope.studentTable = [];
        $scope.lecturerTable = [];
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
        };

        $scope.$on('get_lectureId', function () {
            $scope.$emit('getLectureId', $scope.lectureId);
        });

        $scope.$on("closeAnswer", function (event, answer) {
            $scope.showAnswerDummy = false;
            var mark = "";
            var answerString = "";
            angular.forEach(answer.answer, function (singleAnswer) {
                answerString += mark + singleAnswer;
                mark = "|"
            });
            http({
                url: '/answerToQuestion',
                method: 'POST',
                params: {
                    'question_id': answer.questionId,
                    'answers': answerString
                }
            })
                .success(function () {
                })
                .error(function () {
                    console.log("Failed to answer to question");
                })
        });

        $scope.$on("closeAnswerShow", function () {
            $scope.showStudentAnswers = false;
        });

        $scope.showInfo = function () {
            if (!$scope.messageInfo) {
                $scope.messagesWithInfo = $scope.msg;
                var lines = $scope.msg.split(/\r\n|\r|\n/g);
                var modifiedMessages = "";
                for (var i = 0; i < lines.length - 1; i++) {
                    var endInfo = lines[i].indexOf(">:");
                    modifiedMessages += ">" + lines[i].substring(endInfo + 2) + "\r\n";
                }
                $scope.msg = modifiedMessages;
            } else {
                $scope.msg = $scope.messagesWithInfo;
            }
        };

        $scope.checkIfInLecture();

        $scope.joinLecture = function () {
            if ($scope.chosenLecture == "") {
                $window.alert("Choose lecture to join");
                return;
            }

            http({
                url: '/joinLecture',
                method: 'POST',
                params: {
                    'doc_id': $scope.docId,
                    'lecture_code': $scope.chosenLecture,
                    'password_quess': $scope.passwordQuess
                }
            })
                .success(function (answer) {
                    $scope.passwordQuess = "";
                    if (!answer.correctPassword) {
                        document.getElementById("passwordInput").style.border = "1px solid red";
                        document.getElementById("passwordInput").placeholder = "Wrong password";
                    } else {
                        document.getElementById("passwordInput").style.border = "1px solid black";
                        document.getElementById("passwordInput").placeholder = "Access code";
                        if (answer.inLecture) {
                            $scope.showLectureView(answer);
                        } else {
                            $scope.showBasicView(answer);
                        }
                    }
                })
        };

        $scope.checkDown = function (e) {
            $scope.mouseDownX = e.clientX;
            $scope.mouseDownY = e.clientY;
            var wall = document.getElementById("wall");
            wall.style.position = "absolute";
            wall.style.bottom = 'auto';

        };

        $scope.checkUp = function (e) {
            var mouseUpX = e.clientX;
            var mouseUpY = e.clientY;

            var wall = document.getElementById("wall");
            wall.style.position = "fixed";

            if (Math.abs(Math.sqrt(Math.pow(($scope.mouseDownX - mouseUpX), 2) + Math.pow(($scope.mouseDownY - mouseUpY), 2))) < 2) {
                $scope.hide();
                return;
            }

            var id = $(event.target).attr('id');
        };


        $scope.toggleLecture = function () {
			$('#currentList').hide();
			$('#futureList').hide();
            createDialog('../../../static/templates/start_lecture.html', {
                    id: 'createL',
                    title: '',
                    footerTemplate: " ",
                    controller: 'CreateLectureCtrl'/*,
                     footerTemplate:
                     '<div class="buttons">' +
                     '<button id="btnSubmitLecture" type="button" ng-click="submitLecture()">Submit</button>' +
                     '<button type="button" ng-click="cancelCreation()">Cancel</button>' +
                     '</div>'*/
                },
                {
                    docIdParam: $scope.docId,
                    anotherScope: $scope
                });
        };

        $scope.startFutureLecture = function () {
            http({
                url: '/startFutureLecture',
                method: 'POST',
                params: {'doc_id': $scope.docId, 'lecture_code': $scope.futureLecture.lecture_code}
            })
                .success(function (answer) {
                    $scope.showLectureView(answer);
                })
        };

        $scope.showLectureView = function (answer) {
            $scope.isLecturer = answer.isLecturer;
            $scope.lectureName = answer.lectureCode;
            $scope.wallName = "Wall - " + answer.lectureCode;
            $scope.lectureStartTime = "Started: " + answer.startTime;
            $scope.lectureEndTime = "Ends: " + answer.endTime;
            $scope.inLecture = true;
            $scope.lectureId = answer.lectureId;
            $scope.polling = true;
            $scope.msg = "";
            $scope.showWall = true;

            $scope.getAllMessages();

            if (answer.isLecturer) {
                $scope.canStop = true;
                for (var i = 0; i < answer.students.length; i++) {
                    var student = {
                        name: answer.students[i].name,
                        active: answer.students[i].active
                    };
                    $scope.studentTable.push(student);
                }

                for (i = 0; i < answer.lecturers.length; i++) {
                    var lecturer = {
                        name: answer.lecturers[i].name,
                        active: answer.lecturers[i].active
                    };
                    $scope.lecturerTable.push(lecturer);
                }
            }
        };

        $scope.showBasicView = function (answer) {
            $scope.isLecturer = answer.isLecturer;
            if ($scope.isLecturer) {
                $scope.canStart = true;
                $scope.canStop = false;
            }
            $scope.polling = false;
            $scope.inLecture = false;
            $scope.lectureId = -1;
            $scope.lectureName = "Not running";
            $scope.showStudentAnswers = false;
            $scope.showAnswerDummy = false;
            $scope.lecturerTable = [];
            $scope.studentTable = [];

            angular.forEach(answer.lectures, function (lecture) {
                if ($scope.lectures.indexOf(lecture) == -1) {
                    $scope.lectures.push(lecture);
                }
            });

            angular.forEach(answer.futureLectures, function (lecture) {
                console.log(lecture);
                if ($scope.futureLectures.indexOf(lecture.lecture_code) == -1) {
                    $scope.futureLectures.push(lecture);
                }
            });

        };

        $scope.endLecture = function () {
            http({
                url: '/endLecture',
                method: 'POST',
                params: {'doc_id': $scope.docId, lecture_id: $scope.lectureId}
            })
                .success(function (answer) {
                    $scope.showBasicView(answer);
                    $scope.chosenLecture = "";
                    $scope.msg = "";
                    console.log("Lecture ended, not deleted");

                })
                .error(function () {
                    console.log("Failed to delete the lecture");
                })
        };

        $scope.deleteLecture = function () {
            http({
                url: '/deleteLecture',
                method: 'POST',
                params: {'doc_id': $scope.docId, lecture_id: $scope.lectureId}
            })
                .success(function (answer) {
                    $scope.showBasicView(answer);
                    $scope.lectures.splice($scope.lectureId, 1);
                    $scope.chosenLecture = "";
                    $scope.msg = "";
                    console.log("Lecture deleted");

                })
                .error(function () {
                    console.log("Failed to delete the lecture");
                })
        };

        $scope.leaveLecture = function () {
            $scope.msg = "";
            http({
                url: '/leaveLecture',
                method: "POST",
                params: {'lecture_id': $scope.lectureId, 'doc_id': $scope.docId}
            })
                .success(function (answer) {
                    $scope.showBasicView(answer)
                })
        };

        $scope.hide = function () {
            $scope.showWall = !$scope.showWall;
            //TODO: Do not use getElementById

            var elemWall = document.getElementById("wall");
            if (!$scope.showWall) {
                $scope.wallHeight = elemWall.style.height;
                elemWall.style.height = "";
                $scope.newMessagesAmount = 0;
                $scope.newMessagesAmountText = "(" + $scope.newMessagesAmount + ")";
            } else {
                elemWall.style.height = $scope.wallHeight;

            }
        };

        $scope.modifyLecture = function () {
            $scope.showLectureCreation = true;
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
                .success(function () {
                    $scope.newMsg = "";
                    //TODO: Fix this to scroll bottom without cheating.
                    var wallArea = $('#wallArea');
                    wallArea.animate({scrollTop: wallArea[0].scrollHeight * 10}, 1000);

                })
                .error(function () {
                    console.log("Can't send message or something");
                });

        };

        $scope.getAllMessages = function () {
            http({
                url: '/getAllMessages',
                type: 'GET',
                params: {lecture_id: $scope.lectureId}
            })
                .success(function (answer) {
                    angular.forEach(answer.data, function (msg) {
                        $scope.msg = $scope.msg + msg + "\n";
                    });

                    //TODO: Fix this to scroll bottom without cheating.
                    var wallArea = $('#wallArea');
                    wallArea.animate({scrollTop: wallArea[0].scrollHeight * 10}, 1000);

                    $scope.lastID = answer.lastid;
                    $scope.requestDocId = $scope.lectureId;

                    if ($scope.pollingLectures.indexOf(answer.lectureId) == -1) {
                        $scope.startLongPolling($scope.lastID);
                        $scope.pollingLectures.push(answer.lectureId);
                    }


                })
        };

        $scope.getLectureAnswers = function (answer) {
            http({
                url: '/getLectureAnswers',
                type: 'GET',
                params: {'question_id': answer.questionId, 'doc_id': $scope.docId, 'time': answer.latestAnswer}
            })
                .success(function (answer) {
                    $rootScope.$broadcast("putAnswers", {"answers": answer.answers});
                    $scope.getLectureAnswers(answer);

                })
                .error(function () {
                    console.log("Couldn't get answers");
                })
        };

        $scope.startLongPolling = function (lastID) {
            function message_longPolling(lastID) {
                var timeout;

                if (lastID == null) {
                    lastID = -1;
                }

                $scope.requestOnTheWay = true;
                http({
                    url: '/getUpdates',
                    type: 'GET',
                    params: {
                        'client_message_id': lastID,
                        'lecture_id': $scope.lectureId,
                        'doc_id': $scope.docId,
                        'is_lecturer': $scope.isLecturer
                    }
                })
                    .success(function (answer) {

                        if (!answer.isLecture) {
                            $scope.showBasicView(answer);
                            return;
                        }
                        $scope.pollingLectures.splice($scope.pollingLectures.indexOf(answer.lectureId), 1);
                        if (answer.lectureId != $scope.lectureId) {
                            return;
                        }

                        var oldUser = false;
                        for (var i = 0; i < answer.students.length; i++) {
                            for (var index = 0; index < $scope.studentTable.length; index++) {
                                if ($scope.studentTable[index].name == answer.students[i].name) {
                                    oldUser = true;
                                    $scope.studentTable[index].active = answer.students[i].active;
                                    break;
                                }
                            }

                            if (!oldUser) {
                                var student = {
                                    name: answer.students[i].name,
                                    active: answer.students[i].active
                                };
                                $scope.studentTable.push(student);
                            }
                        }

                        oldUser = false;
                        for (i = 0; i < answer.lecturers.length; i++) {
                            for (index = 0; index < $scope.lecturerTable.length; index++) {
                                if ($scope.lecturerTable[index].name == answer.lecturers[i].name) {
                                    oldUser = true;
                                    $scope.lecturerTable[index].active = answer.lecturers[i].active;
                                    break;
                                }
                            }

                            if (!oldUser) {
                                var lecturer = {
                                    name: answer.lecturers[i].name,
                                    active: answer.lecturers[i].active
                                };
                                $scope.lecturerTable.push(student);
                            }
                        }

                        if (answer.questionJson) {
                            var questionJson = JSON.parse(answer.questionJson);
                        }
                        if (answer.question) {
                            $scope.askedQuestionJson = questionJson;

                            if ($scope.isLecturer) {
                                $scope.showStudentAnswers = true;

                                //TODO: A bit confusing that waiting 1ms helps drawing the chart
                                $timeout(function () {
                                    $rootScope.$broadcast("createChart", questionJson);
                                }, 1);
                                $scope.getLectureAnswers(answer);
                            }


                            $rootScope.$broadcast("setQuestionJson", {
                                questionJson: questionJson,
                                questionId: answer.questionId
                            });


                            $scope.showAnswerDummy = true;
                        }
                        $scope.requestOnTheWay = false;
                        $window.clearTimeout(timeout);
                        if ($scope.polling) {

                            $scope.pollingLectures.push(answer.lectureId);
                            timeout = setTimeout(function () {
                                message_longPolling(answer.lastid);
                            }, 1000);

                            if (answer.status == 'results') {
                                angular.forEach(answer.data, function (msg) {
                                    if ($scope.messageInfo) {
                                        $scope.msg += msg + "\r\n";
                                    } else {
                                        var endInfo = msg.indexOf(">:");
                                        $scope.msg += ">" + msg.substring(endInfo + 2) + "\r\n";
                                        $scope.messagesWithInfo += msg + "\r\n";
                                    }

                                    if (!$scope.showWall) {
                                        $scope.newMessagesAmount++;
                                        $scope.newMessagesAmountText = "(" + $scope.newMessagesAmount + ")";
                                    }
                                });
                                $scope.lastID = answer.lastid;
                                var wallArea = $('#wallArea');
                                wallArea.scrollTop(wallArea[0].scrollHeight);
                            } else {
                                console.log("Sending new poll.");

                            }
                        } else {
                            console.log("Got answer but not polling anymore.")
                        }
                    })
                    .error(function () {
                        $scope.requestOnTheWay = false;
                        $window.clearTimeout(timeout);
                        timeout = setTimeout(function () {
                            message_longPolling();
                        }, 30000);
                    });
            }

            message_longPolling(lastID);
        };

        $scope.chatEnterPressed = function (event) {
            if (event.which === 13) {
                $scope.sendMessageEvent($scope.newMsg);
            }
        };

        $scope.passEnterPressed = function (event) {
            if (event.which === 13) {
                $scope.joinLecture();
            }
        };

        $scope.pollChanged = function () {
            if (!$scope.requestOnTheWay) {
                if ($scope.polling) {
                    $scope.startLongPolling($scope.lastID);
                }
            }
        };

        $scope.leftPadder = function (number, size) {
            var paddedNumber = "" + number;
            var len = paddedNumber.length;
            while (len < size) {
                paddedNumber = "0" + paddedNumber;
                len++;
            }
            return paddedNumber;

        };
    }
])
;

timApp.directive('notEmptyChange', function () {
    return {
        scrope: false,
        link: function (scope, element) {
            element.bind('change', function () {
                scope.isValid(element.context);
            });
            element.bind('blur', function () {
                scope.notEmpty(element.context);
            });
        }
    }
});

timApp.directive('isValidChange', function () {
    return {
        scrope: false,
        link: function (scope, element) {
            element.bind('blur', function () {
                scope.isValid(element.context);
            });
            element.bind('change', function () {
                scope.isValid(element.context);
            });
        }
    }
});

timApp.directive('isPositive', function () {
    return {
        scrope: false,
        link: function (scope, element) {
            element.bind('blur', function () {
                scope.isPositiveNumber(element.context);
            });
            element.bind('change', function () {
                scope.isPositiveNumber(element.context);
            });
        }
    }
});