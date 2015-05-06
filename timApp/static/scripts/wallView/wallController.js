/**
 * Created by hajoviin on 24.2.2015.
 */
/* TODO: The correct name might be lecture controller, because wall is just a part of lecture */
timApp.controller("WallController", ['$scope', '$controller', "$http", "$window", 'createDialog', '$rootScope',

    function ($scope, controller, http, $window, createDialog, $rootScope) {

        $scope.lectureStartTime = "";
        $scope.lectureEndTime = "";
        $scope.lectureName = "";
        $scope.msg = "";
        $scope.newMsg = "";
        $scope.showPoll = true;
        $scope.polling = true;
        $scope.requestOnTheWay = false;
        $scope.showWall = false;
        $scope.canStart = true;
        $scope.canStop = false;
        $scope.showLectureCreation = false;
        $scope.lectures = [];
        $scope.futureLectures = [];
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
            console.log(answer.answer);
            var mark = "";
            var answerString = "";
            angular.forEach(answer.answer, function (singleAnswer) {
                answerString += mark + singleAnswer;
                mark = "|"
            });
            console.log(answerString);
            http({
                url: '/answerToQuestion',
                method: 'POST',
                params: {
                    'question_id': answer.questionId,
                    'answers': answerString
                }
            })
                .success(function () {
                    console.log("Succesfully answered to question");
                })
                .error(function () {
                    console.log("Failed to answer to question");
                })
        });

        $scope.$on("closeAnswerShow", function () {
            $scope.showStudentAnswers = false;
        });


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
                            console.log(answer);
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
            createDialog('../../../static/templates/start_lecture.html', {
                    id: 'createL',
                    title: '',
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

        $scope.showLectureView = function (answer) {
            $scope.isLecturer = answer.isLecturer;
            $scope.lectureName = answer.lectureCode;
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

            angular.forEach(answer.lectures, function (lecture) {
                if ($scope.lectures.indexOf(lecture) == -1) {
                    $scope.lectures.push(lecture);
                }
            });

            angular.forEach(answer.futureLectures, function (lecture) {
                // TODO: Might be problematic in future, because needs to parse out the time before can do anyhting
                // with this. Dummy version.
                if ($scope.futureLectures.indexOf(lecture) == -1) {
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
                    // TODO: Find way to do this without getElementById
                    var textarea = document.getElementById('wallArea');
                    textarea.scrollTop = textarea.scrollHeight;
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

                    var textarea = document.getElementById('wallArea');
                    textarea.scrollTop = textarea.scrollHeight;
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
                params: {'question_id': answer.questionId, 'doc_id': $scope.docId}
            })
                .success(function (answer) {
                    $rootScope.$broadcast("putAnswers", {"answers": answer.answers});
                    $scope.getLectureAnswers(answer);
                    //TODO: Lopeta joskus

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

                        if (answer.questionJson) {
                            var questionJson = JSON.parse(answer.questionJson);
                        }
                        if (answer.question) {
                            if ($scope.isLecturer) {
                                $scope.getLectureAnswers(answer);
                                $scope.showStudentAnswers = true;
                            }
                            //else {

                            $scope.askedQuestionJson = questionJson;

                            $rootScope.$broadcast("setQuestionJson", {
                                questionJson: questionJson,
                                questionId: answer.questionId
                            });


                            $scope.showAnswerDummy = true;

                            /*
                             createDialog('../../../static/templates/questionAskedStudent.html', {
                             id: 'questionAskDialog',
                             title: 'Question',
                             backdrop: true,
                             success: {
                             label: 'Answer', fn: function () {

                             http({
                             url: '/answerQuestion',
                             method: 'POST',
                             params: {}
                             })
                             .success(function () {
                             console.log("Answered to the question");
                             })
                             .error(function (error) {
                             console.log(error);
                             });
                             }
                             },
                             controller: 'QuestionAnswerController'
                             },{
                             questionJson:questionJson
                             });

                             */

                            //}
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
                                    $scope.msg = $scope.msg + msg + "\n";
                                });
                                $scope.lastID = answer.lastid;
                                // TODO: Do no use getElementById. Doens't work well enough
                                var textarea = document.getElementById('wallArea');
                                var scrollHeight = textarea.scrollTop;
                                var position = textarea.scrollHeight - textarea.clientHeight;
                                if (scrollHeight / position < 0.9) {
                                    textarea.scrollTop = textarea.scrollHeight;
                                }


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