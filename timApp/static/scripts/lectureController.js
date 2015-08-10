/**
 * Created by hajoviin on 24.2.2015.
 * Contoller that handles lectures.
 * @module lectureController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

var angular;

var timApp = angular.module('timApp');

//TODO: Painike, josta voisi hakea kysymyksiä.
//TODO: Button, to get questions and wall.
timApp.controller("LectureController", ['$scope', '$controller', "$http", "$window", '$rootScope', '$timeout',

    function ($scope, controller, http, $window, $rootScope, $timeout) {
        "use strict";
        $scope.lectureStartTime = "";
        $scope.lectureEndTime = "";
        $scope.lectureName = "";
        $scope.msg = "";
        $scope.newMsg = "";
        $scope.wallName = "Wall";
        $scope.messageName = true;
        $scope.messageTime = true;
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
        $scope.showAnswerWindow = false;
        $scope.showStudentAnswers = false;
        $scope.studentTable = [];
        $scope.lecturerTable = [];
        $scope.gettingAnswers = false;
        $scope.answeredToLectureEnding = false;
        $scope.showLectureEnding = false;
        $scope.extend = {};
        $scope.extend.extendTime = "15";
        $scope.lectureEnded = false;
        $scope.showLectureForm = false;
        $scope.showLectureOptions = false;
        $scope.useQuestions = true;
        $scope.useWall = true;
        $scope.useAnswers = true;
        $scope.wallMessages = [];
        $scope.questionTitle = "";
        $scope.clockOffset = 0;

        var wall = $('#wall');
        var htmlMessageList = $('#wallMessageList');

        /**
         * Makes http request to check if the current user is in lecture.
         * @memberof module:lectureController
         */
        $scope.checkIfInLecture = function () {

            var showRightView = function (answer) {
                if (answer.isInLecture) {
                    $scope.showLectureView(answer);
                } else {
                    $scope.showBasicView(answer);
                }
            };

            http({
                url: '/checkLecture',
                method: 'GET',
                params: {'doc_id': $scope.docId, 'buster': new Date().getTime()}
            })
                .success(function (answer) {
                    var lectureCode = GetURLParameter('lecture');
                    if (lectureCode) {
                        http({
                            url: '/lectureNeedsPassword',
                            method: 'GET',
                            params: {
                                'doc_id': $scope.docId,
                                'lecture_code': lectureCode,
                                'buster': new Date().valueOf()
                            }
                        }).success(function (data) {
                            var changeLecture = $scope.joinLecture(lectureCode, data, answer.isInLecture, answer.lectureCode);
                            if (!changeLecture)
                                showRightView(answer);
                        }).error(function () {
                            $window.alert("Lecture " + lectureCode + " not found.");
                            showRightView(answer);
                        });
                    } else {
                        showRightView(answer);
                    }
                });
        };

        /**
         * Method to join selected lecture. Checks that lecture is chosen and sends http request to server.
         * @memberof module:lectureController
         * @param name Name of the lecture to be joined.
         * @param code_required True if lecture needs password, else false
         * @return {boolean} true, if successfully joined lecture, else false
         */
        $scope.joinLecture = function (name, code_required, in_lecture, current_lecture) {
            var changeLecture = true;
            if (current_lecture) {
                var inLecture = in_lecture || $scope.inLecture;
                if (inLecture) {
                    if (current_lecture == name) {
                        changeLecture = false;
                    } else {
                        changeLecture = $window.confirm("You are already in lecture " + current_lecture +
                            '. Do you want to switch to lecture ' + name + '?');
                    }
                }
            }
            if (!changeLecture) return false;

            if (code_required) $scope.passwordQuess = $window.prompt("Please enter a password:", "");
            if ($scope.chosenLecture === "" && name === "") {
                $window.alert("Choose lecture to join");
                return false;
            }

            var lectureName = "";
            if (angular.isDefined(name)) {
                lectureName = name;
                $('#currentList').slideUp();
            } else {
                lectureName = $scope.chosenLecture.lecture_code;
            }

            http({
                url: '/joinLecture',
                method: 'POST',
                params: {
                    'doc_id': $scope.docId,
                    'lecture_code': lectureName,
                    'password_quess': $scope.passwordQuess,
                    'buster': new Date().getTime()
                }
            })
                .success(function (answer) {
                    $scope.passwordQuess = "";
                    var input = $("#passwordInput");
                    if (!answer.correctPassword) {
                        $window.alert("Wrong access code!");
                        return false;
                    } else {
                        $scope.studentTable = [];
                        $scope.lecturerTable = [];
                        input.removeClass('errorBorder');
                        input.attr("placeholder", "Access code");
                        if ($scope.isLecturer) {
                            $scope.showLectureView(answer);
                            $scope.useWall = true;
                            $scope.useQuestions = true;
                        } else {
                            $scope.showLectureOptions = true;
                            $scope.lectureAnswer = answer;
                        }
                        return true;
                    }
                })
                .error(function () {
                    return false;
                });
        };

        $scope.checkIfInLecture();

        /**
         * Checks time offset between client server. Used to set question end time right.
         */
        $scope.getClockOffset = function () {
            http({
                url: '/getServerTime',
                method: 'GET',
                params: {'t1': new Date().valueOf()}
            }).success(function (data) {
                var t4 = new Date().valueOf();
                var t3 = parseInt(data.t3);
                var t2 = parseInt(data.t2);
                var t1 = parseInt(data.t1);
                $scope.clockOffset = ((t2 - t1) + (t3 - t4)) / 2;
                console.log('Clock offset: ', $scope.clockOffset);
            });
        };

        $scope.getClockOffset();

        /**
         * Use question_id in data to ask question as new.
         * Use asked_id to reask question already asked.
         */
        $scope.$on("askQuestion", function (event, data) {
            $scope.json = data.json;
            var args = {
                lecture_id: data.lecture_id,
                doc_id: data.doc_id,
                buster: new Date().getTime()
            };
            if (data.asked_id) args['asked_id'] = data.asked_id;
            else if (data.question_id) args['question_id'] = data.question_id;
            http({
                url: '/askQuestion',
                method: 'POST',
                params: args,
                buster: new Date().getTime()
            })
                .success(function (id) {
                    $scope.showStudentAnswers = true;
                    if ($scope.useAnswers) {
                        // Because of dynamic creation needs to wait 1ms to ensure that the directive is made(maybe?)
                        $timeout(function () {
                            $rootScope.$broadcast("createChart", $scope.json);
                            $scope.showStudentAnswers = false;
                        }, 1);

                        var answer = {"askedId": id};
                        $scope.getLectureAnswers(answer);
                    }
                    $rootScope.$broadcast("setQuestionJson", {
                        questionJson: $scope.json,
                        questionId: data.question_id,
                        askedId: id,
                        isLecturer: $scope.isLecturer,
                        askedTime: new Date().valueOf() + $scope.clockOffset,
                        clockOffset: $scope.clockOffset
                    });
                    $scope.showAnswerWindow = true;
                })
                .error(function (error) {
                    $window.console.log(error);
                });
        });

        /*
         Event listener for getLectureId. Emits the lectureId back
         */
        $scope.$on('getLectureId', function () {
            $scope.$emit('postLectureId', $scope.lectureId);
        });

        $scope.$on('joinLecture', function (event, lecture) {
            $scope.joinLecture(lecture.lecture_code, lecture.is_access_code, $scope.inLecture,
                $scope.lectureName);
        });

        $scope.$on('changeQuestionTitle', function (event, data) {
            $scope.questionTitle = data.title;
        });

        $scope.$on('toggleQuestion', function (event, data) {
            $scope.questionShown = !$scope.questionShown;
        });

        /*
         Event listener for getLecture. Emits the boolean value if the user is in lecture
         */
        $scope.$on('getInLecture', function () {
            $scope.$emit('postInLecture', $scope.inLecture);
        });

        /*
         Event listener for getIsLecturer event. Emits value if the user is lecturer
         */
        $scope.$on('getIsLecturer', function () {
            $scope.$emit('postIsLecturer', $scope.isLecturer);
        });

        $scope.$on('showAnswers', function (x) {
            $scope.showStudentAnswers = x;
        });

        /*
         Event listener for closeLectureForm. Closes lecture form.
         */
        $scope.$on('closeLectureForm', function (event, showWall) {
            $scope.showLectureForm = false;
            if (showWall) {
                $scope.useWall = true;
            }
            $scope.checkIfInLecture();
        });

        /*
         Event listener for closeQuestion. Closes quesition pop-up.
         */
        $scope.$on('closeQuestion', function () {
            $scope.showAnswerWindow = false;
        });


        /*
         Event listener for answerToQuesion. Closes the answer pop-up and sends answer to server.
         */
        $scope.$on("answerToQuestion", function (event, answer) {
            if (!$scope.isLecturer) $scope.showAnswerWindow = false;
            var mark = "";
            var answerString = "";
            angular.forEach(answer.answer, function (singleAnswer) {
                answerString += mark + singleAnswer;
                mark = "|";
            });

            http({
                url: '/answerToQuestion',
                method: 'PUT',
                params: {
                    'asked_id': answer.askedId,
                    'lecture_id': $scope.lectureId,
                    'answers': answerString,
                    'buster': new Date().getTime()
                }
            })
                .success(function (answer) {
                    if (angular.isDefined(answer.questionLate)) {
                        $window.alert(answer.questionLate);
                    }
                })
                .error(function () {
                    $window.console.log("Failed to answer to question");
                });
        });


        /*
         Event window for closeAnswerShow. Closes pop-up to show answers and stops gettin more of them.
         */
        $scope.$on("closeAnswerShow", function () {
            $scope.showStudentAnswers = false;
            //$scope.gettingAnswers = false;
        });

        /**
         * Depending on what users wants to see on the wall, makes the msg to correct form. Able to show the
         * name of the sender, time and message. Sender and time are optional.
         * @memberof module:lectureController
         */
        $scope.showInfo = function () {
            $scope.msg = "";
            var i = 0;
            if ($scope.messageName && $scope.messageTime) {
                for (i = 0; i < $scope.wallMessages.length; i++) {
                    $scope.msg += $scope.wallMessages[i].sender;
                    $scope.msg += " <" + $scope.wallMessages[i].time + ">: ";
                    $scope.msg += $scope.wallMessages[i].message + "\r\n";
                }
            }

            if (!$scope.messageName && $scope.messageTime) {
                for (i = 0; i < $scope.wallMessages.length; i++) {
                    $scope.msg += " <" + $scope.wallMessages[i].time + ">: ";
                    $scope.msg += $scope.wallMessages[i].message + "\r\n";
                }
            }

            if ($scope.messageName && !$scope.messageTime) {
                for (i = 0; i < $scope.wallMessages.length; i++) {
                    $scope.msg += $scope.wallMessages[i].sender + ": ";
                    $scope.msg += $scope.wallMessages[i].message + "\r\n";
                }
            }

            if (!$scope.messageName && !$scope.messageTime) {
                for (i = 0; i < $scope.wallMessages.length; i++) {
                    $scope.msg += ">" + $scope.wallMessages[i].message + "\r\n";
                }
            }
        };

        /**
         * Clears the password input when changing the lecture from current lectures list.
         * @memberof module:lectureController
         */
        $scope.clearChange = function () {
            $scope.passwordQuess = "";
        };

        /**
         * Puts chosen lecture options to use.
         * @memberof module:lectureController
         * @param useQuestions Whether or not to display questions?
         * @param useWall Whether or not to display the wall?
         */
        $scope.useOptions = function (useQuestions, useWall) {
            $scope.showLectureView($scope.lectureAnswer);
            $scope.useWall = useWall;
            $scope.useQuestions = useQuestions;
            $scope.showLectureOptions = false;

        };

        /**
         * Ask lecture options from students that are coming to lecture.
         * @memberof module:lectureController
         */
        $scope.lectureOptions = function () {
            if (!$scope.isLecturer) {
                $scope.showLectureOptions = true;
            }
            $scope.joinLecture();
        };

        /**
         * Hides the wall if the wall hasn't moved.
         * @memberof module:lectureController
         */
        $scope.hideWall = function () {
            var base = wall.find('#wallBase');
            $scope.newMessagesAmount = 0;
            $scope.newMessagesAmountText = '';
            $scope.showWall = !$scope.showWall;
            if (!$scope.showWall) {
                $scope.wallHeight = wall.height();
                wall.height(15);
                base.css('display', 'none');
                wall.css('min-height', '0');
            } else {
                base.css('display', '');
                wall.css('min-height', '');
                wall.height($scope.wallHeight);
            }
            $scope.wallName = 'Wall - ' + $scope.lectureName;
            $scope.$apply();
        };

        /**
         * Toggle the lecture creation form.
         * @memberof module:lectureController
         */
        $scope.toggleLecture = function () {
            $('#currentList').hide();
            $('#futureList').hide();
            $scope.showLectureForm = true;
            $rootScope.$broadcast("initLectureFormVals");
        };

        /**
         * Starts lecture that is in future lecture list.
         * @memberof module:lectureController
         */
        $scope.startFutureLecture = function () {
            http({
                url: '/startFutureLecture',
                method: 'POST',
                params: {'doc_id': $scope.docId, 'lecture_code': $scope.futureLecture.lecture_code}
            })
                .success(function (answer) {
                    $scope.showLectureView(answer);
                });
        };

        /**
         * Change the usage of wall.
         * @param wallUsage Whether wall should be displayed or not.
         * @memberof module:lectureController
         */
        $scope.changeUsingWall = function (wallUsage) {
            $scope.useWall = wallUsage;
        };

        /**
         * Change the usage of getting lecture questions.
         * @param questionUsage Whether questions should be displayed or not.
         * @memberof module:lectureController
         */
        $scope.changeUsingQuestions = function (questionUsage) {
            $scope.useQuestions = questionUsage;
        };

        /**
         * Changes the usage of getting answers from students.
         * @param answerUsage Whether to get answers from students or not.
         * @memberof module:lectureController
         */
        $scope.changeUsingAnswers = function (answerUsage) {
            $scope.useAnswers = answerUsage;
        };

        /**
         * Initializes the window to be lecture view a.k.a. the current user in in lecture.
         * @param lecture The lecture to be shown.
         * @memberof module:lectureController
         */
        $scope.showLectureView = function (lecture) {
            $scope.isLecturer = lecture.isLecturer;

            $scope.lectureName = lecture.lectureCode;
            $scope.wallName = "Wall - " + lecture.lectureCode;
            if ($scope.wallName.length > 30) {
                $scope.wallName = $scope.wallName.substring(0, 30) + "...";
            }
            $scope.lectureStartTime = "Started: " + lecture.startTime;
            $scope.lectureEndTime = "Ends: " + lecture.endTime;
            $scope.inLecture = true;
            $scope.lectureId = lecture.lectureId;
            $scope.polling = true;
            $scope.msg = "";
            $scope.showWall = true;
            $scope.useWall = lecture.useWall;
            $scope.useQuestions = lecture.useQuestions;

            $scope.getAllMessages();


            if ($scope.isLecturer) {
                $rootScope.$broadcast("getQuestions");
                $scope.canStop = true;
                $scope.addPeopleToList(lecture.students, $scope.studentTable);
                $scope.addPeopleToList(lecture.lecturers, $scope.lecturerTable);
            }
        };

        /**
         * Adds people entities to give list.
         * @param people name: String, active:String
         * @param peopleList List of people in the lecture.
         * @memberof module:lectureController
         */
        $scope.addPeopleToList = function (people, peopleList) {
            for (var i = 0; i < people.length; i++) {
                var oldUser = false;
                for (var index = 0; index < peopleList.length; index++) {
                    if (peopleList[index].id === people[i].user_id) {
                        oldUser = true;
                        peopleList[index].active = people[i].active;
                        break;
                    }
                }

                if (!oldUser) {
                    var student = {
                        id: people[i].user_id,
                        name: people[i].name,
                        active: people[i].active
                    };
                    peopleList.push(student);
                }
            }
        };

        /**
         * Initializes the window to be basic view a.k.a. view where user is not in lecture.
         * @param answer The lecture to be processed.
         * @memberof module:lectureController
         */
        $scope.showBasicView = function (answer) {

            $scope.isLecturer = answer.isLecturer;
            if ($scope.isLecturer) {
                $rootScope.$broadcast("getQuestions");
                $scope.canStart = true;
                $scope.canStop = false;
            }
            $scope.wallMessages = [];
            $scope.useWall = false;
            $scope.polling = false;
            $scope.inLecture = false;
            $scope.lectureId = -1;
            $scope.lectureName = "Not running";
            $scope.showStudentAnswers = false;
            $scope.showAnswerWindow = false;
            $scope.lecturerTable = [];
            $scope.studentTable = [];
            $scope.lectures = [];
            $scope.futureLectures = [];


            var addLecture = true;
            for (var i = 0; i < answer.lectures.length; i++) {
                $scope.lectures.push(answer.lectures[i]);
            }

            for (i = 0; i < answer.futureLectures.length; i++) {
                $scope.futureLectures.push(answer.futureLectures[i]);
            }

            if ($scope.lectures.length > 0) {
                $scope.chosenLecture = $scope.lectures[0];
            }

            if ($scope.futureLectures.length > 0) {
                $scope.futureLecture = $scope.futureLectures[0];
            }
        };

        /**
         * Extends the lecture based on the time selected in pop-up to extend lecture.
         * Currently extends to the old lecture ending time. Other option is to extend
         * from the current moment(needs to be implemented).
         * @memberof module:lectureController
         */
        $scope.extendLecture = function () {
            var dateTime = $scope.lectureEndTime.split(" ");

            //TODO: Use javascript date object.
            var YearMonthDay = dateTime[1].split("-");
            var endYear = parseInt(YearMonthDay[0]);
            var endMonth = parseInt(YearMonthDay[1]);
            var endDay = parseInt(YearMonthDay[2]);

            var hoursMins = dateTime[2].split(":");
            var endHour = parseInt(hoursMins[0]);
            var endMinutes = parseInt(hoursMins[1]);


            endMinutes += parseInt($scope.extend.extendTime);

            if (endMinutes >= 60) {
                endHour += 1;
                endMinutes -= 60;
                if (endHour >= 24) {
                    endDay += 1;
                    endHour -= 24;
                    switch (endMonth) {
                        case 1:
                        case 3:
                        case 5:
                        case 7:
                        case 8:
                        case 10:
                        case 12:
                            if (endDay > 31) {
                                endDay = 1;
                                endMonth += 1;
                            }
                            break;
                        case 2:
                            if (endDay > 28) {
                                endDay = 1;
                                endMonth += 1;
                            }
                            break;
                        default:
                            if (endDay > 30) {
                                endDay = 1;
                                endMonth += 1;
                            }
                    }
                    if (endMonth > 12) {
                        endMonth = 1;
                        endYear += 1;
                    }
                }
            }


            var endTimeDate = endYear + "-" + $scope.leftPadder(endMonth, 2) + "-" + $scope.leftPadder(endDay, 2) + " " +
                $scope.leftPadder(endHour, 2) + ":" + $scope.leftPadder(endMinutes, 2);
            $scope.lectureEndTime = "Ends: " + endTimeDate;
            $scope.showLectureEnding = false;
            $scope.lectureEnded = false;
            $scope.answeredToLectureEnding = true;

            http({
                url: '/extendLecture',
                method: 'POST',
                params: {'doc_id': $scope.docId, lecture_id: $scope.lectureId, new_end_time: endTimeDate}
            })
                .success(function () {
                    $scope.answeredToLectureEnding = false;
                    $window.console.log("Lecture extended");
                })
                .error(function () {
                    $window.console.log("Failed to extend the lecture");
                });
        };

        /**
         * Closes the lecture view and sets answered to lectureEnding to true to prevent multiple quesitions from this.
         * @memberof module:lectureController
         */
        $scope.continueLecture = function () {
            $scope.answeredToLectureEnding = true;
            $scope.showLectureEnding = false;
        };

        /**
         * NOT IMPLEMENTED YET
         * @memberof module:lectureController
         */
        $scope.editLecture = function (lecture_code) {
            $('#currentList').hide();
            $('#futureList').hide();
            http({
                url: '/showLectureInfoGivenName',
                method: 'GET',
                params: {'lecture_code': lecture_code, 'doc_id': $scope.docId}
            })
                .success(function (lecture) {
                    $rootScope.$broadcast("editLecture", {
                        "lecture_id": lecture.lectureId,
                        "lecture_name": lecture.lectureCode,
                        "start_date": lecture.lectureStartTime,
                        "end_date": lecture.lectureEndTime,
                        "password": lecture.password || "",
                        "editMode": true
                    });
                    $scope.showLectureForm = true;
                })
                .error(function () {
                    $window.console.log("Failed to fetch lecture.");
                });
        };

        /**
         * Sends http request to end the lecture.
         * @memberof module:lectureController
         */
        $scope.endLecture = function () {
            $scope.showLectureEnding = false;

            // TODO: Change to some better confirm dialog.
            var confirmAnswer = $window.confirm("Do you really want to end this lecture?");
            if (confirmAnswer) {
                http({
                    url: '/endLecture',
                    method: 'POST',
                    params: {'doc_id': $scope.docId, lecture_id: $scope.lectureId}
                })
                    .success(function (answer) {
                        $scope.showBasicView(answer);
                        $scope.chosenLecture = "";
                        $scope.msg = "";
                        $window.console.log("Lecture ended, not deleted");

                    })
                    .error(function () {
                        $window.console.log("Failed to delete the lecture");
                    });
            }
        };

        /**
         * Sends http request to delete the lecture.
         * @memberof module:lectureController
         */
        $scope.deleteLecture = function () {
            http({
                url: '/deleteLecture',
                method: 'POST',
                params: {doc_id: $scope.docId, lecture_id: $scope.lectureId, buster: new Date().valueOf()}
            })
                .success(function (answer) {
                    $scope.showBasicView(answer);
                    $scope.lectures.splice($scope.lectureId, 1);
                    $scope.chosenLecture = "";
                    $scope.msg = "";
                    $window.console.log("Lecture deleted");

                })
                .error(function () {
                    $window.console.log("Failed to delete the lecture");
                });
        };

        /**
         * Sends http request to leave the lecture.
         * @memberof module:lectureController
         */
        $scope.leaveLecture = function (lecture_id, confirm) {
            //TODO: better confirm dialog
            $scope.msg = "";
            http({
                url: '/leaveLecture',
                method: "POST",
                params: {
                    'lecture_id': lecture_id || $scope.lectureId,
                    'doc_id': $scope.docId,
                    'buster': new Date().valueOf()
                }
            })
                .success(function (answer) {
                    $scope.showBasicView(answer);
                });
        };

        /**
         * Shows lecture creation. //TODO: Something is missing from here
         */
        $scope.modifyLecture = function () {
            $scope.showLectureCreation = true;
        };

        /**
         * Sends http request to send a message.
         * @param message The message to be sent.
         * @returns {boolean} Whether the message was sent successfully.
         * @memberof module:lectureController
         */
        $scope.sendMessageEvent = function (message) {
            if (message.trim() === "") {
                $window.alert("Can't send empty messages");
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
                    $window.console.log("Can't send message or something");
                });

        };

        /**
         * Sends http request to get all the messages from the current lecture.
         * @memberof module:lectureController
         */
        $scope.getAllMessages = function () {
            $scope.msg = "";
            http({
                url: '/getAllMessages',
                type: 'GET',
                params: {lecture_id: $scope.lectureId, buster: new Date().getTime()}
            })
                .success(function (answer) {
                    angular.forEach(answer.data, function (msg) {
                        $scope.wallMessages.push(msg);
                        if ($scope.messageName) {
                            $scope.msg += msg.sender + " ";
                        }
                        if ($scope.messageTime) {
                            $scope.msg += "<" + msg.time + ">: ";
                        }

                        $scope.msg += msg.message + "\n";
                    });

                    //TODO: Fix this to scroll bottom without cheating.
                    var wallArea = $('#wallArea');
                    wallArea.animate({scrollTop: wallArea[0].scrollHeight * 10}, 1000);

                    $scope.lastID = answer.lastid;
                    $scope.requestDocId = $scope.lectureId;

                    if ($scope.pollingLectures.indexOf(answer.lectureId) === -1) {
                        $scope.startLongPolling($scope.lastID);
                        $scope.pollingLectures.push(answer.lectureId);
                    }
                });
        };

        /**
         * Gets answers from the current lecture to current question.
         * @param answer Lecture to get the answers from.
         * @memberof module:lectureController
         */
        $scope.getLectureAnswers = function (answer) {
            $scope.gettingAnswers = true;
            http({
                url: '/getLectureAnswers',
                type: 'GET',
                params: {
                    'asked_id': answer.askedId,
                    'doc_id': $scope.docId,
                    'lecture_id': $scope.lectureId,
                    'time': answer.latestAnswer,
                    'buster': new Date().getTime()
                }
            })
                .success(function (answer) {
                    $rootScope.$broadcast("putAnswers", {"answers": answer.answers});
                    if ($scope.gettingAnswers && !angular.isDefined(answer.noAnswer)) {
                        $scope.getLectureAnswers(answer);
                    }

                })
                .error(function () {
                    $window.console.log("Couldn't get answers");
                });
        };

        /**
         * Starts long polling for the updates.(messages, questions, lecture ending)
         * @param lastID Last id which was received.
         * @memberof module:lectureController
         */
        $scope.startLongPolling = function (lastID) {
            function message_longPolling(lastID) {
                var timeout;

                if (lastID === null) {
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
                        'is_lecturer': $scope.isLecturer, // Tarkista mielummin serverin päässä
                        'get_messages': $scope.useWall,
                        'get_questions': $scope.useQuestions,
                        'buster': new Date().getTime()
                    }
                })
                    .success(function (answer) {

                        if (!answer.isLecture) {
                            $scope.showBasicView(answer);
                            return;
                        }
                        $scope.pollingLectures.splice($scope.pollingLectures.indexOf(answer.lectureId), 1);
                        if (answer.lectureId !== $scope.lectureId) {
                            return;
                        }

                        if (answer.lectureEnding !== 100) {
                            if (answer.lectureEnding === 1 && !$scope.lectureEnded) {
                                $scope.showLectureEnding = true;
                                $scope.lectureEnded = true;
                            }
                            if (!$scope.answeredToLectureEnding) {
                                $scope.showLectureEnding = true;
                            }
                        }

                        $scope.addPeopleToList(answer.students, $scope.studentTable);
                        $scope.addPeopleToList(answer.lecturers, $scope.lecturerTable);

                        if (answer.question && !$scope.isLecturer) {
                            $scope.showAnswerWindow = true;
                            $rootScope.$broadcast("setQuestionJson", {
                                questionJson: JSON.parse(answer.questionJson),
                                askedId: answer.askedId,
                                isLecturer: $scope.isLecturer,
                                askedTime: answer.asked,
                                clockOffset: $scope.clockOffset
                            });
                        }

                        $scope.requestOnTheWay = false;
                        $window.clearTimeout(timeout);
                        if ($scope.polling) {

                            $scope.pollingLectures.push(answer.lectureId);
                            // Odottaa sekunnin ennen kuin pollaa uudestaan.
                            timeout = setTimeout(function () {
                                message_longPolling(answer.lastid);
                            }, 1000);

                            if (answer.status === 'results') {
                                var newMessages = 0;
                                angular.forEach(answer.data, function (msg) {
                                    newMessages++;
                                    $scope.wallMessages.push(msg);
                                    if ($scope.messageName && $scope.messageTime) {
                                        $scope.msg += msg.sender;
                                        $scope.msg += " <" + msg.time + ">: ";
                                        $scope.msg += msg.message + "\r\n";
                                    }

                                    if (!$scope.messageName && $scope.messageTime) {
                                        $scope.msg += " <" + msg.time + ">: ";
                                        $scope.msg += msg.message + "\r\n";

                                    }

                                    if ($scope.messageName && !$scope.messageTime) {
                                        $scope.msg += msg.sender + ": ";
                                        $scope.msg += msg.message + "\r\n";

                                    }

                                    if (!$scope.messageName && !$scope.messageTime) {
                                        $scope.msg += ">" + msg.message + "\r\n";

                                    }
                                });
                                $scope.newMessagesAmount += newMessages;
                                if (!$scope.showWall)
                                    $scope.newMessagesAmountText = ' (' + $scope.newMessagesAmount.toString() + ')';
                                else
                                    $scope.newMessagesAmountText = '';
                                $scope.wallName = 'Wall ' + $scope.lectureName + $scope.newMessagesAmountText;
                                $scope.lastID = answer.lastid;
                                var wallArea = $('#wallArea');
                                wallArea.scrollTop(wallArea[0].scrollHeight);
                            } else {
                                $window.console.log("Sending new poll.");

                            }
                        } else {
                            $window.console.log("Got answer but not polling anymore.");
                        }
                    })
                    .error(function () {
                        $scope.requestOnTheWay = false;
                        $window.clearTimeout(timeout);
                        //Odottaa 30s ennen kuin yrittää uudelleen errorin jälkeen.
                        timeout = setTimeout(function () {
                            message_longPolling($scope.lastID);
                        }, 30000);
                    });
            }

            message_longPolling(lastID);
        };

        /**
         * Event for pressing enter while writing message. Sends message.
         * @param event They key press.
         * @memberof module:lectureController
         */
        $scope.chatEnterPressed = function (event) {
            if (event.which === 13) {
                $scope.sendMessageEvent($scope.newMsg);
            }
        };

        /**
         * Event when pressing enter while writing password for lecture. Tries to join lecture
         * @param event The key press.
         * @memberof module:lectureController
         */
        $scope.passEnterPressed = function (event) {
            if (event.which === 13) {
                $scope.joinLecture();
            }
        };

        /**
         * Left padder that adds 0 to left side of number.
         * @param number Given number to add 0s
         * @param size how many characters the padded value should be.
         * @returns {string} The string of the length specified padded with zeroes.
         * @memberof module:lectureController
         */
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