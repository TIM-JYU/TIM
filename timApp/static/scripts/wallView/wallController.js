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
        $scope.passwordQuess = "";
        $scope.pollingLectures = [];
        $scope.useDate = false;
        $scope.useDuration = false;

        var date = new Date();


        $scope.setCurrentTime = function () {
            $scope.startDay = date.getDate();
            $scope.startMonth = date.getMonth() + 1;
            $scope.startYear = date.getFullYear();
            $scope.startHour = date.getHours();
            $scope.startMin = date.getMinutes();
        };

        $scope.setCurrentTime();

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
                console.log("Choose lecture to join");
                return;
            }

            http({
                url: '/joinLecture',
                method: 'POST',
                params: {'lecture_code': $scope.chosenLecture, 'password_quess': $scope.passwordQuess}
            })
                .success(function (answer) {
                    $scope.passwordQuess = "";
                    if (!answer.correctPassword) {
                        document.getElementById("passwordInput").style.border = "1px solid red";
                        document.getElementById("passwordInput").placeholder = "Wrong password";
                    } else {
                        document.getElementById("passwordInput").style.border = "1px solid black";
                        document.getElementById("passwordInput").placeholder = "Password";
                        if (answer.inLecture) {
                            console.log(answer)
                            $scope.showLectureView(answer);
                        } else {
                            $scope.showBasicView(answer);
                        }
                    }
                })
        }

        $scope.toggleLecture = function () {
            $scope.showLecture = !$scope.showLecture
            if ($scope.showLecture) {
                $scope.setCurrentTime();
                document.getElementById("startMonth").value = $scope.startMonth;
                document.getElementById("startYear").value = $scope.startYear;
                document.getElementById("startHour").value = $scope.startHour;
                document.getElementById("startMin").value = $scope.startMin;
                document.getElementById("startDay").value = $scope.startDay;
            } else {
                $scope.cancelCreation();
            }
        }

        $scope.showLectureView = function (answer) {
            $scope.inLecture = true;
            $scope.lectureId = answer.lectureId;
            $scope.polling = true;
            $scope.showWall = true;

            $scope.getAllMessages();

            if (answer.isLecturer) {
                $scope.canStop = true;
            }
            document.getElementById("lectureName").innerText = answer.lectureCode;
        }

        $scope.showBasicView = function (answer) {
            $scope.canStart = true;
            $scope.canStop = false;
            $scope.polling = false;
            $scope.inLecture = false;
            $scope.lectureId = -1;
            document.getElementById("lectureName").innerText = "Not running";
            if (answer.lectures != undefined) {
                jQuery.each(answer.lectures, function (i, lecture) {
                    if ($scope.lectures.indexOf(lecture) == -1) {
                        $scope.lectures.push(lecture);
                    }
                });
            }
        }

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
                    $scope.requestDocId = $scope.lectureId;

                    if ($scope.pollingLectures.indexOf(answer.lectureId) == -1) {
                        $scope.startLongPolling($scope.lastID);
                        $scope.pollingLectures.push(answer.lectureId);
                    }

                })
        };

        $scope.startLongPolling = function (lastID) {
            function message_longPolling(lastID) {
                var timeOut;

                if (lastID == null) {
                    lastID = -1;
                }

                $scope.requestOnTheWay = true;
                jQuery.ajax({
                        url: '/getMessages',
                        type: 'GET',
                        data: {'client_message_id': lastID, lecture_id: $scope.lectureId},
                        success: function (answer) {
                            $scope.pollingLectures.splice($scope.pollingLectures.indexOf(answer.lectureId), 1);
                            if (answer.lectureId != $scope.lectureId) {
                                return;
                            }

                            $scope.requestOnTheWay = false;
                            clearInterval(timeOut);
                            if ($scope.polling) {

                                $scope.pollingLectures.push(answer.lectureId);
                                if (answer.status == 'results' || answer.status == 'no-results') {

                                    timeOut = setTimeout(function () {
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
                            clearInterval(timeOut);
                            timeOut = setTimeout(function () {
                                message_longPolling();
                            }, 30000);
                        }
                    }
                )
                ;
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

        var errors = 0;

        $scope.enableDate2 = function () {

            $scope.endDay = $scope.startDay;
            $scope.endMonth = $scope.startMonth;
            $scope.endYear = $scope.startYear;
            $scope.endHour = parseInt($scope.startHour) + 2;
            $scope.endMin = $scope.startMin;

            document.getElementById("calendarStop").disabled = false;
            $scope.useDate = true;
            $scope.useDuration = false;
            document.getElementById("hours2").value = "";
            document.getElementById("mins2").value = "";
            $scope.defInputStyle(document.getElementById("hours2"));
            $scope.defInputStyle(document.getElementById("mins2"));

            $scope.defInputStyle(document.getElementById("lbend"));
        };

        /*Function for enabling fields and buttons for "Duration" and disabling them for "Use date".*/
        $scope.enableDue2 = function () {
            $scope.useDuration = true;
            $scope.useDate = false;
            document.getElementById("calendarStop").disabled = true;
            $scope.defInputStyle(document.getElementById("stopDay"));
            $scope.defInputStyle(document.getElementById("stopMonth"));
            $scope.defInputStyle(document.getElementById("stopYear"));
            $scope.defInputStyle(document.getElementById("stopHour"));
            $scope.defInputStyle(document.getElementById("stopMin"));

            $scope.defInputStyle(document.getElementById("lbend"));
        };

        /*Function for checking that elements value isn't empty.*/
        $scope.notEmpty = function (element) {
            if (element.value == "") {
                element.style.border = "1px solid red";
                element.title = "This field can't be empty.";
                errors++;
            } else {

                $scope.defInputStyle(element);
            }

        };

        /*Function for showing the error message.*/
        $scope.showErrorMessage = function () {
            document.getElementById("errorMessage").innerHTML = "Errors in the form. Please, correct the fields marked with red to continue.";
        };

        /*Function for checking that input is a number.*/
        $scope.isValid = function (element) {
            if (isNaN(element.value) == true) {
                element.style.border = "1px solid red";
                element.title = "Use a number.";
                errors++;
            }
            else {
                $scope.notEmpty(element);
            }
        };

        /*Removes error style from element (red border around field).*/
        $scope.defInputStyle = function (element) {
            element.style.border = "";
            element.style.title = "";
        };

        /*Function for checking that hours are correct (between 0 and 23).*/
        $scope.isHour = function (element) {
            if (element.value > 23 || element.value < 0) {
                element.style.border = "1px solid red";
                element.title = "Hour has to be between 0 and 23.";
                $scope.showErrorMessage();
            }
        };

        /*Function for checking that minutes are correct (between 0 and 59).*/
        $scope.isMinute = function (element) {
            if (element.value > 59 || element.value < 0) {
                element.style.border = "1px solid red";
                element.title = "Minutes has to be between 0 and 59.";
                $scope.showErrorMessage();
            }
        };

        /*Function for checking that number is positive.*/
        $scope.isPosiviteNumber = function (element) {
            if (element.value < 0) {
                element.style.border = "1px solid red";
                element.title = "Number has to be positive.";
                $scope.showErrorMessage();
            }
        };

        /*Function for creating a new lecture and error checking.*/
        $scope.submitLecture = function () {
            var elements = [document.getElementById("startDay"),
                document.getElementById("startMonth"),
                document.getElementById("startYear"),
                document.getElementById("startHour"),
                document.getElementById("startMin"),
                document.getElementById("stopDay"),
                document.getElementById("stopMonth"),
                document.getElementById("stopYear"),
                document.getElementById("stopHour"),
                document.getElementById("stopMin"),
                document.getElementById("hours2"),
                document.getElementById("mins2")];

            var i;
            /*Checks if there are errors in input.*/
            for (i = 0; i < elements.length; i++) {
                if (elements[i].style.border == "1px solid red") {
                    $scope.showErrorMessage();
                }
                else document.getElementById("errorMessage").innerHTML = "";
            }


            /*This checks that "lecture code"-field is not empty.*/
            if (document.getElementById("lCode").value == "") {
                document.getElementById("lCode").style.border = "1px solid red";
                document.getElementById("lCode").title = "You must type in something.";
                $scope.showErrorMessage();
            } else  $scope.defInputStyle(document.getElementById("lCode"));

            /*This checks that either "Use date" or "Duration" is chosen for ending time.*/
            if (document.getElementById("dateChosen2").checked == false && document.getElementById("durationChosen").checked == false) {
                document.getElementById("lbend").style.border = "1px solid red";
                document.getElementById("lbend").title = "You must select something.";
                $scope.showErrorMessage();
            } else  $scope.defInputStyle(document.getElementById("lbend"));
            /*Checks that hours in starting and ending time are between 0 and 23.
             Checks that minutes in starting and ending time are between 0 and 59*/
            $scope.isHour(elements[3]);
            $scope.isMinute(elements[4]);

            if (document.getElementById("dateChosen2").checked == true) {
                $scope.isHour(elements[8]);
                $scope.isMinute(elements[9]);
            }

            g_globalObject.closeCalendar();
            g_globalObject2.closeCalendar();

            if (document.getElementById("errorMessage").innerHTML.length > 0) {
                return;
            }

            $scope.cancelCreation();


            var startDate = "" + $scope.numberFormatter($scope.startYear, 4) + "."
                + $scope.numberFormatter($scope.startMonth, 2) + "."
                + $scope.numberFormatter($scope.startDay, 2) + "|"
                + $scope.numberFormatter($scope.startHour, 2) + ":"
                + $scope.numberFormatter($scope.startMin, 2);

            var endDate = "" + $scope.numberFormatter($scope.endYear, 4) + "."
                + $scope.numberFormatter($scope.endMonth, 2) + "."
                + $scope.numberFormatter($scope.endDay, 2) + "|"
                + $scope.numberFormatter($scope.endHour, 2) + ":"
                + $scope.numberFormatter($scope.endMin, 2);

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

        $scope.numberFormatter = function (number, size) {
            var formattedNumber = "" + number;
            var len = formattedNumber.length;
            while (len < size) {
                formattedNumber = "0" + formattedNumber
                len++;
            }
            return parseInt(formattedNumber);

        }

        /*Function for cancelling the lecture creation.*/
        $scope.cancelCreation = function () {
            var elementsToClear = [document.getElementById("lCode"),
                document.getElementById("lbend"),
                document.getElementById("startDay"),
                document.getElementById("startMonth"),
                document.getElementById("startYear"),
                document.getElementById("startHour"),
                document.getElementById("startMin"),
                document.getElementById("stopDay"),
                document.getElementById("stopMonth"),
                document.getElementById("stopYear"),
                document.getElementById("stopHour"),
                document.getElementById("stopMin"),
                document.getElementById("hours2"),
                document.getElementById("mins2")];
            var i;
            for (i = 0; i < elementsToClear.length; i++) {
                $scope.defInputStyle(elementsToClear[i]);
            }
            $scope.showLecture = false;
            document.getElementById("errorMessage").innerHTML = "";
            document.getElementById("lectureForm").reset();
            $scope.useDate = false;
            $scope.useDuration = false;
            g_globalObject.closeCalendar();
            g_globalObject2.closeCalendar();
        };

        $scope.prepareCalender = function () {
            g_globalObject = new JsDatePick({
                useMode: 2,
                target: "calendarStart",
                dateFormat: "%d-%M-%Y",
                yearsRange: [2010, 2020]
            });

            g_globalObject.setOnSelectedDelegate(function () {
                var obj = g_globalObject.getSelectedDay();
                document.getElementById("startDay").value = obj.day;
                document.getElementById("startMonth").value = obj.month;
                document.getElementById("startYear").value = obj.year;
                g_globalObject.closeCalendar();
            });

            g_globalObject2 = new JsDatePick({
                useMode: 2,
                target: "calendarStop",
                dateFormat: "%d-%M-%Y"
                /*selectedDate:{				This is an example of what the full configuration offers.
                 day:5,						For full documentation about these settings please see the full version of the code.
                 month:9,
                 year:2006
                 },
                 yearsRange:[1978,2020],
                 limitToToday:false,
                 cellColorScheme:"beige",
                 dateFormat:"%m-%d-%Y",
                 imgPath:"img/",
                 weekStartDay:1*/
            });

            g_globalObject2.setOnSelectedDelegate(function () {
                var obj = g_globalObject2.getSelectedDay();
                document.getElementById("stopDay").value = obj.day;
                document.getElementById("stopMonth").value = obj.month;
                document.getElementById("stopYear").value = obj.year;
                g_globalObject2.closeCalendar();
            });
        };
        $scope.prepareCalender();


    }]);

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