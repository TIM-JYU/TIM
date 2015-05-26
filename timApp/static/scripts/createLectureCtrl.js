timApp.controller("CreateLectureCtrl", ['$scope', "$http", "$window",

    function ($scope, $http, $window) {

        $scope.showLectureCreation = false;
        $scope.useDate = false;
        $scope.useDuration = false;
        $scope.dateChosen = false;
        $scope.durationChosen = false;
        $scope.durationHour = "";
        $scope.durationMin = "";
        $scope.lectureId = null;
        $scope.dateCheck = false;
        $scope.dueCheck = false;
        $scope.error_message = "";
        $scope.lectureCode = "";
        $scope.password = "";
        $scope.startDate = "";
        $scope.startHour = "";
        $scope.startMin = "";
        var date = new Date();
        $scope.startDate = date.getDate()+"."+(date.getMonth()+1)+"."+date.getFullYear();

        $scope.$on('initLectureFormVals', function () {
            $scope.initLectureForm();
        });

        // Sets the calendars in the form to the current date and date-format dd.m.yyyy
        $(function() {
            $('#startDate').datepicker({ dateFormat: 'dd.m.yy' });
            $('#endDate').datepicker({ dateFormat: 'dd.m.yy' });
        });

        $scope.initLectureForm = function() {
            $window.console.log("Lecture initialized.");
            var date = new Date();
            $scope.startDate = date.getDate()+"."+(date.getMonth()+1)+"."+date.getFullYear();
            $scope.startHour = $scope.leftPadder(date.getHours(),2);
            $scope.startMin =  $scope.leftPadder(date.getMinutes(),2);
            $scope.dueCheck = true;
            $scope.enableDue2();
        };

        $scope.setCurrentTimeAndDate = function () {
            $scope.startDay = $scope.leftPadder(date.getDate(),2);
            $scope.startMonth = $scope.leftPadder(date.getMonth()+1,2);
            $scope.startYear = $scope.leftPadder(date.getFullYear(),4);
            $scope.startHour = $scope.leftPadder(date.getHours(),2);
            $scope.startMin =  $scope.leftPadder(date.getMinutes(),2);
        };

        $scope.setCurrentTimeAndDate();

        $scope.enableDate2 = function () {
            $window.console.log($scope.startDate);
            $scope.dateCheck = true;
            $scope.dueCheck = false;
            $scope.endDate = $scope.startDate;
            $scope.endHour = $scope.leftPadder(parseInt($scope.startHour) + 2,2);
            $scope.endMin = $scope.leftPadder($scope.startMin,2);

            $scope.useDate = true;
            $scope.useDuration = false;
            $scope.durationHour = "";
            $scope.durationMin = "";
        };

        /*Function for enabling fields and buttons for "Duration" and disabling them for "Use date".*/
        $scope.enableDue2 = function () {
            $scope.dateCheck = false;
            $scope.dueCheck = true;
            $scope.useDuration = true;
            $scope.useDate = false;
            $scope.endDay = "";
            $scope.endMonth = "";
            $scope.endYear = "";
            $scope.endHour = "";
            $scope.endMin = "";
            $scope.durationHour = "02";
            $scope.durationMin = "00";
        };

        /*Function for checking that elements value isn't empty.*/
        $scope.notEmpty = function (element,val) {
            if (element.value === "") {
                $scope.errorize(val, "This field can't be empty.");
            } else {
                $scope.defInputStyle(val);
            }
        };

        /*Function for checking that input is a number.*/
        $scope.isValid = function (element,val) {
            if (isNaN(element.value) === true) {
                $scope.errorize(val, "Use a number.");
            }
            else {
                $scope.notEmpty(element);
            }
        };

        /*Removes error style from element (red border around field).*/
        $scope.defInputStyle = function (element) {
            if(element !== null || !element.isDefined){
                document.getElementById(element).style.border = "";
            }
        };

        /*Function for checking that hours are correct (between 0 and 23).*/
        $scope.isHour = function (element, val) {
            if (element > 23 || element < 0) {
                $scope.errorize(val, "Hour has to be between 0 and 23.");
            }
        };

        /*Function for checking that minutes are correct (between 0 and 59).*/
        $scope.isMinute = function (element,val) {
            if (element > 59 || element < 0) {
                $scope.errorize(val, "Minutes has to be between 0 and 59.");
            }
        };

        /*Function for checking that number is positive.*/
        $scope.isPositiveNumber = function (element,val) {
            if (isNaN(element) || element < 0) {
                $scope.errorize(val, "Number has to be positive.");
            }
        };

        $scope.isDateValid = function (element,val) {
            var reg = new RegExp("^(0?[1-9]|[12][0-9]|3[01])[.]((0?[1-9]|1[012])[.](19|20)?[0-9]{2})*$");
            if(!reg.test(element)){
                $scope.errorize(val,"Date is not of format dd.mm.yyyy.");
              }
        };

        $scope.dateObjectToString = function(input, include_time) {
            var output = $scope.leftPadder(input.getFullYear(), 4) + "-" +
                $scope.leftPadder(input.getMonth()+1,2) + "-" +
                $scope.leftPadder(input.getDate(),2);
            if(include_time) {
                output += " " + $scope.leftPadder(input.getHours(),2) + ":" +
                $scope.leftPadder(input.getMinutes(),2);
            }
            return output;
        };
        /*Creates a new date object with the specified date and time*/
        $scope.translateToDateObject = function(date_to_be_validated, time_hours, time_mins, splitter){
            var parms = date_to_be_validated.split(splitter);
            if(splitter === ".") {
                var dd = parseInt(parms[0]);
                var mm = parseInt(parms[1]);
                var yyyy = parseInt(parms[2]);
            } else {
                var dd = parseInt(parms[2]);
                var mm = parseInt(parms[1]);
                var yyyy = parseInt(parms[0]);
            }
            var hours = parseInt(time_hours);
            var mins = parseInt(time_mins);
            return new Date(yyyy,mm-1,dd,hours,mins,0);
        };

        /*Function for creating a new lecture and error checking.*/
        $scope.submitLecture = function () {
            $scope.removeErrors();
            $scope.isDateValid($scope.startDate, "startDate");
            if($scope.error_message<=0) {
                $scope.start_date = $scope.translateToDateObject($scope.startDate, $scope.startHour, $scope.startMin,".");
                if ($scope.endDate !== undefined, $scope.useDate) {
                    $scope.isDateValid($scope.endDate, "endDate");
                    $scope.end_date = $scope.translateToDateObject($scope.endDate, $scope.endHour, $scope.endMin,".");
                } else {
                    $scope.end_date = "";
                }
            }

            /*This checks that "lecture code"-field is not empty.*/
            if ($scope.lectureCode === "") {
                $scope.errorize("lCode", "Lecture code must be entered!");
            }

            /*This checks that either "Use date" or "Duration" is chosen for ending time.*/
            if ($scope.dateCheck === false && $scope.dueCheck === false) {
                $scope.errorize("endInfo","A date or duration must be chosen.");
            }
            /*Checks that hours in starting and ending time are between 0 and 23.
             Checks that minutes in starting and ending time are between 0 and 59*/
            $scope.isHour($scope.startHour,"startHour");
            $scope.isMinute($scope.startMin,"startMin");

            if($scope.start_date !== undefined) {
                var now_hours = date.getHours();
                var now_minutes = date.getMinutes();
                var now_date_object = $scope.translateToDateObject($scope.dateObjectToString(date, false), now_hours, now_minutes, "-");
                var lecture_starting_in_past = $scope.start_date - now_date_object < 0;
                var lecture_ending_in_past = false;

                if ($scope.useDate && $scope.end_date !== undefined) {
                    $scope.isHour($scope.stopHour, "stopHour");
                    $scope.isMinute($scope.stopMin, "stopMin");
                    if ($scope.end_date - $scope.start_date <= 0) {
                        $scope.errorize("endDateDiv", "Lecture has to last at least a minute.");
                    }
                    $scope.endDateForDB = $scope.dateObjectToString($scope.end_date, true);
                }

                if ($scope.useDuration) {
                    $scope.isPositiveNumber($scope.durationHour, "durationHour");
                    $scope.isPositiveNumber($scope.durationMin, "durationMin");
                    if ($scope.durationHour.length <= 0 && $scope.durationMin.length <= 0) {
                        $scope.errorize("durationDiv", "Please give a duration.");
                    }
                    $scope.end_date = new Date($scope.start_date.getTime());
                    $scope.end_date.setHours($scope.start_date.getHours() + parseInt($scope.durationHour));
                    $scope.end_date.setMinutes($scope.start_date.getMinutes() + parseInt($scope.durationMin));
                    if ($scope.end_date - $scope.start_date <= 0) {
                        $scope.errorize("durationDiv", "Lecture has to last at least a minute.");
                    }
                    $scope.endDateForDB = $scope.dateObjectToString($scope.end_date, true);
                }
                var alert_message = "";
                lecture_ending_in_past = $scope.end_date - now_date_object <= 0;
                if (lecture_starting_in_past) {
                    alert_message += "Are you sure you want the lecture to start before now? ";
                }
                if (lecture_ending_in_past) {
                    alert_message += "Are you sure that the lecture ends in the past or now and will not run?";
                }
                console.log("$scope.error_message.length")
                if (alert_message !== "" && $scope.error_message.length<=0) {
                    if (!$window.confirm(alert_message)) {
                        if (lecture_starting_in_past) {
                            $scope.errorize("startInfo", "Please select another date and time.");
                        }
                        if (lecture_ending_in_past) {
                            $scope.errorize("endInfo", "Please select another date or duration.");
                        }
                    }
                }
            }
            if($scope.error_message<=0) {
                $scope.startDateForDB = $scope.dateObjectToString($scope.start_date,true);
                $http({
                    url: '/createLecture',
                    method: 'POST',
                    params: {
                        'doc_id': $scope.docId,
                        'lecture_code': $scope.lectureCode,
                        'password': $scope.password,
                        'start_date': $scope.startDateForDB,
                        'end_date': $scope.endDateForDB
                    }
                })
                    .success(function (answer) {
                        console.log("Lecture created: " + answer.lectureId);
                        $scope.clearForm();
                    })
                    .error(function (answer) {
                        $scope.error_message += answer.error;
                        console.log(answer);
                    });
            }
        };

        /* Changes the border of the element to red*/
        $scope.errorize = function(div_val,error_text) {
            document.getElementById(div_val).style.border = "1px solid red";
            if(error_text.length > 0) {$scope.error_message += error_text + "<br />";}
        };

        $scope.removeErrors = function () {
            $scope.error_message = "";
            var elementsToRemoveErrorsFrom = [
                "lCode",
                "startDate",
                "startHour",
                "startMin",
                "startInfo",
                "endInfo",
                "endDate",
                "endHour",
                "endMin",
                "endDateDiv",
                "durationDiv"
            ]
            for(var i = 0; i < elementsToRemoveErrorsFrom.length; i++){
                if (elementsToRemoveErrorsFrom[i] !== undefined) {
                    $scope.defInputStyle(elementsToRemoveErrorsFrom[i]);
                }
            }
        };

        /* Clears the form of all values */
        $scope.clearForm = function () {
            $scope.$emit("closeLectureForm");
            $scope.lectureCode = "";
            $scope.password = "";
            $scope.startDate = "";
            $scope.endDate = "";
            $scope.startHour = "";
            $scope.startMin = "";
            $scope.durationMin = "";
            $scope.durationHour = "";
            $scope.endHour = "";
            $scope.endMin = "";
            $scope.showLectureCreation = false;
            $scope.removeErrors();
            document.getElementById("lectureForm").reset();
            $scope.useDate = false;
            $scope.useDuration = true;
            $scope.dateChosen = false;
            $scope.durationChosen = false;
            $scope.dateCheck = false;

        };

        /* Adds size number of 0's at the start of the number*/
        $scope.leftPadder = function (number, size) {
            var paddedNumber = "" + number;
            var len = paddedNumber.length;
            while (len < size) {
                paddedNumber = "0" + paddedNumber;
                len++;
            }
            return paddedNumber;
        };

        /*Function for cancelling the lecture creation.*/
        $scope.cancelCreation = function () {
            $scope.clearForm();
        };
    }
]);