timApp.controller("CreateLectureCtrl", ['$scope', "$http",

    function ($scope, $http) {

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
        var date = new Date();
        $scope.startDate = date.getDate()+"."+(date.getMonth()+1)+"."+date.getFullYear();

        // Sets the calendars in the form to the current date and date-format dd.m.yyyy
        $(function() {
            $('#startDate').datepicker({ dateFormat: 'dd.m.yy' });
            $('#endDate').datepicker({ dateFormat: 'dd.m.yy' });
        });



        $scope.setCurrentTimeAndDate = function () {
            $scope.startDay = date.getDate();
            $scope.startMonth = date.getMonth()+1;
            $scope.startYear = date.getFullYear();
            $scope.startHour = date.getHours();
            $scope.startMin = (date.getMinutes() < 10 ? '0' : '') + date.getMinutes();
        };

        $scope.setCurrentTimeAndDate();

        $scope.enableDate2 = function () {
            console.log($scope.startDate);
            $scope.dateCheck = true;
            $scope.dueCheck = false;
            $scope.endDate = $scope.startDate;
            $scope.endHour = parseInt($scope.startHour) + 2;
            $scope.endMin = $scope.startMin;

            $scope.useDate = true;
            $scope.useDuration = false;
            $scope.durationHour = "";
            $scope.durationMin = "";
            $scope.defInputStyle(document.getElementById("hours2"));
            $scope.defInputStyle(document.getElementById("mins2"));
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

            $scope.defInputStyle(document.getElementById("stopDay"));
            $scope.defInputStyle(document.getElementById("stopMonth"));
            $scope.defInputStyle(document.getElementById("stopYear"));
            $scope.defInputStyle(document.getElementById("stopHour"));
            $scope.defInputStyle(document.getElementById("stopMin"));
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

        /*Function for checking that input is a number.*/
        $scope.isValid = function (element) {
            if (isNaN(element.value) === true) {
                element.style.border = "1px solid red";
                element.title = "Use a number.";
            }
            else {
                $scope.notEmpty(element);
            }
        };

        /*Removes error style from element (red border around field).*/
        $scope.defInputStyle = function (element) {
            if(element !== null){
                element.style.border = "";
                element.style.title = "";
            }
        };

        /*Function for checking that hours are correct (between 0 and 23).*/
        $scope.isHour = function (element, val) {
            if (element > 23 || element < 0) {
                document.getElementById(val).style.border = "1px solid red";
                $scope.error_message += "Hour has to be between 0 and 23.<br/>";
            }
        };

        /*Function for checking that minutes are correct (between 0 and 59).*/
        $scope.isMinute = function (element,val) {
            if (element > 59 || element < 0) {
                document.getElementById(val).style.border = "1px solid red";
                $scope.error_message += "Minutes has to be between 0 and 59. <br/>";
            }
        };

        /*Function for checking that number is positive.*/
        $scope.isPositiveNumber = function (element) {
            if (isNaN(element.value) || element.value < 0) {
                element.style.border = "1px solid red";
                element.title = "Number has to be positive.";
                $scope.showErrorMessage();
            } else {
                element.style.border = "";
                element.title = "Number has to be positive.";
            }
        };
        $scope.translateToDateObject = function(date_to_be_validated, time_hours, time_mins){
            var parms = date_to_be_validated.split(".");
            var dd = parseInt(parms[0]);
            var mm = parseInt(parms[1]);
            var yyyy = parseInt(parms[2]);
            var hours = parseInt(time_hours);
            var mins = parseInt(time_mins);
            return new Date(yyyy,mm-1,dd,hours,mins,0);
        }
        /*Function for creating a new lecture and error checking.*/
        $scope.submitLecture = function () {
            var start_date = $scope.translateToDateObject($scope.startDate, $scope.startHour, $scope.startMin);
            var end_date = $scope.translateToDateObject($scope.endDate, $scope.endHour, $scope.endMin);
            console.log(start_date);
            console.log(end_date);
            var elements = [
                $scope.startDay,
                $scope.startMonth,
                $scope.startYear,
                $scope.startHour,
                $scope.startMin,
                $scope.stopDay,
                $scope.stopMonth,
                $scope.stopYear,
                $scope.stopHour,
                $scope.stopMin,
                $scope.durationHour,
                $scope.durationMin];

            /*This checks that "lecture code"-field is not empty.*/
            if ($scope.lectureCode == "") {
                document.getElementById("lCode").style.border = "1px solid red";
                $scope.error_message += "Lecture code must be entered!<br/>";
            } else  {$scope.defInputStyle(document.getElementById("lCode"))};

            /*This checks that either "Use date" or "Duration" is chosen for ending time.*/
            if ($scope.dateCheck == false && $scope.dueCheck == false) {
                document.getElementById("endInfo").style.border = "1px solid red";
                $scope.error_message += "A date or duration must be chosen.<br />";
            } else  {$scope.defInputStyle(document.getElementById("lbend"))};
            /*Checks that hours in starting and ending time are between 0 and 23.
             Checks that minutes in starting and ending time are between 0 and 59*/
            $scope.isHour(elements[3],"startHour");
            $scope.isMinute(elements[4],"startMin");

            if ($scope.useDate) {
                $scope.isHour(elements[8], "stopHour");
                $scope.isMinute(elements[9], "stopMin");
            }


            if ($scope.useDuration) {

                $scope.isPositiveNumber(elements[10]);
                $scope.isPositiveNumber(elements[11]);
                if ($scope.durationHour.value.length <= 0 && $scope.durationMin.value.length <= 0) {
                    document.getElementById("durationHour").style.border = "1px solid red";
                    document.getElementById("durationMin").style.border = "1px solid red";
                    $scope.error_message += "Please give positive number.<br>";
                }
            }

            if ($scope.useDuration) {

                $scope.endDay = $scope.startDay;
                $scope.endMonth = $scope.startMonth;
                $scope.endYear = $scope.startYear;
                $scope.durationMin = this.formScope.form.endMm.$viewValue;
                $scope.durationHour = this.formScope.form.endHh.$viewValue;

                if ($scope.durationMin == "") {
                    $scope.durationMin = 0;
                }

                if ($scope.durationHour == "") {
                    $scope.durationHour = 0;
                }

                var hoursFromMins = parseInt($scope.durationMin) / 60 >> 0;
                $scope.endMin = parseInt(this.formScope.form.startMm.$viewValue) + parseInt($scope.durationMin) % 60;
                $scope.endHour = parseInt(this.formScope.form.startHh.$viewValue) + parseInt($scope.durationHour) + hoursFromMins;
                if ($scope.endHour >= 24) {
                    var extraDays = parseInt($scope.endHour) / 24 >> 0;
                    $scope.endHour = $scope.endHour % 24;
                    $scope.endDay = $scope.endDay + extraDays;
                    switch ($scope.endMonth) {
                        case 1:
                        case 3:
                        case 5:
                        case 7:
                        case 8:
                        case 10:
                        case 12:
                            if ($scope.endDay > 31) {
                                $scope.endMonth += 1;
                                $scope.endDay = 1;
                            }
                            break;
                        case 2:
                            if ($scope.endDay > 28) {
                                $scope.endMonth += 1;
                                $scope.endDay = 1;
                            }
                            break;
                        default :
                            if ($scope.endDay > 30) {
                                $scope.endMonth += 1;
                                $scope.endDay = 1;
                            }
                    }

                    if ($scope.endMonth > 12) {
                        $scope.endMonth = 1;
                        $scope.endYear += 1;
                    }
                }

            }

            if ($scope.useDate) {
                $scope.endDateForDB = "" + $scope.leftPadder(end_date.getFullYear(), 4) + "-"
                    + $scope.leftPadder(end_date.getMonth()+1, 2) + "-"
                    + $scope.leftPadder(end_date.getDate(), 2) + " "
                    + $scope.leftPadder(end_date.getHours(), 2) + ":"
                    + $scope.leftPadder(end_date.getMinutes(), 2);
            }

            if ($scope.useDuration) {
                var endDate = "" + $scope.leftPadder($scope.endYear, 4) + "-"
                    + $scope.leftPadder($scope.endMonth, 2) + "-"
                    + $scope.leftPadder($scope.endDay, 2) + " "
                    + $scope.leftPadder($scope.endHour, 2) + ":"
                    + $scope.leftPadder($scope.endMin, 2);

                console.log(endDate);
            }
            console.log($scope.error_message.length);
            if($scope.error_message<=0) {
                $http({
                    url: '/createLecture',
                    method: 'POST',
                    params: {
                        'doc_id': $scope.docId,
                        'lecture_code': $scope.lectureCode,
                        'password': $scope.password,
                        'start_date': start_date.getFullYear() + "-" + $scope.leftPadder(start_date.getMonth()+1,2) + "-" + start_date.getDate() + " " + start_date.getHours() + ":" + start_date.getMinutes(),
                        'end_date': $scope.endDateForDB
                    }
                })
                    .success(function (answer) {
                        console.log("Lecture created: " + answer.lectureId);
                        $scope.$emit("closeLectureForm")
                    })
                    .error(function () {
                        console.log("Failed to start a lecture");
                    })
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

        /*Function for cancelling the lecture creation.*/
        $scope.cancelCreation = function () {


            $scope.$emit("closeLectureForm");
            var elementsToClear = [$scope.startDay,
                $scope.startMonth,
                $scope.startYear,
                $scope.startHour,
                $scope.startMin,
                $scope.stopDay,
                $scope.stopMonth,
                $scope.stopYear,
                $scope.stopHour,
                $scope.stopMin,
                $scope.durationHour,
                $scope.durationMin];
            var i;
            for (i = 7; i < elementsToClear.length; i++) {
                if (elementsToClear[i] != undefined) {
                    elementsToClear[i] = "";
                }
            }
            $scope.showLectureCreation = false;
            $scope.error_message = "";
            document.getElementById("lectureForm").reset();
            $scope.useDate = false;
            $scope.useDuration = false;
            $scope.dateChosen = false;
            $scope.durationChosen = false;

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