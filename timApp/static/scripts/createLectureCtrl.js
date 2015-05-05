timApp.controller("CreateLectureCtrl", ['$scope', 'docIdParam', 'anotherScope', '$controller', "$http", "$window",

    function ($scope, docIdParam, anotherScope, controller, http, $window) {

        $scope.showLectureCreation = false;
        $scope.useDate = false;
        $scope.useDuration = false;
        $scope.dateChosen = false;
        $scope.durationChosen = false;
        $scope.durationHour = "";
        $scope.durationMin = "";
        $scope.lectureId = null;

        var date = new Date();

        $scope.setCurrentTime = function () {
           this.formScope.form.startD.$viewValue = date.getDate();
            $scope.startMonth = date.getMonth() + 1;
            $scope.startYear = date.getFullYear();
            $scope.startHour = date.getHours();
            $scope.startMin = date.getMinutes();
			console.log(this.formScope.form.startD.$viewValue); 
        };
		
		$scope.setCurrentTime();
		/*this.formScope.form.startHh.$viewValue = $scope.leftPadder($scope.startHour, 2);
		this.formScope.form.startMm.$viewValue = $scope.leftPadder($scope.startMin, 2);
		document.getElementById("startMonth").value = $scope.startMonth;
		document.getElementById("startYear").value = $scope.startYear;
		document.getElementById("startDay").value = $scope.startDay;*/

        var errors = 0;

        $scope.enableDate2 = function () {
			
            $scope.endDay = this.formScope.form.startD.$viewValue;
            $scope.endMonth = this.formScope.form.startM.$viewValue;
            $scope.endYear = this.formScope.form.startY.$viewValue;
            $scope.endHour = parseInt(this.formScope.form.startHh.$viewValue) + 2;
            $scope.endMin = this.formScope.form.startMm.$viewValue;

            /*document.getElementById("stopDay").value = $scope.endDay;
            document.getElementById("stopMonth").value = $scope.endMonth;
            document.getElementById("stopYear").value = $scope.endYear;
            document.getElementById("stopHour").value = $scope.endHour;
            document.getElementById("stopMin").value = $scope.endMin;*/


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
            $scope.endDay = "";
            $scope.endMonth = "";
            $scope.endYear = "";
            $scope.endHour = "";
            $scope.endMin = "";

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
		$scope.setFormScope= function(scope){
			this.formScope = scope;
		}
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

            if ($scope.useDate) {
                $scope.isHour(elements[8]);
                $scope.isMinute(elements[9]);
            }


            if ($scope.useDuration) {

                $scope.isPositiveNumber(elements[10]);
                $scope.isPositiveNumber(elements[11]);

                if ($scope.durationHour.length <= 0 && $scope.durationMin.length <= 0) {
                    elements[10].style.border = "1px solid red";
                    elements[11].style.border = "1px solid red";
                    elements[10].title = "Please give positive number.";
                    elements[11].title = "Please give positive number.";
                    $scope.showErrorMessage()
                }
            }

            /*g_globalObject.closeCalendar();
            g_globalObject2.closeCalendar();/*/

            // TODO: Make better way to check errors.
            if (document.getElementById("errorMessage").innerHTML.length > 0) {
                return;
            }

            var startDate = "" + $scope.leftPadder(this.formScope.form.startY.$viewValue, 4) + "-"
                + $scope.leftPadder(this.formScope.form.startM.$viewValue, 2) + "-"
                + $scope.leftPadder(this.formScope.form.startD.$viewValue, 2) + " "
                + $scope.leftPadder(this.formScope.form.startHh.$viewValue, 2) + ":"
                + $scope.leftPadder(this.formScope.form.startMm.$viewValue, 2);

            if ($scope.useDuration) {

                $scope.endDay = $scope.startDay;
                $scope.endMonth = $scope.startMonth;
                $scope.endYear = $scope.startYear;

                if ($scope.durationMin == "") {
                    $scope.durationMin = 0;
                }

                if ($scope.durationHour == "") {
                    $scope.durationHour = 0;
                }

                var hoursFromMins = parseInt($scope.durationMin) / 60 >> 0;
                $scope.endMin = parseInt($scope.startMin) + parseInt($scope.durationMin) % 60;
                $scope.endHour = parseInt($scope.startHour) + parseInt($scope.durationHour) + hoursFromMins;
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

            var endDate = "" + $scope.leftPadder(this.formScope.form.stopY.$viewValue, 4) + "-"
                + $scope.leftPadder(this.formScope.form.stopM.$viewValue, 2) + "-"
                + $scope.leftPadder(this.formScope.form.stopD.$viewValue, 2) + " "
                + $scope.leftPadder(this.formScope.form.stopHh.$viewValue, 2) + ":"
                + $scope.leftPadder(this.formScope.form.stopMm.$viewValue, 2);



            http({
                url: '/createLecture',
                method: 'POST',
                params: {
                    'doc_id': docIdParam, 'lecture_code': this.formScope.form.code.$viewValue, 'password': this.formScope.form.password.$viewValue,
                    'start_date': startDate, 'end_date': endDate
                }
            })
                .success(function (answer) {
                    anotherScope.checkIfInLecture();
                    console.log("Lecture created: " + answer.lectureId);
					$scope.$modalClose();
                })
                .error(function () {
                    console.log("Failed to start a lecture");
                })
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
            for (i = 7; i < elementsToClear.length; i++) {
                elementsToClear[i].value = "";
            }
            $scope.showLectureCreation = false;
            document.getElementById("errorMessage").innerHTML = "";
            document.getElementById("lectureForm").reset();
            $scope.useDate = false;
            $scope.useDuration = false;
            $scope.dateChosen = false;
            $scope.durationChosen = false;
			$scope.$modalClose();
            /*g_globalObject.closeCalendar();
            g_globalObject2.closeCalendar();*/
        };

        /*$scope.prepareCalender = function () {
            g_globalObject = new JsDatePick({
                useMode: 2,
                target: "calendarStart",
                dateFormat: "%d-%M-%Y",
                yearsRange: [2010, 2020]
            });

            g_globalObject.setOnSelectedDelegate(function () {
                var obj = g_globalObject.getSelectedDay();
                $scope.startDay = obj.day;
                $scope.startMonth = obj.month;
                $scope.startYear = obj.year;
                // TODO: This if necessary or if there is other way
                $scope.$apply();
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
                 weekStartDay:1*//*
            });

            g_globalObject2.setOnSelectedDelegate(function () {
                var obj = g_globalObject2.getSelectedDay();
                $scope.endDay = obj.day;
                $scope.endMonth = obj.month;
                $scope.endYear = obj.year;
                // TODO: This if necessary or if there is other way
                $scope.$apply();
                g_globalObject2.closeCalendar();
            });
        };
        $scope.prepareCalender();
	*/
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