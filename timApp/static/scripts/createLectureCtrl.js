timApp.controller("CreateLectureCtrl", ['$scope', "$http",

    function ($scope, http) {

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


        var date = new Date();

        $scope.setCurrentTime = function () {
           $scope.startDay = date.getDate();
            $scope.startMonth = date.getMonth() + 1;
            $scope.startYear = date.getFullYear();
            $scope.startHour = date.getHours();
            $scope.startMin = (date.getMinutes()<10?'0':'') + date.getMinutes();
        };
		
		$scope.setCurrentTime();
        var errors = 0;

        $scope.enableDate2 = function () {
			
			$scope.dateCheck = true;
			$scope.dueCheck = false;
            $scope.endDate = lectureForm.form.startDate.value.split("-");
            $scope.endDay = $scope.endDate[0];
            $scope.endMonth = $scope.endDate[1];
            $scope.endYear = $scope.endDate[2];
            $scope.endHour = $scope.startHour + 2;
            $scope.endMin = $scope.startMin;

            $scope.useDate = true;
            $scope.useDuration = false;
            lectureForm.form.durationHour.value = "";
            lectureForm.form.durationMin.value = "";
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

        /*Function for showing the error message.*/
        $scope.showErrorMessage = function () {
			//alert("Errors");
			//document.getElementById("errorMessage").innerHTML = "Errors in the form. Please, correct the fields marked with red to continue.";
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

            var elements = [
                lectureForm.form.startDay,
                lectureForm.form.startMonth,
                lectureForm.form.startYear,
                lectureForm.form.startHour,
                lectureForm.form.startMin,
                lectureForm.form.stopDay,
                lectureForm.form.stopMonth,
                lectureForm.form.stopYear,
                lectureForm.form.stopHour,
                lectureForm.form.stopMin,
                lectureForm.form.durationHour,
                lectureForm.form.durationMin];

            /*This checks that "lecture code"-field is not empty.*/
            if (lectureForm.form.code.value == undefined || lectureForm.form.code.value == "") {
                document.getElementById("lCode").style.border = "1px solid red";
                $scope.error_message += "Lecture code must be entered!<br/>";
            } else  $scope.defInputStyle(document.getElementById("lCode"));

            /*This checks that either "Use date" or "Duration" is chosen for ending time.*/
            if ($scope.dateCheck == false && $scope.dueCheck == false) {
                document.getElementById("endInfo").style.border = "1px solid red";
                $scope.error_message += "A date or duration must be chosen.<br />";
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
                if (lectureForm.form.durationHour.value.length <= 0 && lectureForm.form.durationMin.value.length <= 0) {
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
				var endDate = "" + $scope.leftPadder(this.formScope.form.stopY.$viewValue, 4) + "-"
					+ $scope.leftPadder(this.formScope.form.stopM.$viewValue, 2) + "-"
					+ $scope.leftPadder(this.formScope.form.stopD.$viewValue, 2) + " "
					+ $scope.leftPadder(this.formScope.form.stopHh.$viewValue, 2) + ":"
					+ $scope.leftPadder(this.formScope.form.stopMm.$viewValue, 2);
			}
			
			if ($scope.useDuration) {
				var endDate = "" + $scope.leftPadder($scope.endYear, 4) + "-"
					+ $scope.leftPadder($scope.endMonth, 2) + "-"
					+ $scope.leftPadder($scope.endDay, 2) + " "
					+ $scope.leftPadder($scope.endHour, 2) + ":"
					+ $scope.leftPadder($scope.endMin, 2);
					
				console.log(endDate);
			}


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
            var elementsToClear = [lectureForm.form.startDay,
                lectureForm.form.startMonth,
                lectureForm.form.startYear,
                lectureForm.form.startHour,
                lectureForm.form.startMin,
                lectureForm.form.stopDay,
                lectureForm.form.stopMonth,
                lectureForm.form.stopYear,
                lectureForm.form.stopHour,
                lectureForm.form.stopMin,
                lectureForm.form.durationHour,
                lectureForm.form.durationMin];
            var i;
            for (i = 7; i < elementsToClear.length; i++) {
                if(elementsToClear[i] != undefined)
                    elementsToClear[i].value = "";
            }
            $scope.showLectureCreation = false;
            $scope.error_message = "";
            document.getElementById("lectureForm").reset();
            $scope.useDate = false;
            $scope.useDuration = false;
            $scope.dateChosen = false;
            $scope.durationChosen = false;
            console.log($rootScope);
			myService.setLectureForm(false);
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