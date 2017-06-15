import angular from "angular";
import $ from "jquery";
import moment from "moment";
import {timApp} from "tim/app";
import {$http, $log, $window} from "../ngimport";

/**
 * Lecture creation controller which is used to handle and validate the form data.
 * @module createLectureCtrl
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

timApp.controller("CreateLectureCtrl", ["$scope",
    function($scope) {
        "use strict";
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
        $scope.lectureEncoded = "";
        $scope.password = "";
        $scope.maxStudents = "";
        $scope.earlyJoining = true;
        $scope.editMode = false;

        $scope.dateTimeOptions = {
            format: "D.M.YYYY HH:mm:ss",
            showTodayButton: true,
        };

        /**
         * Function called when new lecture form button is pressed.
         * @memberof module:createLectureCtrl
         */
        $scope.$on("initLectureFormVals", function() {
            $scope.initLectureForm();
        });

        $scope.$on("editLecture", function(event, data) {
            $scope.lectureCode = data.lecture_name;
            $scope.lectureCodeEncoded = encodeURIComponent($scope.lectureCode);

            $scope.hostName = encodeURIComponent(location.host);
            $scope.lectureId = data.lecture_id;
            $scope.startTime = data.start_date;
            $scope.endTime = data.end_date;
            $scope.enableDate2();
            if (data.password !== undefined) {
                $scope.password = data.password;
            }
            $scope.editMode = data.editMode;
            $scope.addKeyListeners();
        });

        $scope.addKeyListeners = function() {
            if ( $scope.dataform ) {
                return; // already keys binded
            }
            $scope.dataform = $("#lectureForm")[0];

            $scope.dataform.addEventListener("keydown", function(event) {
                if (event.ctrlKey || event.metaKey) {
                    switch (String.fromCharCode(event.which).toLowerCase()) {
                    case "s":
                        event.preventDefault();
                        $scope.submitLecture();
                        break;
                    }
                }
            });
        };

        /**
         * Lecture form initialized to have the current date and time as default starting time.
         * @memberof module:createLectureCtrl
         */
        $scope.initLectureForm = function() {
            $log.info("Lecture initialized.");
            $scope.startTime = moment();
            $scope.dueCheck = true;
            $scope.earlyJoining = true;
            $scope.enableDue2();
            $scope.addKeyListeners();

        };

        $scope.populateLectureForm = function(name) {
            $scope.lectureCode = name;
        };

        /**
         * This function is called if end date is selected. Sets boolean values to reflect this choice and sets
         * the default end time to 2 hours ahead of start time.
         * @memberof module:createLectureCtrl
         */
        $scope.enableDate2 = function() {
            $scope.dateCheck = true;
            $scope.dueCheck = false;
            if ($scope.endTime == null) {
                $scope.endTime = moment($scope.startTime).add(2, "hours");
            }
            $scope.useDate = true;
            $scope.useDuration = false;
            $scope.durationHour = "";
            $scope.durationMin = "";
        };

        /**
         * Function for enabling fields and buttons for "Duration" and disabling them for "Use date". This function
         * is called when duration is chosen and is chosen by default.
         * @memberof module:createLectureCtrl
         */
        $scope.enableDue2 = function() {
            $scope.dateCheck = false;
            $scope.dueCheck = true;
            $scope.useDuration = true;
            $scope.useDate = false;
            $scope.durationHour = "02";
            $scope.durationMin = "00";
        };

        /**
         * Remove border from given element.
         * @param element ID of the field whose border will be removed.
         * @memberof module:createLectureCtrl
         */
        $scope.defInputStyle = function(element) {
            if (element !== null || !element.isDefined) {
                angular.element("#" + element).css("border", "");
            }
        };

        /**
         * Checks if the value is between 0-23
         * @param element The value of the element to be validated.
         * @param val The ID of the input field so user can be notified of the error.
         * @memberof module:createLectureCtrl
         */
        $scope.isHour = function(element, val) {
            if (element === "" || isNaN(element) || element > 23 || element < 0) {
                $scope.errorize(val, "Hour has to be between 0 and 23.");
            }
        };

        $scope.isNumber = function(element, val) {
            console.log(element);
            if (!(element === "") && (isNaN(element) || element < 0)) {
                $scope.errorize(val, "Max number of students must be a positive number or empty.");
            }
        };

        /**
         * Checks if the value is between 0-59
         * @param element The value of the element to be validated.
         * @param val The ID of the input field so user can be notified of the error.
         * @memberof module:createLectureCtrl
         */
        $scope.isMinute = function(element, val) {
            if (element === "" || isNaN(element) || element > 59 || element < 0) {
                $scope.errorize(val, "Minutes has to be between 0 and 59.");
            }
        };

        /**
         * Checks if the number given is positive.
         * @param element The value of the element to be validated.
         * @param val The ID of the input field so user can be notified of the error.
         * @memberof module:createLectureCtrl
         */
        $scope.isPositiveNumber = function(element, val) {
            if (element === "" || isNaN(element) || element < 0) {
                $scope.errorize(val, "Number has to be positive.");
            }
        };

        /**
         * Function for creating a new lecture and validation.
         * @memberof module:createLectureCtrl
         */
        $scope.submitLecture = function() {
            if ($scope.lectureId === undefined || $scope.lectureId === null) {
                $scope.editMode = false;
            }
            $scope.removeErrors();
            if (!$scope.startTime) {
                $scope.errorize("startTime", "Start time must be entered!");
            }

            /*This checks that "lecture code"-field is not empty.*/
            if ($scope.lectureCode === "") {
                $scope.errorize("lCode", "Lecture name must be entered!");
            }

            /*This checks that either "Use date" or "Duration" is chosen for ending time.*/
            if ($scope.dateCheck === false && $scope.dueCheck === false) {
                $scope.errorize("endInfo", "A date or duration must be chosen.");
            }
            $scope.isNumber($scope.maxStudents, "maxStudents");

            if ($scope.startTime !== null) {
                const lectureStartingInPast = moment().diff($scope.startTime) >= 0;

                /* Checks that are run if end date is used*/
                if ($scope.useDate && $scope.endTime !== null) {
                    if ($scope.endTime.diff($scope.startTime) < 120000) {
                        $scope.errorize("endDateDiv", "Lecture has to last at least two minutes.");
                    }
                }

                /* Check that are run if duration is used. */
                if ($scope.useDuration) {
                    if ($scope.durationHour === "") {
                        $scope.durationHour = "00";
                    }
                    if ($scope.durationMin === "") {
                        $scope.durationMin = "00";
                    }
                    $scope.isPositiveNumber($scope.durationHour, "durationHour");
                    $scope.isPositiveNumber($scope.durationMin, "durationMin");
                    if ($scope.durationHour.length <= 0 && $scope.durationMin.length <= 0) {
                        $scope.errorize("durationDiv", "Please give a duration.");
                    }
                    $scope.endTime = moment($scope.startTime)
                        .add(parseInt($scope.durationHour), "hours")
                        .add(parseInt($scope.durationMin));
                    if ($scope.endTime.diff($scope.startTime) < 120000) {
                        $scope.errorize("durationDiv", "Lecture has to last at least two minutes.");
                    }
                }
                let alertMessage = "";
                const lectureEndingInPast = moment().diff($scope.endTime) >= 0;
                /* Confirmations if lecture starts and/or ends before the current date */
                if (lectureStartingInPast && !$scope.editMode) {
                    alertMessage += "Are you sure you want the lecture to start before now? ";
                }
                if (lectureEndingInPast && !$scope.editMode) {
                    alertMessage += "Are you sure that the lecture ends in the past or now and will not run?";
                }
                $window.console.log($scope.error_message.length);
                if (alertMessage !== "" && $scope.error_message.length <= 0) {
                    if (!$window.confirm(alertMessage)) {
                        if (lectureStartingInPast) {
                            $scope.errorize("startInfo", "Please select another date and time.");
                        }
                        if (lectureEndingInPast) {
                            $scope.errorize("endInfo", "Please select another date or duration.");
                        }
                    }
                }
            }
            /* If no errors save the lecture to the database */
            if ($scope.error_message <= 0) {
                if ($scope.earlyJoining) {
                    $scope.startTime.subtract(15, "minutes");
                }
                $http<{ lectureId: number }>({
                    url: "/createLecture",
                    method: "POST",
                    params: {
                        lecture_id: $scope.lectureId,
                        doc_id: $scope.docId,
                        lecture_code: $scope.lectureCode,
                        password: $scope.password,
                        start_date: $scope.startTime,
                        end_date: $scope.endTime,
                        max_students: $scope.maxStudents || "",
                    },
                })
                    .then(function(response) {
                        if ($scope.editMode) {
                            $window.console.log("Lecture " + response.data.lectureId + " updated.");
                        } else {
                            $window.console.log("Lecture created: " + response.data.lectureId);
                        }
                        $scope.$parent.lectureSettings.useWall = true;
                        $scope.$parent.useAnswers = true;
                        $scope.clearForm();
                        $scope.$emit("closeLectureForm", true);
                        $scope.$emit("lectureUpdated", $scope.lectureCode);
                    }, function(response) {
                        $scope.error_message += response.error;
                        $window.console.log(response);
                    });
            }
        };

        /**
         * Changes the border of the element to red.
         * @param inputId The ID of the input field so user can be notified of the error.
         * @param errorText Error text that will be printed if the error occurs.
         * @memberof module:createLectureCtrl
         */
        $scope.errorize = function(inputId, errorText) {
            angular.element("#" + inputId).css("border", "1px solid red");
            if (errorText.length > 0) {
                $scope.error_message += errorText + "<br />";
            }
        };

        /**
         * Calls defInputStyle for all the form elements.
         * @memberof module:createLectureCtrl
         */
        $scope.removeErrors = function() {
            $scope.error_message = "";

            const elementsToRemoveErrorsFrom = [
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
                "durationDiv",
                "durationHour",
                "durationMin",
                "maxStudents",
            ];
            for (let i = 0; i < elementsToRemoveErrorsFrom.length; i++) {
                if (elementsToRemoveErrorsFrom[i] !== undefined) {
                    $scope.defInputStyle(elementsToRemoveErrorsFrom[i]);
                }
            }
        };

        /**
         *  Resets all the values of the form.
         *  @memberof module:createLectureCtrl
         */
        $scope.clearForm = function() {
            $scope.lectureCode = "";
            $scope.password = "";
            $scope.startDate = moment();
            $scope.endDate = null;
            $scope.startHour = "";
            $scope.startMin = "";
            $scope.durationMin = "";
            $scope.durationHour = "";
            $scope.endHour = "";
            $scope.endMin = "";
            $scope.showLectureCreation = false;
            $scope.removeErrors();
            $scope.useDate = false;
            $scope.useDuration = true;
            $scope.dateChosen = false;
            $scope.durationChosen = false;
            $scope.dateCheck = false;
            $scope.lectureId = null;
        };

        /**
         * Function for cancelling the lecture creation.
         * @memberof module:createLectureCtrl
         */
        $scope.cancelCreation = function() {
            $scope.editMode = false;
            $scope.$emit("closeLectureForm");
            $scope.clearForm();
        };

        $scope.$watch("lectureCode", function() {
            $scope.lectureCodeEncoded = encodeURIComponent($scope.lectureCode);
        });

        $scope.$watch("hostName", function() {
            $scope.hostName = encodeURIComponent(location.host);
        });
    },
]);
