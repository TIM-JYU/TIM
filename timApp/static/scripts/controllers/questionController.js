/**
 * Controller for creating and editing questions
 * @module questionController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

timApp.controller("QuestionController", ['$scope', '$http', '$window', '$rootScope', function (scope, http, $window, $rootScope) {
    "use strict";
    $(function () {
        $('#calendarStart').datepicker({dateFormat: 'dd.m.yy'});
    });
    scope.dynamicAnswerSheetControl = {};
    scope.asked_id = false;

    scope.putBackQuotations = function (x) {
        return x.replace(/&quot;/g, '"');
    };

    scope.settings = $window.settings;

    scope.setTime = function () {
        scope.question.timeLimit = {hours: 0, minutes: 0, seconds: 30};
        if (scope.settings && scope.settings['timelimit'] && scope.settings['timelimit'] > 0) {
            var time = scope.settings['timelimit'];
            if (time > 3600) {
                scope.question.timeLimit.hours = Math.floor(time / 3600);
            } else {
                scope.question.timeLimit.hours = 0;
            }
            if (time > 60) {
                scope.question.timeLimit.minutes = Math.floor(time / 60);
                time = time % 60;
            } else {
                scope.question.timeLimit.minutes = 0;
            }
            if (time > 0) {
                scope.question.timeLimit.seconds = time;
            } else {
                scope.question.timeLimit.seconds = 0;
            }
        }
    };

    scope.$on("editQuestion", function (event, data) {
            var id = data.question_id;
            var asked_id = data.asked_id;
            var json = data.json;

            scope.asked_id = false;
            if (id) {
                scope.question.question_id = id;
            } else if (asked_id) {
                scope.asked_id = data.asked_id;
            }

            if (json["TITLE"]) scope.question.title = scope.putBackQuotations(json["TITLE"]);
            if (json["QUESTION"]) scope.question.question = scope.putBackQuotations(json["QUESTION"]);
            if (json["TYPE"]) scope.question.type = json["TYPE"];
            if (json["MATRIXTYPE"]) scope.question.matrixType = json["MATRIXTYPE"];
            if (json["ANSWERFIELDTYPE"]) scope.question.answerFieldType = (json["ANSWERFIELDTYPE"]);

            var jsonData = json["DATA"];
            var jsonHeaders = jsonData["HEADERS"];
            var jsonRows = jsonData["ROWS"];

            var columnHeaders = [];
            for (var i = 0; i < jsonHeaders.length; i++) {
                columnHeaders[i] = {
                    id: i,
                    type: jsonHeaders[i].type,
                    text: scope.putBackQuotations(jsonHeaders[i].text)
                };
            }
            scope.columnHeaders = columnHeaders;
            scope.pointsTable = [];
            if (data.points && data.points !== '') {
                var points = data.points.split('|');
                for (var i = 0; i < points.length; i++) {
                    var rowPoints = points[i].split(';');
                    var rowPointsDict = {};
                    for (var k = 0; k < rowPoints.length; k++) {
                        if (rowPoints[k] !== '') {
                            var colPoints = rowPoints[k].split(':', 2);
                            rowPointsDict[colPoints[0]] = colPoints[1];
                        }
                    }
                    scope.pointsTable.push(rowPointsDict);
                }
            }
            var rows = [];
            for (var i = 0; i < jsonRows.length; i++) {
                rows[i] = {
                    id: jsonRows[i].id,
                    text: scope.putBackQuotations(jsonRows[i].text),
                    type: jsonRows[i].type,
                    value: jsonRows[i].value
                };

                var jsonColumns = jsonRows[i]["COLUMNS"];
                var columns = [];
                for (var j = 0; j < jsonColumns.length; j++) {
                    var columnPoints = '';

                    if (scope.question.type === 'matrix' || scope.question.type === 'true-false') {
                        if (scope.question.matrixType !== 'textArea') {
                            if (scope.pointsTable.length > i) {
                                if ((j + 1).toString() in scope.pointsTable[i]) {
                                    columnPoints = scope.pointsTable[i][(j + 1).toString()];
                                }
                            }
                        }
                    } else {
                        if (scope.pointsTable.length > 0) {
                            if ((i + 1).toString() in scope.pointsTable[0]) {
                                columnPoints = scope.pointsTable[0][(i + 1).toString()];
                            }
                        }
                    }
                    columns[j] = {
                        id: j,
                        rowId: i,
                        text: jsonColumns[j].text,
                        points: columnPoints,
                        type: jsonColumns[j].type,
                        answerFieldType: jsonColumns[j].answerFieldType
                    };
                }
                rows[i].columns = columns;
            }
            scope.rows = rows;

            if (json["TIMELIMIT"] && json["TIMELIMIT"] > 0) {
                var time = json["TIMELIMIT"];
                scope.question.endTimeSelected = true;
                if (time > 3600) {
                    scope.question.timeLimit.hours = Math.floor(time / 3600);
                    time = time % 3600;
                } else {
                    scope.question.timeLimit.hours = 0;
                }

                if (time > 60) {
                    scope.question.timeLimit.minutes = Math.floor(time / 60);
                    time = time % 60;
                } else {
                    scope.question.timeLimit.minutes = 0;
                }

                if (time > 0) {
                    scope.question.timeLimit.seconds = time;
                } else {
                    scope.question.timeLimit.seconds = 0;
                }

            } else {
                scope.question.endTimeSelected = false;
            }

            scope.$emit('toggleQuestion');
        }
    );

    scope.question = {
        title: "",
        question: "",
        matrixType: "",
        answerFieldType: "",
        timeLimit: {hours: 0, minutes: 0, seconds: 30},
        endTimeSelected: true,
        showPreview: false
    };


    scope.rows = [];
    scope.columns = [];
    scope.columnHeaders = [];
    scope.setTime();
    scope.error_message = "";
    scope.answerFieldTypes = [
        {label: "Text area", value: "textArea"},
        {label: "Radio Button horizontal", value: "radiobutton-horizontal"},
        {label: "Checkbox", value: "checkbox"}
    ];

    /**
     * A function for creating a matrix.
     * @memberof module:questionController
     * @param rowsCount The number of rows to create for the matrix.
     * @param columnsCount The number of columns to create for new matrix.
     * @param type The answer type of the matrix.
     */
    scope.createMatrix = function (type) {
        var rowsCount = 0;
        var columnsCount = 0;
        if (type === 'matrix' || type === 'true-false') {
            rowsCount = 2;
            columnsCount = 2;
        } else {
            rowsCount = 2;
            columnsCount = 1;
        }

        if (scope.rows.length < 1) {
            for (var i = 0; i < rowsCount; i++) {
                scope.addRow(i);
            }
        }


        if (type === 'radio-vertical' || type === 'true-false') {
            scope.question.answerFieldType = 'radio';
        } else if (type === 'checkbox-vertical') {
            scope.question.answerFieldType = 'checkbox';
        } else if (type === 'matrix') {
            scope.question.answerFieldType = 'matrix';
        }

        for (var i = 0; i < scope.rows.length; i++) {
            if (scope.rows[i].columns.length > columnsCount) scope.rows[i].columns.splice(columnsCount, scope.rows[i].columns.length);
            if (scope.rows[i].columns.length < columnsCount) scope.addCol(scope.rows[0].columns.length);
        }

        scope.columnHeaders = [];
        if (type === 'matrix') {
            for (var i = 0; i < scope.rows[0].columns.length; i++) {
                scope.columnHeaders[i] = {
                    id: i,
                    text: "",
                    type: 'header'
                };
            }
        }
    };

    /**
     * A function to add a column to an existing matrix.
     * @memberof module:questionController
     * @param loc The index in the matrix where to add the new column.
     */
    scope.addCol = function (loc) {
        var location = loc;
        if (loc === -1) {
            location = scope.rows[0].columns.length;
            loc = scope.rows[0].columns.length;
        }
        scope.columnHeaders.splice(loc, 0, {type: "header", id: loc, text: ""});
        //add new column to columns
        for (var i = 0; i < scope.rows.length; i++) {
            scope.rows[i].columns.splice(loc, 0, {
                id: location,
                rowId: i,
                text: '',
                points: '',
                type: "answer",
                answerFiledType: scope.question.answerFieldType
            });
        }
        if (scope.question.showPreview) scope.createJson();
    };

    /**
     * The function adds a row to an existing matrix
     * @memberof module:questionController
     * @param loc The index in the matrix where to add the new row.
     */
    scope.addRow = function (loc) {
        scope.CreateColumnsForRow = function (location) {
            var columns = [];
            if (scope.rows.length > 0) {
                for (var j = 0; j < scope.rows[0].columns.length; j++) {
                    columns[j] = {
                        id: j,
                        rowId: location,
                        type: "answer",
                        value: '',
                        answerFiledType: scope.question.answerFieldType,
                        points: ""
                    };

                }
            }
            return columns;
        };

        var location = loc;
        if (loc === -1) {
            location = scope.rows.length;
            loc = scope.rows.length;
        }

        var columns = scope.CreateColumnsForRow(location);
        scope.rows.splice(loc, 0,
            {
                id: location,
                text: "",
                type: "question",
                value: "",
                columns: columns
            });

        for (var i = 0; i < scope.rows.length; i++) {
            scope.rows[i].id = i;
        }

        if (scope.question.showPreview) scope.createJson();
    };

    /**
     * A function to delete a row from a matrix.
     * @memberof module:questionController
     * @param indexToBeDeleted The index of the row to be deleted.
     */
    scope.delRow = function (indexToBeDeleted) {
        scope.error_message = "";
        if (scope.rows.length > 1) {
            if (indexToBeDeleted === -1) {
                scope.rows.splice(-1, 1);
            }
            else {
                scope.rows.splice(indexToBeDeleted, 1);
            }
        } else {
            scope.errorize("", "You cannot have an empty table.");
        }

        if (scope.question.showPreview) scope.createJson();
    };

    /**
     * A function to delete a column from a matrix.
     * @memberof module:questionController
     * @param indexToBeDeleted Index of the column to be deleted.
     */
    scope.delCol = function (indexToBeDeleted) {
        for (var i = 0; i < scope.rows.length; i++) {
            if (indexToBeDeleted === -1) {
                scope.rows[i].columns.splice(-1, 1);
            }
            else {
                scope.rows[i].columns.splice(indexToBeDeleted, 1);
            }
        }
        if (indexToBeDeleted === -1) {
            scope.columnHeaders.splice(-1, 1);
        }
        else {
            scope.columnHeaders.splice(indexToBeDeleted, 1);
        }

        if (scope.question.showPreview) scope.createJson();
    };

    /**
     * A function to reset the question values.
     * @memberof module:questionController
     */
    scope.clearQuestion = function () {
        scope.question = {
            title: "",
            question: "",
            matrixType: "",
            answerFieldType: "",
            endTimeSelected: true,
            showPreview: false
        };
        scope.setTime();

        scope.rows = [];
        scope.answer = "";
        scope.columnHeaders = [];
    };

    /**
     * A function to close question edition form.
     * @memberof module:questionController
     */
    scope.close = function () {
        scope.removeErrors();
        scope.clearQuestion();
        scope.setShowPreview(false);
        scope.dynamicAnswerSheetControl.closePreview();
        if (scope.questionShown) scope.$emit('toggleQuestion');
    };

    /**
     * The function replaces linebreaks with HTML code.
     * @memberof module:questionController
     * @param val The input string
     * @returns {*} The reformatted line.
     */
    scope.replaceLinebreaksWithHTML = function (val) {
        var output = val.replace(/(?:\r\n|\r|\n)/g, '<br />');
        output = output.replace(/"/g, '&quot;');
        return output.replace(/\\/g, "\\\\");
    };

    /**
     * The function to highlight the source of the errors for a given ID.
     * @memberof module:questionController
     * @param div_val ID of the element to be errorized.
     * @param error_text Description of the occured error.
     */
    scope.errorize = function (div_val, error_text) {
        angular.element("#" + div_val).css('border', "1px solid red");
        if (error_text.length > 0) {
            scope.error_message += error_text + "<br />";
        }
    };

    /**
     * The function to highlight the source of the errors for a given class.
     * @memberof module:questionController
     * @param div_val Class of the element to be errorized.
     * @param error_text Description of the occured error.
     */
    scope.errorizeClass = function (div_val, error_text) {
        angular.element("." + div_val).css('border', "1px solid red");
        if (error_text.length > 0) {
            scope.error_message += error_text + "<br />";
        }
    };

    /**
     * Removes border of a given element.
     * @memberof module:questionController
     * @param element ID of the field whose border will be removed.
     */
    scope.defInputStyle = function (element) {
        if (element !== null || !element.isDefined) {
            angular.element("#" + element).css("border", "");
        }
    };

    /**
     * Calls defInputStyle for all the form elements.
     * @memberof module:questionController
     */
    scope.removeErrors = function () {
        scope.error_message = "";
        var elementsToRemoveErrorsFrom = [
            "questionName",
            "questionTiming",
            "questionStart",
            "questionTimer",
            "qType",
            "matrix",
            "durationSec",
            "durationHour",
            "durationMin",
            "durationDiv"
        ];
        for (var i = 0; i < elementsToRemoveErrorsFrom.length; i++) {
            if (elementsToRemoveErrorsFrom[i] !== undefined) {
                scope.defInputStyle(elementsToRemoveErrorsFrom[i]);
            }
        }
        angular.element(".rowHeading").css("border", "");
    };

    /**
     * Function for checking if the row headings are empty.
     * @memberof module:questionController
     * @param rows The array of rows to be checked.
     * @returns {boolean} Whether or not the row headings are empty.
     */
    scope.rowHeadingsEmpty = function (rows) {
        for (var i = 0; i < rows.length; i++) {
            if (rows[i].text === "" || rows[i].text === null) {
                return true;
            }
        }
        return false;
    };

    /**
     * Checks if a value is a positive number and makes the appropriate errors if this is not the case.
     * @memberof module:questionController
     * @param element The value to be checked.
     * @param val The id of the value, which is used in case the number is not positive.
     */
    scope.isPositiveNumber = function (element, val) {
        if (element === "" || isNaN(element) || element < 0) {
            scope.errorize(val, "Number has to be positive.");
        }
    };


    scope.createPoints = function () {
        var points = '';
        var separator = '';
        var separator2 = '';
        if (scope.question.type === 'matrix' || scope.question.type === 'true-false') {
            if (scope.question.matrixType !== 'textArea') {
                for (var i = 0; i < scope.rows.length; i++) {
                    points += separator;
                    separator2 = '';
                    for (var j = 0; j < scope.rows[i].columns.length; j++) {
                        var currentColumn = scope.rows[i].columns[j];
                        if (currentColumn.points !== '' && currentColumn.points != '0') {
                            points += separator2;
                            var id = parseInt(currentColumn.id) + 1;
                            points += id.toString() + ':' + parseFloat(currentColumn.points) || 0;
                            separator2 = ';';
                        }
                    }
                    separator = '|';
                }
            }
        } else {
            for (var i = 0; i < scope.rows.length; i++) {
                points += separator;
                var currentColumn = scope.rows[i].columns[0];
                if (currentColumn.points !== '' && currentColumn.points != '0') {
                    points += separator2;
                    var id = parseInt(scope.rows[i].id) + 1;
                    points += id.toString() + ':' + parseFloat(currentColumn.points) || 0;
                    separator2 = ';';
                }
            }
        }
        return points;
    };

    scope.createJson = function () {
        if (scope.asked_id) {
            return scope.updatePoints();
        }
        scope.removeErrors();
        if (scope.question.question === undefined || scope.question.question.trim().length === 0 || scope.question.title === undefined || scope.question.title.trim().length === 0) {
            scope.errorize("questionName", "Both title and question are required for a question.");
        }
        if (scope.question.type === undefined) {
            scope.errorize("qType", "Question type must be selected.");
        } else if (scope.question.type === "matrix" && (scope.question.matrixType === undefined || scope.question.matrixType === "")) {
            scope.errorize("check", "Answer type must be selected.");
        } else if ((scope.question.type === "radio-vertical" ||
            scope.question.type === "checkbox-vertical" ||
            scope.question.type === "true-false") &&
            scope.rowHeadingsEmpty(scope.rows)) {
            scope.errorizeClass("rowHeading", "All rows must be filled in.");
        }
        if (scope.rows.length > 0) {
            if ((scope.question.type === "radio-vertical" || scope.question.type === "checkbox-vertical") && scope.rows.length < 2) {
                scope.errorize("matrix", "You must have at least two choices.");
            }
        } else if (scope.question.type !== undefined) {
            scope.errorize("matrix", "You must have at least one row.");
        }
        var timeLimit = "";
        if (scope.question.endTimeSelected) {
            if (scope.question.timeLimit.hours === "") {
                scope.question.timeLimit.hours = 0;
            }
            if (scope.question.timeLimit.minutes === "") {
                scope.question.timeLimit.minutes = 0;
            }
            if (scope.question.timeLimit.seconds === "") {
                scope.question.timeLimit.seconds = 0;
            }
            scope.isPositiveNumber(scope.question.timeLimit.hours, "durationHour");
            scope.isPositiveNumber(scope.question.timeLimit.minutes, "durationMin");
            scope.isPositiveNumber(scope.question.timeLimit.seconds, "durationSec");
            timeLimit = 0;
            timeLimit = parseInt(timeLimit) + parseInt(scope.question.timeLimit.seconds);
            if (scope.question.timeLimit.hours) {
                timeLimit = parseInt(timeLimit) + (scope.question.timeLimit.hours * 60 * 60);
            }
            if (scope.question.timeLimit.minutes) {
                timeLimit = parseInt(timeLimit) + (scope.question.timeLimit.minutes * 60);
            }
            if (timeLimit <= 0) {
                scope.errorize("durationDiv", "Please enter a duration greater then zero or for unending question uncheck the duration box.");
            }
        } else {
            timeLimit = "";
        }

        if (scope.error_message !== "") {
            return;
        }
        scope.removeErrors();
        if (scope.question.type === 'matrix') {

            if (scope.question.matrixType === "radiobutton-horizontal" || scope.question.matrixType === "radiobutton-vertical") {
                scope.question.answerFieldType = "radio";
            }

            if (scope.question.matrixType === "textArea") {
                scope.question.answerFieldType = "text";
            }
            if (scope.question.matrixType === "checkbox") {
                scope.question.answerFieldType = "checkbox";
            }
        }

        //TODO use  JSON.stringify


        scope.question.question = scope.replaceLinebreaksWithHTML(scope.question.question);
        scope.question.title = scope.replaceLinebreaksWithHTML(scope.question.title);

        var headersJson = [];
        if (scope.question.type === "matrix") {
            for (i = 0; i < scope.columnHeaders.length; i++) {
                var header = {
                    'type': scope.columnHeaders[i].type,
                    'id': scope.columnHeaders[i].id,
                    'text': scope.replaceLinebreaksWithHTML(scope.columnHeaders[i].text) || ''
                };
                headersJson.push(header);
            }
        }

        var rowsJson = [];
        for (var i = 0; i < scope.rows.length; i++) {
            var text = scope.replaceLinebreaksWithHTML(scope.rows[i].text);
            var row = {
                'id': scope.rows[i].id,
                'type': scope.rows[i].type,
                'text': scope.replaceLinebreaksWithHTML(scope.rows[i].text)
            };
            var columnsJson = [];
            for (var j = 0; j < scope.rows[i].columns.length; j++) {
                var column = {
                    'id': scope.rows[i].columns[j].id,
                    'type': scope.rows[i].columns[j].type,
                    'rowId': row.id
                };
                columnsJson.push(column);
            }
            row['COLUMNS'] = columnsJson;
            rowsJson.push(row);
        }

        var questionJson = {
            'QUESTION': scope.question.question,
            'TITLE': scope.question.title,
            'TYPE': scope.question.type,
            'ANSWERFIELDTYPE': scope.question.answerFieldType,
            'MATRIXTYPE': scope.question.matrixType,
            'TIMELIMIT': timeLimit,
            'DATA': {'HEADERS': headersJson, 'ROWS': rowsJson}
        };

        scope.json = questionJson;
        scope.dynamicAnswerSheetControl.createAnswer();
        return questionJson;
    };

    /**
     * Validates and saves the question into the database.
     * @memberof module:questionController
     */
    scope.createQuestion = function (ask) {
        var questionJson = scope.createJson();
        if (!questionJson) return;

        var points = scope.createPoints();

        var doc_id = scope.docId;
        var $par = scope.par;
        var par_id = scope.getParId($par);

        var testJson = JSON.stringify(scope.question);
        testJson += JSON.stringify(scope.columnHeaders);
        $window.settings['timelimit'] = questionJson.TIMELIMIT.toString();
        ;
        setTimeout(function () {
            setsetting('timelimit', questionJson.TIMELIMIT.toString());
        }, 1000);

        http({
            method: 'POST',
            url: '/addQuestion/',
            params: {
                'question_id': scope.question.question_id,
                'question_title': scope.question.title,
                'answer': "test", //answerVal,
                'par_id': par_id,
                'doc_id': doc_id,
                'points': points,
                'questionJson': JSON.stringify(questionJson),
                'buster': new Date().valueOf()
            }
        })
            .success(function (data) {
                $window.console.log("The question was successfully added to database");
                scope.removeErrors();
                //TODO: This can be optimized to get only the new one.
                scope.$parent.getQuestions();
                if (ask) {
                    scope.json = JSON.parse(data.questionJson);
                    scope.qId = data.question_id;
                    scope.$emit('askQuestion', {
                        "lecture_id": scope.lectureId,
                        "question_id": scope.qId,
                        "doc_id": scope.docId,
                        "json": scope.json
                    });
                }
            }).error(function () {
                $window.console.log("There was some error creating question to database.");
            });
        scope.close();
    };

    scope.updatePoints = function () {
        var points = scope.createPoints();
        http({
            method: 'POST',
            url: '/updatePoints/',
            params: {
                'asked_id': scope.asked_id,
                'points': points,
                'buster': new Date().valueOf()
            }
        })
            .success(function () {
                $window.console.log("Points successfully updated.");
            }).error(function () {
                $window.console.log("There was some error when updating points.");
            });
        scope.close();
    };

    scope.deleteQuestion = function () {
        var confirmDi = $window.confirm("Are you sure you want to delete this question?");
        if (confirmDi) {
            http({
                url: '/deleteQuestion',
                method: 'POST',
                params: {question_id: scope.qId, doc_id: scope.docId}
            })
                .success(function () {
                    $window.console.log("Deleted question done!");
                    //location.reload();
                    scope.close();
                    //scope.clearQuestion();
                    scope.getQuestions();
                })
                .error(function (error) {

                    $window.console.log(error);
                    scope.getQuestions();

                });
        }
    };

    scope.setShowPreview = function (show) {
        if (show) {
            scope.createJson();
            $(".createQuestion").on('change.createjson', ':input', function () {
                scope.createJson();
            });
        } else {
            $(".createQuestion").off('change.createjson');
        }
    };
}])
;
