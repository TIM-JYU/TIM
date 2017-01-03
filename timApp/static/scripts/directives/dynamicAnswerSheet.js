/**
 * Created by localadmin on 25.5.2015.
 * Directive for dynamic answer sheet. Sheet to answer lecture questions.
 * If preview parameter is used, inputs are disable and there is no progressbar or answer button
 * @module dynamicAnswerSheet
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

var angular;

function uncheckRadio(e) {
    // set this to click-method if you want a radio that can be uncheked.  for the radio there
    // must be also property form that has other radios.
    var elem = $(this);
    var form = elem.prop("form");
    if ( !form ) return;
    var gn = elem.prop("name");
    if ( elem.prop('previousValue') === true ) {
        elem.prop('checked', false);
    } else {
        $('input[name=' + gn + ']', form).prop('previousValue', false);
        // elem.prop('previousValue', false);
    }
    elem.prop('previousValue', elem.prop('checked'));
}


function getJsonAnswers(answer) {
    // Converts answer string to JSON table
    if ( answer.length > 0 && answer[0] === "[" ) {
        return JSON.parse(answer);
    }
    var single_answers = [];
    var all_answers = answer.split('|');

    for (var i = 0; i < all_answers.length; i++) {
        single_answers.push(all_answers[i].split(','))
    }
    return single_answers;
}


function minimizeJson(json) {
    // remove not needed fields from json, call when saving the question
    var result = {};
    if ( json.headers ) {
        result.headers = [];
        for (var i = 0; i < json.headers.length; i++) {
            var header = json.headers[i]
            if ( header.id == i  &&  header.type === "header" ) header = header.text;
            result.headers.push(header);
        }
    }

    var ir = -1;
    var allText = true;
    var rows = json.rows;
    var rrows = [];

    for (var i = 0; i < rows.length; i++) {
        var row = rows[i];
        if ( row.id == i && (!row.type || row.type === "question") ) {
            row = row.text; // { text: row.text};
        } else {
            allText = false;
        }
        rrows.push(row);
    }
    rrows.push({}); // push empty object to force Python json yaml dump to put rows in separate lines. Remember to remove it

    // if ( allText ) rrows = rrows.join("\n"); // oletuksena menee samalle riville kaikki json text muunnoksessa.

    result.rows = rrows;
    result.questionText = json.questionText;
    result.title = json.title;
    result.questionType = json.questionType;
    result.answerFieldType = json.answerFieldType;
    result.matrixType = json.matrixType;
    if ( json.timeLimit ) result.timeLimit = json.timeLimit;
    return result;
}


function fixQuestionJson(json) {
    // fill all missing fields from question json, call before use json
    var columnHeaders = [];
    if ( json.data ) {
        json.headers = json.data.headers;
        json.rows = json.data.rows;
    }
    if ( json.headers ) {
        var headers = json.headers;
        if ( Array !== headers.constructor ) { // if just on text string
            var jrows = headers.split("\n");
            headers = [];
            var ir = -1;
            for (var i=0; i<jrows.length; i++) {
                var jrow = jrows[i];
                if (jrow) {
                    ir++;
                    headers.push({text: jrow.toString(), type: "header", id: ir})
                }
            }
        }

        json.headers = headers;

        for (var i = 0; i < json.headers.length; i++) {
            var header = json.headers[i]
            if (!header.text) header = {text: header.toString(), type: "header", id: i};
            if (!header.id) header.id = i;
            if (!header.type) header.type = "header";
            json.headers[i] = header;
        }
    }

    if ( !json.answerFieldType ) json.answerFieldType = "radio";

    if (json.questionType === "true-false" && (!json.headers || json.headers.length == 0 ) ) {
        json.headers[0] = {"type": "header", "id": 0, "text": "True"};
        json.headers[1] = {"type": "header", "id": 1, "text": "False"};
    }


    var rows =  json.rows;
    if ( Array !== rows.constructor ) { // if just on text string
        var jrows = rows.split("\n");
        rows = [];
        for (var i=0; i<jrows.length; i++) {
            var jrow = jrows[i];
            if (jrow) rows.push({text: jrow.toString(), type: "question", id: i})
        }
    }

    var ir = -1;
    var n = rows.length-1;
    if ( n >= 0 ) {
        var last = rows[n];  // remove ending object if needed
        if ( $.isEmptyObject(last)) rows.splice(n,1);
    }

    for (var i = 0; i < rows.length; i++) {
        var row = rows[i];
        if (!row.text) row = {text: row.toString(), type: "question"};
        if (!row.text) continue;
        ir++;
        if (!row.id) row.id = ir;
        if (!row.columns ) {
            row.columns = [{}];
            var nh = 0; if ( json.headers ) nh = json.headers.length;
            for (var ic = 1; ic< nh; ic++) row.columns.push({});
        }
        rows[i] = row;
    }
    json.rows = rows;
}

var timApp = angular.module('timApp');
timApp.directive('dynamicAnswerSheet', ['$interval', '$compile', '$rootScope', '$http', function ($interval, $compile, $rootScope, $http) {
    "use strict";
    return {
        restrict: 'E',
        replace: "true",
        scope: {
            control: '=',
            preview: '@'
        },
        transclude: true,
        link: function ($scope, $element) {
            var promise;
            var timeLeft;
            var barFilled;
            $scope.internalControl = $scope.control || {};

            $scope.cg = function() {
                return "group"; // + $scope.gid;
            }

            /**
             * Creates question answer/preview form.
             */
            $scope.internalControl.createAnswer = function (answclass, preview) {
                $scope.result = $scope.$parent.result;
                $scope.gid = $scope.$parent.tid || "";
                $scope.gid = $scope.gid.replace(".","_");
                $scope.buttonText = $scope.$parent.markup.button || $scope.$parent.markup.buttonText || "Answer";

                if ( !answclass ) answclass = "answerSheet";
                $scope.previousAnswer = $scope.$parent.previousAnswer;
                var disabled = '';
                // If showing preview or question result, inputs are disabled
                if (preview || $scope.preview || $scope.result) disabled = ' disabled ';
                $element.empty();
                $scope.json = $scope.$parent.markup.json;
                $scope.markup = $scope.$parent.markup;
                var data = $scope.json.data || $scope.json; // compability to old format
                var json = $scope.json;

                fixQuestionJson(data);

                $scope.answerTable = $scope.$parent.answerTable || [];

                // If user has answer to question, create table of answers and select inputs according to it
                if ($scope.previousAnswer) $scope.answerTable = getJsonAnswers($scope.previousAnswer);
                var answerTable = $scope.answerTable;

                $scope.expl = $scope.$parent.expl;
                $scope.askedTime = $scope.$parent.askedTime - $scope.$parent.clockOffset;
                $scope.endTime = $scope.$parent.askedTime + $scope.json.timeLimit * 1000 - $scope.$parent.clockOffset;

                // var htmlSheet = $('<div>', {class: answclass});
                var htmlSheet = $('<form>', {class: answclass});
                $scope.htmlSheet = htmlSheet;
                if ($scope.json.timeLimit !== "" && $scope.endTime  && !$scope.preview && !$scope.result) {
                    var progress = $('<progress>', {
                        max: ($scope.endTime - $scope.askedTime),
                        id: 'progressBar'
                    });
                    htmlSheet.append(progress);
                    htmlSheet.append($('<span>', {
                        class: 'progresslabel',
                        id: 'progressLabel',
                        text: $scope.json.timeLimit + " s"
                    }));
                }
                htmlSheet.append($('<h5>', {text: json.questionText}));
                var table = $('<table>', {id: 'answer-sheet-table', class: 'table table-borderless'});


                if (data.headers &&
                    data.headers.length > 0 && !(data.headers[0].text === "" && data.headers.length === 1)) {
                    var tr = $('<tr>', {class: 'answer-heading-row',});
                    if (data.headers.length > 1) {
                        tr.append($('<th>'));
                    }
                    angular.forEach(data.headers, function (header) {
                        tr.append($('<th>', {text: header.text || header}));
                    });
                    if ($scope.result && $scope.markup.expl)
                        tr.append($('<th>', {}));
                    table.append(tr);
                }

                var ir = -1;
                angular.forEach(data.rows, function (row) {
                    ir++;

                    var tr = $('<tr>');
                    if (json.questionType === "matrix" || json.questionType === "true-false") {
                        tr.append($('<td>', {text: row.text}));
                    }
                    var header = 0;
                    //TODO: Needs correct JSON to be made better way
                    for (var ic = 0; ic < row.columns.length; ic++) {
                        var group;
                        group = $scope.cg() + ir;
                        if (json.questionType === "matrix" || json.questionType === "true-false") {
                            row.columns[ic].id = ic;
                            if (json.answerFieldType === "text") {
                                var text = "";
                                if (answerTable && ir < answerTable.length && ic < answerTable[ir].length) {
                                    text = answerTable[ir][ic];
                                }
                                var textArea = $('<textarea>', {
                                    id: 'textarea-answer',
                                    name: group
                                });
                                textArea.text(text);
                                if (disabled !== '') textArea.attr('disabled', true);
                                if (data.headers[0].text === "" && data.headers.length === 1 && data.rows.length === 1) {
                                    textArea.attr('style', 'height:200px');
                                }
                                tr.append($('<td>', {class: 'answer-button'}).append($('<label>').append(textArea)));
                                header++;
                            } else {
                                // group = $scope.cg() + row.text.replace(/[^a-zA-Z0-9]/g, "");
                                var checked = false;
                                if (answerTable && ir < answerTable.length ) {
                                    var value = "" +(ic + 1);// (row.columns[ic].id + 1).toString();
                                    checked = (answerTable[ir].indexOf(value) >= 0);
                                }
                                var input = $('<input>', {
                                    type: json.answerFieldType,
                                    name: group,
                                    value: ic + 1, // parseInt(row.columns[ic].id) + 1,
                                    checked: checked
                                });
                                if ( json.answerFieldType === "radio" ) {
                                    input.prop("form", htmlSheet)
                                    input.click(uncheckRadio);  // TODO: Tähän muutoskäsittely ja jokaiseen tyyppiin tämä
                                }
                                if (disabled !== '') input.attr('disabled', true);
                                tr.append($('<td>', {class: 'answer-button'}).append($('<label>').append(input)));
                                header++;
                            }
                        } else {
                            var type = row.type || "question";
                            group = $scope.cg() + type.replace(/[^a-zA-Z0-9]/g, "");
                            var checked = false;
                            if (answerTable && answerTable.length > 0) {
                                var value = ""+(ir+1); // (row.id + 1).toString();
                                checked = (answerTable[0].indexOf(value) >= 0);
                            }
                            var input = $('<input>', {
                                type: json.answerFieldType,
                                name: group,
                                value: ir + 1, //parseInt(row.id) + 1,
                                checked: checked
                            });
                            if ( json.answerFieldType === "radio" ) {
                                input.prop("form", htmlSheet)
                                input.click(uncheckRadio);
                            }
                            if (disabled !== '') input.attr('disabled', true);
                            var label = $('<label>');
                            label.append(input).append(" " + row.text);
                            tr.append($('<td>', {class: 'answer-button2'}).append(label));
                        }
                    }
                    // If showing question results, add question rows explanation
                    if ($scope.result && $scope.markup.expl && ir.toString() in $scope.markup.expl) {
                        tr.append($('<td>', {class: 'explanation', text: $scope.markup.expl[ir.toString()]}));
                    }
                    table.append(tr);
                });

                htmlSheet.append($('<div>').append(table));
                $element.append(htmlSheet);
                $compile($scope);

                if (!$scope.preview && !$scope.result) {
                    var $table = $element.find('#answer-sheet-table');
                    window.setTimeout(function () {
                        var $table = $element.find('#answer-sheet-table');
                        var $input = null;
                        if ($scope.json.answerFieldType !== "text")
                            $input = $table.find('input:first');
                        else
                            $input = $table.find('textarea:first');
                        if ( $scope.$parent.isAsking ) $input[0].focus();
                    }, 0);
                    //
                    $table.on('keyup.send', $scope.answerWithEnter);
                    var now = new Date().valueOf();
                    timeLeft = $scope.endTime - now;
                    barFilled = 0;
                    if ( $scope.endTime && $scope.json.timeLimit !== "") {
                        var timeBetween = 500;
                        var maxCount = timeLeft / timeBetween + 5 * timeBetween;
                        $scope.progressElem = $("#progressBar");
                        $scope.progressText = $("#progressLabel");
                        $scope.start = function () {
                            promise = $interval($scope.internalControl.updateBar, timeBetween, maxCount);
                        };
                        $scope.start();
                    }
                }
            };


            /**
             * Use time parameter to either close question/points window or extend question end time.
             * If time is null, question/points is closed.
             * Else time is set as questions new end time.
             */
            $scope.$on('update_end_time', function (event, time) {
                if (time !== null) {
                    $scope.endTime = time - $scope.$parent.clockOffset;
                    $scope.progressElem.attr("max", $scope.endTime - $scope.askedTime);
                } else {
                    if (!$scope.$parent.isLecturer) {
                        $interval.cancel(promise);
                        $element.empty();
                        $scope.$emit('closeQuestion');
                    }
                }
            });

            /**
             * Updates progressbar and time left text
             * @memberof module:dynamicAnswerSheet
             */
            $scope.internalControl.updateBar = function () {
                //TODO: Problem with inactive tab.
                var now = new Date().valueOf();
                if ( !$scope.endTime || !promise ) return;
                timeLeft = $scope.endTime - now;
                barFilled = ($scope.endTime - $scope.askedTime) - ($scope.endTime - now);
                $scope.progressElem.attr("value", (barFilled));
                $scope.progressText.text(Math.max((timeLeft / 1000), 0).toFixed(0) + " s");
                $scope.progressElem.attr("content", (Math.max((timeLeft / 1000), 0).toFixed(0) + " s"));
                if (barFilled >= $scope.progressElem.attr("max")) {
                    $interval.cancel(promise);
                    if (!$scope.$parent.isLecturer && !$scope.$parent.questionEnded) {
                        $scope.internalControl.answerToQuestion();
                    } else {
                        $scope.progressText.text("Time's up");
                    }
                    $scope.$parent.questionEnded = true;
                }
            };

            $scope.internalControl.endQuestion = function () {
                if ( !promise ) return;
                $interval.cancel(promise);
                var max = $scope.progressElem.attr("max");
                $scope.progressElem.attr("value", max);
                $scope.progressText.text("Time's up");
            };

            $scope.answerWithEnter = function (e) {
                if (e.keyCode === 13) {
                    $('#answer-sheet-table').off('keyup.send');
                    $scope.$parent.answer();
                }
            };


            $scope.internalControl.getAnswers = function() {
                var answers = [];
                var data = $scope.json.data || $scope.json;
                if (angular.isDefined(data.rows)) {
                    var groupName; // data.rows[ir].text.replace(/[^a-zA-Z0-9]/g, '');
                    if ($scope.json.questionType === "matrix" || $scope.json.questionType === "true-false") {
                        for (var ir = 0; ir < data.rows.length; ir++) {
                            groupName = $scope.cg() + ir;
                            var answer = [];
                            var matrixInputs;
                            // groupName = $scope.cg() + ir; // data.rows[ir].text.replace(/[^a-zA-Z0-9]/g, '');

                            if ($scope.json.answerFieldType === "text") {
                                matrixInputs = $('textarea[name=' + groupName + ']', $scope.htmlSheet);
                                for (var ic = 0; ic < matrixInputs.length; ic++) {
                                    var v = matrixInputs[ic].value || "";
                                    answer.push(v);
                                }

                                answers.push(answer);
                                continue;
                            }

                            matrixInputs = $('input[name=' + groupName + ']:checked', $scope.htmlSheet);

                            for (var k = 0; k < matrixInputs.length; k++) {
                                var v = matrixInputs[k].value || "";
                                answer.push(v);
                            }
                            if (matrixInputs.length <= 0) {
                                answer.push("");
                            }
                            answers.push(answer);
                        }
                    }
                    else {
                        answers.push([]);
                        var type = data.rows[0].type || "question";
                        groupName = $scope.cg() + type.replace(/[^a-zA-Z0-9]/g, '');
                        var checkedInputs = $('input[name=' + groupName + ']:checked', $scope.htmlSheet);
                        for (var j = 0; j < checkedInputs.length; j++) {
                            answers[0].push(checkedInputs[j].value);
                        }

                        if (checkedInputs.length <= 0) {
                            answers[0].push(""); // was "undefined"
                        }
                    }
                }

                if (angular.isDefined(data.columns)) {
                    angular.forEach(data.columns, function (column) {
                        var groupName = $scope.cg() + column.Value.replace(/ /g, '');
                        answers.push($('input[name=' + groupName + ']:checked').val());
                    });
                }

                return answers;

            }

            /**
             * Function to create question answer and send it to server.
             * @memberof module:dynamicAnswerSheet
             */
            $scope.internalControl.answerToQuestion = function () {

                var answers = $scope.internalControl.getAnswers();

                if (!$scope.$parent.isLecturer) {
                    $element.empty();
                    $interval.cancel(promise);
                }
                $scope.$emit('answerToQuestion', {answer: answers, askedId: $scope.$parent.askedId});
            };

            /**
             * Closes question window and clears updateBar interval.
             * If user is lecturer, also closes answer chart window.
             * @memberof module:dynamicAnswerSheet
             */
            $scope.internalControl.closeQuestion = function () {
                $interval.cancel(promise);
                $element.empty();
                $scope.$emit('closeQuestion');
                if ($scope.$parent.isLecturer) $rootScope.$broadcast('closeAnswerSheetForGood');
            };

            $scope.internalControl.closePreview = function () {
                $element.empty();
            };
        }

    };
}
])
;