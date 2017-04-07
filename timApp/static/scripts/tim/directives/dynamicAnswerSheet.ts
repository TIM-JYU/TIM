import angular = require("angular");
import {timApp} from "tim/app";
import $ = require("jquery");

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
 * @authos Vesa Lappalainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

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


export function getJsonAnswers(answer) {
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


function deletePar(s) {
    if ( !s.startsWith("<p>") ) return s;
    var rs = s.substring(3);
    if ( !rs.endsWith("</p>") ) return s;
    return rs.substring(0,rs.length-4);
}



export function getPointsTable(markupPoints) {
    // Format of markupPoints: 1:1.1;2:1.2;3:1.3||2:3.2
    var pointsTable = [];
    if (markupPoints && markupPoints !== '') {
        var points = markupPoints.split('|');
        for (var i = 0; i < points.length; i++) {
            var rowPoints = points[i].split(';');
            var rowPointsDict = {};
            for (var k = 0; k < rowPoints.length; k++) {
                if (rowPoints[k] !== '') {
                    var colPoints = rowPoints[k].split(':', 2);
                    rowPointsDict[colPoints[0]] = colPoints[1];
                }
            }
            pointsTable.push(rowPointsDict);
        }
    }
    return pointsTable;
}


export function minimizeJson(json) {
    // remove not needed fields from json, call when saving the question
    var result: any = {};
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
        if ( row.id == i+1 && (!row.type || row.type === "question") ) {
            row = row.text; // { text: row.text};
        } else {
            allText = false;
        }
        rrows.push(row);
    }
   // rrows.push({}); // push empty object to force Python json yaml dump to put rows in separate lines. Remember to remove it

    // if ( allText ) rrows = rrows.join("\n"); // oletuksena menee samalle riville kaikki json text muunnoksessa.

    result.rows = rrows;
    result.questionText = json.questionText;
    result.questionTitle = json.questionTitle;
    result.questionType = json.questionType;
    result.answerFieldType = json.answerFieldType;
    result.matrixType = json.matrixType;
    if ( json.timeLimit ) result.timeLimit = json.timeLimit;
    return result;
}


function fixLineBreaks(s) {
    // var result = s.replace(" < "," &lt; ");
    //result = result.replace(" > "," &gt; ");
    let result = s;
    return result;
    //return s.replace("\n","<br />");
}

export function fixQuestionJson(json) {
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
            if (typeof(header.text) === 'undefined') header = {text: header.toString(), type: "header", id: i};
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
        if ( typeof last === 'object' && $.isEmptyObject(last)) rows.splice(n,1); // trust that null is also object!
    }

    for (var i = 0; i < rows.length; i++) {
        var row = rows[i];
        if (typeof(row.text) === 'undefined') row = {text: row.toString(), type: "question"};
        // if (!row.text) continue;
        ir++;
        if (!row.id) row.id = ir+1;
        if (!row.columns ) {
            row.columns = [{}];
            var nh = 0; if ( json.headers ) nh = json.headers.length;
            for (var ic = 1; ic< nh; ic++) row.columns.push({});
        }
        rows[i] = row;
    }
    json.rows = rows;
}

timApp.directive('dynamicAnswerSheet', ['$interval', '$compile', '$rootScope', '$http', 'ParCompiler', function ($interval, $compile, $rootScope, $http, ParCompiler) {
    "use strict";
    return {
        restrict: 'E',
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
            $scope.internalControl.createAnswer = function (params) {
                $scope.result = params.result;
                $scope.gid = params.tid || "";
                $scope.gid = $scope.gid.replace(".","_");
                $scope.buttonText = params.markup.button || params.markup.buttonText || "Answer";

                var answclass = params.answclass || "answerSheet";
                $scope.previousAnswer = params.previousAnswer;
                
                var disabled = '';
                // If showing preview or question result, inputs are disabled
                if (params.preview || $scope.preview || $scope.result) disabled = ' disabled ';
                if ( params.noDisable ) disabled = '';
                
                $element.empty();
                $scope.json = params.markup.json;
                $scope.markup = params.markup;
                var data = $scope.json.data || $scope.json; // compability to old format
                var json = $scope.json;

                fixQuestionJson(data);

                $scope.answerTable = params.answerTable || [];

                // If user has answer to question, create table of answers and select inputs according to it
                if ($scope.previousAnswer) $scope.answerTable = getJsonAnswers($scope.previousAnswer);
                var answerTable = $scope.answerTable;
                var pointsTable = getPointsTable(params.points || params.markup.points);

                $scope.expl = params.expl || params.markup.expl;
                $scope.askedTime = params.askedTime - params.clockOffset;
                $scope.endTime = params.askedTime + $scope.json.timeLimit * 1000 - params.clockOffset;

                // var htmlSheet = $('<div>', {class: answclass});
                var htmlSheet = $('<form>', {class: answclass});
                $scope.htmlSheet = htmlSheet;
                params.htmlSheet = htmlSheet;
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
                var h5 = $('<h5>');
                h5.append(fixLineBreaks(json.questionText));

                htmlSheet.append(h5);
                // htmlSheet.append($('<h5>', {text: json.questionText}));
                if ( params.markup.userpoints !== undefined) {
                    htmlSheet.append($('<p>', {text: "Points: " + params.markup.userpoints }));
                }


                var table = $('<table>', {id: 'answer-sheet-table', class: 'table table-borderless'});


                if (data.headers &&
                    data.headers.length > 0 && !(data.headers[0].text === "" && data.headers.length === 1)) {
                    var tr = $('<tr>', {class: 'answer-heading-row',});
                    if (data.headers.length > 0) {
                        tr.append($('<th>'));
                    }
                    angular.forEach(data.headers, function (header) {
                        var th = $('<th>')
                        th.append(header.text);
                        tr.append(th);
                        // tr.append($('<th>', {text: header.text || header}));
                    });
                    if ($scope.result && $scope.expl)
                        tr.append($('<th>', {}));
                    table.append(tr);
                }

                var ir = -1;
                angular.forEach(data.rows, function (row) {
                    ir++;
                    var pointsRow = {};
                    if ( params.result && pointsTable.length > ir &&  pointsTable[ir] ) pointsRow = pointsTable[ir];
                    var rtext = fixLineBreaks(row.text);
                    var tr = $('<tr>');
                    if (json.questionType === "matrix" || json.questionType === "true-false") {
                        var td = $('<td>');
                        td.append(rtext);
                        tr.append(td);
                        //tr.append($('<td>', {text: row.text}));
                    }
                    var header = 0;
                    //TODO: Needs correct JSON to be made better way
                    for (var ic = 0; ic < row.columns.length; ic++) {
                        var group;
                        group = $scope.cg() + ir;

                        if (json.questionType === "matrix" || json.questionType === "true-false") {
                            var value = "" +(ic + 1);// (row.columns[ic].id + 1).toString();

                            var colTDPoints = undefined;
                            var colPtsClass = 'qst-normal';
                            if ( value in pointsRow ) {
                                var colPoints = pointsRow[value];
                                colTDPoints = $('<p>', {class: 'qst-points'}).append(colPoints);
                                if ( colPoints > 0 ) colPtsClass =  'qst-correct';
                            }
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
                                if (disabled !== '') textArea.attr('disabled', "disabled");
                                if (data.headers && data.headers.length === 1 && data.headers[0].text === "" && data.rows.length === 1) {
                                    textArea.attr('style', 'height:200px');
                                }
                                tr.append($('<td>', {class: 'answer-button'}).append($('<label>').append(textArea)));
                                header++;
                            } else {
                                // group = $scope.cg() + rtext.replace(/[^a-zA-Z0-9]/g, "");
                                var checked = false;
                                if (answerTable && ir < answerTable.length ) {
                                    checked = (answerTable[ir].indexOf(value) >= 0);
                                }
                                var input = $('<input>', {
                                    type: json.answerFieldType,
                                    name: group,
                                    value: ic + 1, // parseInt(row.columns[ic].id) + 1,
                                    checked: checked
                                });
                                if ( json.answerFieldType === "radio" ) {
                                    input.prop("form", htmlSheet as any)
                                    input.click(uncheckRadio);  // TODO: Tähän muutoskäsittely ja jokaiseen tyyppiin tämä
                                }
                                if (disabled !== '') input.attr('disabled', "disabled");

                                var td = $('<td>', {class: 'answer-button'});
                                var ispan = $('<span>', {class:colPtsClass});
                                ispan.append(input);
                                td.append($('<label>').append(ispan))

                                if ( colTDPoints ) td.append(colTDPoints);
                                tr.append(td);
                                header++;
                            }
                        } else {
                            var value = ""+(ir+1); // (row.id + 1).toString();
                            var colTDPoints = undefined;
                            var colPtsClass = 'qst-normal';
                            var pointsRow = {};
                            if ( params.result && pointsTable.length > 0 &&  pointsTable[0] ) pointsRow = pointsTable[0];
                            if ( value in pointsRow ) {
                                var colPoints = pointsRow[value];
                                colTDPoints = $('<p>', {class: 'qst-points'}).append(colPoints);
                                if ( colPoints > 0 ) colPtsClass =  'qst-correct';
                            }
                            row.columns[ic].id = ic;


                            var type = row.type || "question";
                            group = $scope.cg() + type.replace(/[^a-zA-Z0-9]/g, "");
                            var checked = false;
                            if (answerTable && answerTable.length > 0) {
                                checked = (answerTable[0].indexOf(value) >= 0);
                            }
                            var input = $('<input>', {
                                type: json.answerFieldType,
                                name: group,
                                value: ir + 1, //parseInt(row.id) + 1,
                                checked: checked
                            });
                            if ( json.answerFieldType === "radio" ) {
                                input.prop("form", htmlSheet as any)
                                input.click(uncheckRadio);
                            }
                            if (disabled !== '') input.attr('disabled', "disabled");
                            var label = $('<label>');
                            var ispan = $('<span>', {class:colPtsClass});
                            ispan.append(input);
                            label.append(ispan).append(" " + deletePar("" + rtext));
                            var td = $('<td>', {class: 'answer-button2'});
                            td.append(label);
                            if ( colTDPoints ) td.append(colTDPoints);
                            tr.append(td);
                        }
                    }
                    // If showing question results, add question rows explanation
                    if ($scope.result && $scope.expl ) {
                        var expl = '';
                        var ir1 = (ir+1).toString()
                        if ( ir1 in $scope.expl ) expl = $scope.expl[ir1];
                        var tde = $('<td>', {class: 'explanation'});
                        tde.append(expl)
                        // tr.append($('<td>', {class: 'explanation', text: expl}));
                        tr.append(tde);
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
                        if ( params.isAsking ) $input[0].focus();
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
                ParCompiler.processAllMath($scope.htmlSheet);
            };
            // createAnswer ends

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
                        var checkedInputs = $('input[name=' + groupName + ']:checked', $scope.htmlSheet) as any as Array<HTMLInputElement>;
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