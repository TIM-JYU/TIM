timLogTime("answerbrowser3 load","answ");

var angular;
var timApp = angular.module('timApp');
var LAZYWORD = "lazylazylazy";
var LAZYSTART="<!--lazy ";
var LAZYEND =" lazy-->";
var RLAZYSTART = new RegExp(LAZYSTART, 'g');
var RLAZYEND = new RegExp(LAZYEND, 'g');


function makeNotLazy(html) {
    var s = html.replace(RLAZYSTART,"");
    var i = s.lastIndexOf(LAZYEND);
    if ( i >= 0 ) s = s.substring(0,i);
    s = s.replace(RLAZYEND,""); 
    s = s.replace(/-LAZY->/g,"-->");
    s = s.replace(/<!-LAZY-/g,"<!--");
    return s;
}

timApp.directive("answerbrowserlazy", ['Upload', '$http', '$sce', '$compile', '$window',
    function (Upload, $http, $sce, $compile, $window) {
        "use strict";
        timLogTime("answerbrowserlazy directive function","answ");
        return {
            restrict: 'E',
            scope: {
                taskId: '@'
            },

            controller: function ($scope) {
                timLogTime("answerbrowserlazy ctrl function", "answ", 1);
                $scope.compiled = false;

                /**
                 * Returns whether the given task id is valid.
                 * A valid task id is of the form '1.taskname'.
                 * @param taskId {string} The task id to validate.
                 * @returns {boolean} True if the task id is valid, false otherwise.
                 */
                $scope.isValidTaskId = function (taskId) {
                    return taskId.slice(-1) !== ".";
                };
            },
            
            link: function ($scope, $element, $attrs) {
                timLogTime("answerbrowserlazy link function","answ",1);

                $element.parent().on('mouseenter touchstart', function () {
                    var plugin = $element.parents('.par').find('.parContent');
                    if ( $scope.compiled ) return;
                    $scope.compiled = true;
                    if (!$scope.$parent.noBrowser && $scope.isValidTaskId($scope.taskId)) {
                        var newHtml = '<answerbrowser task-id="' + $scope.taskId + '"></answerbrowser>';
                        var newElement = $compile(newHtml);
                        var parent = $element.parents(".par")[0];
                        parent.replaceChild(newElement($scope.$parent)[0], $element[0]);
                    }
                    // Next the inside of the plugin to non lazy
                    var origHtml = plugin[0].innerHTML;
                    if ( origHtml.indexOf(LAZYSTART) >= 0 ) {

                    } else plugin = null;
                    if ( plugin ) {
                        var newPluginHtml = makeNotLazy(origHtml);
                        var newPluginElement = $compile(newPluginHtml);
                        plugin.html(newPluginElement($scope));
                        $scope.$parent.processAllMathDelayed(plugin);
                    }
                });
            }
        };
    }]);


timApp.directive("answerbrowser", ['Upload', '$http', '$sce', '$compile', '$window', '$filter',
    function (Upload, $http, $sce, $compile, $window, $filter) {
        "use strict";
        timLogTime("answerbrowser directive function","answ");
        return {
            templateUrl: "/static/templates/answerBrowser.html",
            restrict: 'E',
            scope: {
                taskId: '@'
            },
            controller: function ($scope) {
            },
            link: function ($scope, $element, $attrs) {
                $scope.element = $element.parents('.par');
                $scope.parContent = $scope.element.find('.parContent');
                //$scope.$parent = $scope.$parent; // muutos koska scope on syntynyt tuon toisen lapseksi
                timLogTime("answerbrowser link","answ");
                
                $scope.$watch("taskId", function (newValue, oldValue) {
                    if (newValue === oldValue) {
                        return;
                    }
                    if ($scope.$parent.teacherMode) {
                        $scope.getAvailableUsers();
                    }
                    $scope.getAvailableAnswers();
                });

                $scope.updatePoints = function () {
                    $scope.points = $scope.selectedAnswer.points;
                    if ($scope.points !== null && $scope.points.length > 0) {
                        $scope.giveCustomPoints = $scope.points.slice(-1) === " ";
                    } else {
                        $scope.giveCustomPoints = false;
                    }
                };

                $scope.loading = 0;
                $scope.changeAnswer = function () {
                    if ($scope.selectedAnswer === null) {
                        return;
                    }
                    $scope.updatePoints();
                    var $par = $scope.element;
                    var par_id = $scope.$parent.getParId($par);
                    $scope.loading++;
                    $http.get('/getState', {
                        params: {
                            doc_id: $scope.$parent.docId,
                            par_id: par_id,
                            user_id: $scope.user.id,
                            answer_id: $scope.selectedAnswer.id
                        }
                    }).success(function (data, status, headers, config) {
                        var newhtml = makeNotLazy(data.html);
                        var plugin = $par.find('.parContent');
                        plugin.html($compile(newhtml)($scope));
                        plugin.css('opacity', '1.0');
                        $scope.$parent.processAllMathDelayed(plugin);
                    }).error(function (data, status, headers, config) {
                        $scope.error = 'Error getting state: ' + data.error;
                    }).finally(function () {
                        $scope.loading--;
                    });
                };

                $scope.next = function () {
                    var newIndex = $scope.findSelectedAnswerIndex() - 1;
                    if (newIndex < 0) {
                        newIndex = $scope.filteredAnswers.length - 1;
                    }
                    $scope.selectedAnswer = $scope.filteredAnswers[newIndex];
                    $scope.changeAnswer();
                };

                $scope.previous = function () {
                    var newIndex = $scope.findSelectedAnswerIndex() + 1;
                    if (newIndex >= $scope.filteredAnswers.length) {
                        newIndex = 0; 
                    }
                    $scope.selectedAnswer = $scope.filteredAnswers[newIndex];
                    $scope.changeAnswer();
                };

                $scope.setNewest = function () {
                    if ($scope.filteredAnswers.length > 0) {
                        $scope.selectedAnswer = $scope.filteredAnswers[0];
                        $scope.changeAnswer();
                    }
                };

                $scope.indexOfSelected = function () {
                    if ( !$scope.filteredAnswers || !$scope.selectedAnswer ) return -1;
                    var arrayLength = $scope.filteredAnswers.length;
                    for (var i = 0; i < arrayLength; i++) {
                        if ($scope.filteredAnswers[i].id === $scope.selectedAnswer.id) {
                            return i;
                        }
                    }
                    return -1;
                };

                $scope.getBrowserData = function () {
                    if ($scope.answers.length > 0 && $scope.selectedAnswer)
                        return {
                            answer_id: $scope.selectedAnswer.id,
                            saveTeacher: $scope.saveTeacher,
                            teacher: $scope.$parent.teacherMode,
                            points: $scope.points,
                            giveCustomPoints: $scope.giveCustomPoints,
                            userId: $scope.user.id,
                            saveAnswer: !$scope.$parent.noBrowser
                        };
                    else
                        return {
                            saveTeacher: false,
                            teacher: $scope.$parent.teacherMode,
                            saveAnswer: !$scope.$parent.noBrowser
                        };
                };

                $scope.getAvailableUsers = function () {
                    $scope.loading++;
                    $http.get('/getTaskUsers/' + $scope.taskId, {params: {group: $scope.$parent.group}})
                        .success(function (data, status, headers, config) {
                            $scope.users = data;
                        }).error(function (data, status, headers, config) {
                            $scope.error = 'Error getting users: ' + data.error;
                        }).finally(function () {
                            $scope.loading--;
                        });
                };

                $scope.getAvailableAnswers = function (updateHtml) {
                    updateHtml = (typeof updateHtml === "undefined") ? true : updateHtml;
                    if ( !$scope.$parent.rights || !$scope.$parent.rights.browse_own_answers) {
                        return;
                    }
                    if ($scope.user === null) {
                        return;
                    }
                    $scope.loading++;
                    $http.get('/answers/' + $scope.taskId + '/' + $scope.user.id + '?rnd='+Math.random())  
                        .success(function (data, status, headers, config) {
                            if (data.length > 0 && ($scope.hasUserChanged() || data.length !== ($scope.answers || []).length)) {
                                $scope.answers = data;
                                $scope.selectedAnswer = $scope.answers[0];
                                $scope.updatePoints();
                                if (updateHtml) {
                                    $scope.changeAnswer();
                                }
                            } else {
                                $scope.answers = data;
                                if ($scope.answers.length == 0 && $scope.$parent.teacherMode) {
                                    $scope.dimPlugin();
                                }
                                $scope.updateFiltered();
                                var i = $scope.findSelectedAnswerIndex();
                                if (i >= 0) {
                                    $scope.selectedAnswer = $scope.filteredAnswers[i];
                                    $scope.updatePoints();
                                }
                            }
                            $scope.fetchedUser = $scope.user;
                        }).error(function (data, status, headers, config) {
                            $scope.error = 'Error getting answers: ' + data.error;
                        }).finally(function () {
                            $scope.loading--;
                        });
                };

                $scope.$on('answerSaved', function (event, args) {
                    if (args.taskId === $scope.taskId) {
                        $scope.getAvailableAnswers(false);
                        // HACK: for some reason the math mode is lost because of the above call, so we restore it here
                        $scope.$parent.processAllMathDelayed($scope.element.find('.parContent'));
                    }
                });

                $scope.hasUserChanged = function () {
                    return ($scope.user || {}).id !== ($scope.fetchedUser || {}).id;
                };

                $scope.$on('userChanged', function (event, args) {
                    $scope.user = args.user;
                    $scope.firstLoad = false;
                    $scope.shouldUpdateHtml = true;
                    if ($scope.hasUserChanged()) {
                        $scope.dimPlugin();
                    } else {
                        $scope.parContent.css('opacity', '1.0');
                    }
                });

                $scope.dimPlugin = function () {
                    $scope.parContent.css('opacity', '0.3');
                };

                $scope.allowCustomPoints = function () {
                    if ($scope.taskInfo === null) {
                        return false;
                    }
                    return $scope.taskInfo.userMin !== null && $scope.taskInfo.userMax !== null;
                };

                $scope.loadIfChanged = function () {
                    if ($scope.hasUserChanged()) {
                        $scope.getAvailableAnswers($scope.shouldUpdateHtml);
                        $scope.loadInfo();
                        $scope.firstLoad = false;
                        $scope.shouldUpdateHtml = false;
                    }
                };

                $scope.showTeacher = function () {
                    return $scope.$parent.teacherMode && $scope.$parent.rights.teacher;
                };

                $scope.getTriesLeft = function () {
                    if ($scope.taskInfo === null) {
                        return null;
                    }
                    return Math.max($scope.taskInfo.answerLimit - $scope.answers.length, 0);
                };

                $scope.loadInfo = function () {
                    if ($scope.taskInfo !== null) {
                        return;
                    }
                    $scope.loading++;
                    $http.get('/taskinfo/' + $scope.taskId)
                        .success(function (data, status, headers, config) {
                            $scope.taskInfo = data;
                        }).error(function (data, status, headers, config) {
                            $scope.error = 'Error getting taskinfo: ' + data.error;
                        }).finally(function () {
                            $scope.loading--;
                        });
                };

                
                $scope.checkUsers = function () {
                    if ($scope.loading > 0) {
                        return;
                    }
                    $scope.loadIfChanged();
                    if ($scope.$parent.teacherMode && $scope.users === null) {
                        $scope.users = [];
                        if ($scope.$parent.users.length > 0) {
                            $scope.getAvailableUsers();
                        }
                    }
                };


                $scope.getAllAnswers = function() {
                    var sPageURL = decodeURIComponent(window.location.search.substring(1));
                    var age = "";
                    if ( sPageURL.indexOf("age=min") >= 0 ) age = "&age=min";
                    $scope.loading++;
                    $http.get('/allAnswers/' + $scope.taskId + '?rnd='+Math.random() + age, {params: {group: $scope.$parent.group}})
                        .success(function (data, status, headers, config) {
                            $scope.allAnswers = data.join("\n\n----------------------------------------------------------------------------------\n");
                            var nw = $window;
                            //var nd = nw.document;
                            //nd.write(data[0]);
                        }).error(function (data, status, headers, config) {
                            $scope.error = 'Error getting answers: ' + data.error;
                        }).finally(function () {
                            $scope.loading--;
                        });
                };

                $scope.findSelectedAnswerIndex = function () {
                    if ($scope.filteredAnswers === null) {
                        return -1;
                    }
                    for (var i = 0; i < $scope.filteredAnswers.length; i++) {
                        if ($scope.filteredAnswers[i].id === $scope.selectedAnswer.id) {
                            return i;
                        }
                    }
                    return -1;
                };


                $scope.getLink = function() {
                     return 'data:text/plain;charset=UTF-8,' + encodeURIComponent($scope.allAnswers);
                };

                if ( $scope.$parent.selectedUser ) {
                    $scope.user = $scope.$parent.selectedUser;
                }
                else if ($scope.$parent && $scope.$parent.users && $scope.$parent.users.length > 0) { 
                    $scope.user = $scope.$parent.users[0];
                } else {
                    $scope.user = null;
                }

                $scope.fetchedUser = null;
                $scope.firstLoad = true;
                $scope.shouldUpdateHtml = $scope.user !== $scope.$parent.users[0];
                if ($scope.shouldUpdateHtml) {
                    $scope.dimPlugin();
                }
                $scope.saveTeacher = false;
                $scope.users = null;
                $scope.answers = [];
                $scope.filteredAnswers = [];
                $scope.onlyValid = true;
                $scope.selectedAnswer = null;
                $scope.taskInfo = null;
                $scope.anyInvalid = false;
                $scope.giveCustomPoints = false;

                $scope.updateFiltered = function (newValues, oldValues, scope) {
                    $scope.anyInvalid = false;
                    $scope.filteredAnswers = $filter('filter')($scope.answers, function (value, index, array) {
                        if (value.valid) {
                            return true;
                        }
                        $scope.anyInvalid = true;
                        return !$scope.onlyValid;
                    });
                    if ($scope.findSelectedAnswerIndex() < 0) {
                        $scope.setNewest();
                    }
                };

                $scope.$watchGroup(['onlyValid', 'answers'], $scope.updateFiltered);

                // call checkUsers automatically for now; suitable only for lazy mode!
                $scope.checkUsers();
                $element.parent().on('mouseenter touchstart', function () {
                    $scope.checkUsers();
                });
            }
        };
    }]);
