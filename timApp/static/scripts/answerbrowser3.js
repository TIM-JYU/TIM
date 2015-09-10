timLogTime("answerbrowser3 load","answ");

var angular, Waypoint;
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

timApp.directive("answerbrowserlazy", ['$upload', '$http', '$sce', '$compile', '$window',
    function ($upload, $http, $sce, $compile, $window) {
        "use strict";
        timLogTime("answerbrowserlazy directive function","answ");
        return {
            restrict: 'E',
            scope: {
                taskId: '@'
            },
            
            controller: function ($scope) {
                timLogTime("answerbrowserlazy ctrl function","answ",1);
                $scope.compiled = false;
            },
            
            link: function ($scope, $element, $attrs) {
                timLogTime("answerbrowserlazy link function","answ",1);

                $element.parent().on('mouseenter touchstart', function () {
                    var plugin = $element.parents('.par').find('.parContent');
                    if ( $scope.compiled ) return;
                    $scope.compiled = true;
                    var newScope = $scope;
                    var newHtml = '<answerbrowser task-id="' + $scope.taskId + '"></answerbrowser>';
                    var newElement = $compile(newHtml);
                    var parent = $element.parents(".par")[0];
                    parent.replaceChild(newElement($scope.$parent)[0],$element[0]);
                    
                    // Next the inside of the plugin to non lazy
                    var origHtml = plugin[0].innerHTML;
                    if ( origHtml.indexOf(LAZYSTART) >= 0 ) {

                    } else plugin = null;
                    if ( plugin ) {
                        var newPluginHtml = makeNotLazy(origHtml);
                        var newPluginElement = $compile(newPluginHtml);
                        plugin.html(newPluginElement($scope));
                        $scope.$parent.processAllMathDelayed(plugin);
                        origHtml = null; // save some space
                    }
                });
            }
        };
    }]);


var GLOBALBrowseUser = null;    
    

timApp.directive("answerbrowser", ['$upload', '$http', '$sce', '$compile', '$window', '$filter',
    function ($upload, $http, $sce, $compile, $window, $filter) {
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

                $scope.loading = 0;
                $scope.changeAnswer = function () {
                    $scope.points = $scope.selectedAnswer.points;
                    var $par = $scope.element;
                    var par_id = $scope.$parent.getParId($par);
                    $scope.loading++;
                    $http.get('/getState', {
                        params: {
                            doc_id: $scope.$parent.docId,
                            par_id: par_id,
                            user_id: $scope.user.id,
                            state: $scope.selectedAnswer.content
                        }
                    }).success(function (data, status, headers, config) {
                        var newhtml = makeNotLazy(data.html);
                        var plugin = $par.find('.parContent');
                        plugin.html($compile(newhtml)($scope));
                        $scope.$parent.processAllMathDelayed(plugin);
                    }).error(function (data, status, headers, config) {
                        $window.alert('Error getting answers: ' + data.error);
                    }).finally(function () {
                        $scope.loading--;
                    });
                };

                $scope.next = function () {
                    var newIndex = $scope.filteredAnswers.indexOf($scope.selectedAnswer) - 1;
                    if (newIndex < 0) {
                        newIndex = $scope.filteredAnswers.length - 1;
                    }
                    $scope.selectedAnswer = $scope.filteredAnswers[newIndex];
                    $scope.changeAnswer();
                };

                $scope.previous = function () {
                    var newIndex = $scope.filteredAnswers.indexOf($scope.selectedAnswer) + 1;
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

                $scope.getTeacherData = function () {
                    if ($scope.answers.length > 0)
                        return {
                            answer_id: $scope.selectedAnswer.id,
                            saveTeacher: $scope.saveTeacher,
                            teacher: true,
                            points: $scope.points
                        };
                    else
                        return {
                            saveTeacher: false,
                            teacher: true
                        };
                };

                $scope.getAvailableUsers = function () {
                    $scope.loading++;
                    $http.get('/getTaskUsers/' + $scope.taskId, {params: {group: $scope.$parent.group}})
                        .success(function (data, status, headers, config) {
                            $scope.users = data;
                        }).error(function (data, status, headers, config) {
                            $window.alert('Error getting users: ' + data.error);
                        }).finally(function () {
                            $scope.loading--;
                        });
                };

                $scope.getAvailableAnswers = function (updateHtml) {
                    updateHtml = (typeof updateHtml === "undefined") ? true : updateHtml;
                    if (!$scope.$parent.rights.browse_own_answers) {
                        return;
                    }
                    if ($scope.user === null) {
                        return;
                    }
                    $scope.loading++;
                    $http.get('/answers/' + $scope.taskId + '/' + $scope.user.id + '?rnd='+Math.random())  
                        .success(function (data, status, headers, config) {
                            $scope.answers = data;
                            if ($scope.answers.length > 0) {
                                $scope.selectedAnswer = $scope.answers[0];
                                $scope.points = $scope.selectedAnswer.points;
                                if (updateHtml) {
                                    $scope.changeAnswer();
                                }
                            }
                        }).error(function (data, status, headers, config) {
                            $window.alert('Error getting answers: ' + data.error);
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

                $scope.$on('userChanged', function (event, args) {
                    $scope.user = args.user;
                    GLOBALBrowseUser = args.user;
                    $scope.changed = true;
                    $scope.firstLoad = false;
                    $scope.shouldUpdateHtml = true;
                });

                $scope.loadIfChanged = function () {
                    if ($scope.changed) {
                        $scope.getAvailableAnswers($scope.shouldUpdateHtml);
                        $scope.changed = false;
                        $scope.firstLoad = false;
                        $scope.shouldUpdateHtml = false;
                    }
                };

                
                $scope.checkUsers = function () {
                    $scope.loadIfChanged();
                    if ($scope.$parent.teacherMode && $scope.users === null) {
                        $scope.users = [];
                        if ($scope.$parent.users.length > 0) {
                            $scope.getAvailableUsers();
                        }
                    }
                };

                if ( GLOBALBrowseUser ) {
                    $scope.user = GLOBALBrowseUser;
                }    
                else if ($scope.$parent.users.length > 0) {
                    $scope.user = $scope.$parent.users[0];
                } else {
                    $scope.user = null;
                }

                $scope.changed = true;
                $scope.firstLoad = true;
                $scope.shouldUpdateHtml = false;
                $scope.saveTeacher = false;
                $scope.users = null;
                $scope.answers = [];
                $scope.onlyValid = true;
                $scope.selectedAnswer = null;

                $scope.$watchGroup(['onlyValid', 'answers'], function (newValues, oldValues, scope) {
                    $scope.filteredAnswers = $filter('filter')($scope.answers, function (value, index, array) {
                        if (value.valid) {
                            return true;
                        }
                        return !$scope.onlyValid;
                    });
                    if ($scope.filteredAnswers.indexOf($scope.selectedAnswer) < 0) {
                        $scope.setNewest();
                    }
                });

                $scope.checkUsers();
                $element.parent().on('mouseenter touchstart', function () {
                    $scope.checkUsers();
                });
            }
        };
    }]);
