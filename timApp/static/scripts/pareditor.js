var angular;
var timApp = angular.module('timApp');

timApp.directive("pareditor", ['$upload', '$http', '$sce', '$compile', '$window',
    function ($upload, $http, $sce, $compile, $window) {
        return {
            templateUrl: "/static/templates/parEditor.html",
            restrict: 'E',
            scope: {
                saveUrl: '@',
                deleteUrl: '@',
                previewUrl: '@',
                extraData: '=',
                afterSave: '&',
                afterCancel: '&',
                afterDelete: '&',
                options: '=',
                editorText: '@',
                initialTextUrl: '@'
            },
            controller: function ($scope) {
                $scope.aceChanged = function () {
                    $scope.outofdate = true;
                    if ($scope.timer) {
                        $window.clearTimeout($scope.timer);
                    }
                    $scope.timer = $window.setTimeout(function () {
                        var text = $scope.editor.getSession().getValue();
                        $http.post($scope.previewUrl, {
                            "text": text
                        }).success(function (data, status, headers, config) {
                            var len = data.texts.length;

                            var $previewDiv = angular.element(".previewcontent");
                            $previewDiv.html("");

                            for (var i = 0; i < len; i++) {
                                var html = data.texts[i].html;
                                if ('task_id' in data.texts[i]) {
                                    html = $compile(html)($scope);
                                }
                                $previewDiv.append(angular.element("<div>", {class: "par"})
                                    .append(angular.element("<div>", {class: "parContent"})
                                        .html(html)));
                            }
                            $scope.$parent.processAllMath($previewDiv);
                            $scope.outofdate = false;
                            $scope.parCount = len;
                        }).error(function (data, status, headers, config) {
                            $window.alert("Failed to show preview: " + data.error);
                        });
                    }, 500);
                };

                $scope.aceLoaded = function (editor) {
                    $scope.editor = editor;
                    editor.renderer.setPadding(10, 10, 10, 10);
                    editor.getSession().setMode("markdown");
                    editor.getSession().setUseWrapMode(false);
                    editor.getSession().setWrapLimitRange(0, 79);
                    editor.setOptions({maxLines: 40, minLines: 3});
                    if ($scope.initialTextUrl) {
                        editor.getSession().setValue('Loading text...');
                        $http.get($scope.initialTextUrl).
                            success(function (data, status, headers, config) {
                                $scope.editorText = data.text;
                            }).
                            error(function (data, status, headers, config) {
                                $window.alert('Failed to get text: ' + data.error);
                            });
                    }

                    var iOS = /(iPad|iPhone|iPod)/g.test($window.navigator.platform);

                    // iPad does not open the keyboard if not manually focused to editable area
                    if (!iOS) {
                        editor.focus();
                    }
                };
            },
            link: function ($scope, $element, $attrs) {
                $scope.timer = null;
                $scope.outofdate = false;
                $scope.parCount = 0;
                $scope.extraData.markRead = false;
                var snippetManager = ace.require("ace/snippets").snippetManager;

                var langTools = ace.require("ace/ext/language_tools");
                langTools.setCompleters([]);
                // $scope.editor.getSession().setMode("ace/mode/java");
                $scope.editor.setOptions({enableBasicAutocompletion: true});
                var pluginCompleter = {
                    getCompletions: function (editor, session, pos, prefix, callback) {
                        if (prefix.length === 0) {
                            callback(null, []);
                            return
                        }
                        $.getJSON(
                            "http://rhymebrain.com/talk?function=getRhymes&word=" + prefix,
                            function (wordList) {
                                // wordList like [{"word":"flow","freq":24,"score":300,"flags":"bc","syllables":"1"}]
                                callback(null, wordList.map(function (ea) {
                                    return {name: ea.word, value: ea.word , score: ea.score, meta: "plugin" }
                                }));
                            })
                    }
                };
                langTools.addCompleter(pluginCompleter);

                //var touchDevice = 'ontouchstart' in document.documentElement;
                //var touchDevice = (typeof window.ontouchstart !== 'undefined');
                var touchDevice = true;

                $scope.wrapFn = function (func) {
                    // console.log(touchDevice);
                    if (!touchDevice) $scope.editor.focus();
                    return (func());
                };

                $scope.saveClicked = function () {
                    var text = $scope.editor.getSession().getValue();
                    $http.post($scope.saveUrl, angular.extend({
                        text: text
                    }, $scope.extraData)).success(function (data, status, headers, config) {
                        $scope.afterSave({
                            extraData: $scope.extraData,
                            saveData: data
                        });
                        if ($scope.options.destroyAfterSave) {
                            $element.remove();
                        }
                    }).error(function (data, status, headers, config) {
                        $window.alert("Failed to save: " + data.error);
                    });
                };

                $scope.deleteClicked = function () {
                    if (!$window.confirm("Delete - are you sure?")) {
                        return;
                    }
                    $http.post($scope.deleteUrl, $scope.extraData).
                        success(function (data, status, headers, config) {
                            $scope.afterDelete({
                                extraData: $scope.extraData,
                                saveData: data
                            });
                            if ($scope.options.destroyAfterSave) {
                                $element.remove();
                            }
                        }).
                        error(function (data, status, headers, config) {
                            $window.alert("Failed to delete: " + data.error);
                        });
                };

                $scope.cancelClicked = function () {
                    $element.remove();
                    $scope.afterCancel({
                        extraData: $scope.extraData
                    });
                };

                $scope.releaseClicked = function () {
                    var div = $("#previewDiv");

                    if (div.css("position") == "absolute") {
                        div.css("position", "static");
                        div.find(".draghandle").css("visibility", "hidden");
                        document.getElementById("releaseButton").innerHTML = "&#8594;";
                    }
                    else {
                        div.css("position", "absolute");
                        div.find(".draghandle").css("visibility", "visible");
                        document.getElementById("releaseButton").innerHTML = "&#8592;";
                    }
                };

                //Navigation
                $scope.undoClicked = function () {
                    $scope.editor.undo();
                };

                $scope.redoClicked = function () {
                    $scope.editor.redo();
                };

                $scope.boldClicked = function () {
                    snippetManager.insertSnippet($scope.editor, "**${0:$SELECTION}**");
                };

                $scope.italicClicked = function () {
                    snippetManager.insertSnippet($scope.editor, "*${0:$SELECTION}*");
                };


                $scope.leftClicked = function () {
                    $scope.editor.navigateLeft(1);
                };

                $scope.rightClicked = function () {
                    $scope.editor.navigateRight(1);
                };

                $scope.upClicked = function () {
                    $scope.editor.navigateUp(1);
                };

                $scope.downClicked = function () {
                    $scope.editor.navigateDown(1);
                };
                //Navigation

                //Style
                $scope.linkClicked = function () {
                    var selectedText = "Linkin osoite";
                    if (!$scope.editor.getSelection().$isEmpty)
                        selectedText = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                    //console.log(selectedText.toLowerCase().indexOf("http://") === 0);
                    snippetManager.insertSnippet($scope.editor, "[${0:Linkin teksti}](" + selectedText + ")");
                    //snippetManager.insertSnippet($scope.editor, "[${0:Linkin teksti}]($SELECTION)");
                };

                $scope.imageClicked = function () {
                    var selectedText = "Kuvan osoite";
                    if (!$scope.editor.getSelection().$isEmpty)
                        selectedText = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                    //console.log(selectedText.toLowerCase().indexOf("http://") === 0);
                    snippetManager.insertSnippet($scope.editor, "![${0:Kuvan teksti}](" + selectedText + ")");
                    //snippetManager.insertSnippet($scope.editor, "[${0:Linkin teksti}]($SELECTION)");
                };

                $scope.codeClicked = function () {
                    snippetManager.insertSnippet($scope.editor, "```\n${0:$SELECTION}\n```");
                };

                $scope.indentClicked = function () {
                    $scope.editor.indent();
                };

                $scope.outdentClicked = function () {
                    $scope.editor.blockOutdent();
                };

                $scope.listClicked = function () {
                    $scope.editor.navigateLineStart();
                    snippetManager.insertSnippet($scope.editor, "- ${0:$SELECTION}");
                };

                $scope.ruleClicked = function () {
                    $scope.editor.navigateLineEnd();
                    snippetManager.insertSnippet($scope.editor, "\n---\n");
                };
                //Style

                //Special characters

                $scope.charClicked = function ($event) {
                    var character = $($event.target).text();
                    $scope.editor.insert(character);
                    if (!touchDevice) $scope.editor.focus();
                };

                //Special characters

                //TEX
                $scope.texClicked = function () {
                    snippetManager.insertSnippet($scope.editor, "$$${0:$SELECTION}$$");
                };

                $scope.indexClicked = function () {
                    snippetManager.insertSnippet($scope.editor, "_{${0:$SELECTION}}");
                };

                $scope.powerClicked = function () {
                    snippetManager.insertSnippet($scope.editor, "^{${0:$SELECTION}}");
                };

                $scope.squareClicked = function () {
                    snippetManager.insertSnippet($scope.editor, "\\sqrt{${0:$SELECTION}}");
                };

                $scope.rootClicked = function () {
                    snippetManager.insertSnippet($scope.editor, "\\sqrt[$0]{$SELECTION}");
                };
                //TEX

                //Plugins

                $scope.pluginClicked = function (plugin, template) {
                    // $scope.editor.setReadOnly(!$scope.editor.getReadOnly());
                    //TODO
                    $.ajax({
                        type: 'GET',
                        url: '/' + plugin + '/template/' + template,
                        success: function (data) {
                            snippetManager.insertSnippet($scope.editor, data);
                        },
                        error: function () {
                            console.log("Virhe");
                        }
                    })
                }

                $scope.insertText

                //Plugins

                $scope.onFileSelect = function (url, $files) {
                    //$files: an array of files selected, each file has name, size, and type.
                    for (var i = 0; i < $files.length; i++) {
                        var file = $files[i];
                        $scope.upload = $upload.upload({
                            url: url,
                            method: 'POST',
                            file: file
                        }).progress(function (evt) {
                            $scope.progress = 'Uploading... ' + parseInt(100.0 * evt.loaded / evt.total) + '%';
                        }).success(function (data, status, headers, config) {
                            $scope.uploadedFile = '/images/' + data.file;
                            $scope.progress = 'Uploading... Done!';
                            $scope.editor.insert("![Image](" + $scope.uploadedFile + ")");
                        }).error(function (data, status, headers, config) {
                            $scope.progress = 'Error while uploading: ' + data.error;
                        });
                    }
                };

                $scope.ribbonClicked = function ($event, area) {
                    var naviArea = $($event.target).parent().find("#" + area);
                    var buttons = $($event.target).parent().find(".extraButtonArea");
                    for (var i = 0; i < buttons.length; i++) {
                        $(buttons[i]).attr("class", 'extraButtonArea hidden');
                    }
                    $(naviArea).attr("class", 'extraButtonArea');
                    if (!touchDevice) $scope.editor.focus();
                };
            }
        };
    }]);
