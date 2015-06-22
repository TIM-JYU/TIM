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
                    var max = 50;
                    if ('ontouchstart' in window || navigator.msMaxTouchPoints) {
                        var line = editor.renderer.lineHeight;
                        var height = $(window).height();
                        max = Math.floor((height / 2) / line);
                    }

                    editor.renderer.setPadding(10, 10, 10, 10);
                    editor.getSession().setMode("markdown");
                    editor.getSession().setUseWrapMode(false);
                    editor.getSession().setWrapLimitRange(0, 79);
                    editor.setOptions({
                        maxLines: max,
                        minLines: 5,
                        autoScrollEditorIntoView: true,
                        vScrollBarAlwaysVisible: true
                    });

                    editor.commands.addCommand({
                        name: 'saveFile',
                        bindKey: {
                            win: 'Ctrl-S',
                            mac: 'Command-S',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.saveClicked();
                        }
                    });

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

                $(document).on('webkitfullscreenchange mozfullscreenchange fullscreenchange MSFullscreenChange', function (event) {
                    var editor = document.getElementById("pareditor");
                    if (!document.fullscreenElement &&    // alternative standard method
                        !document.mozFullScreenElement && !document.webkitFullscreenElement && !document.msFullscreenElement) {
                        editor.removeAttribute('style');
                    }
                });

                $scope.timer = null;
                $scope.outofdate = false;
                $scope.parCount = 0;
                var snippetManager = ace.require("ace/snippets").snippetManager;

                var langTools = ace.require("ace/ext/language_tools");
                langTools.setCompleters([]);
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
                                    return {name: ea.word, value: ea.word, score: ea.score, meta: "plugin"}
                                }));
                            })
                    }
                };
                langTools.addCompleter(pluginCompleter);

                var touchDevice = false;

                $scope.wrapFn = function (func) {
                    if (!touchDevice) $scope.editor.focus();
                    if (func) return (func());
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

                $scope.surroundClicked = function (str, func) {
                    if (($scope.editor.session.getTextRange($scope.editor.getSelectionRange()) === "")) {
                        $scope.selectWord();
                    }
                    var text = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                    var surrounded = false;
                    if (func) {
                        surrounded = func();
                    }
                    else {
                        surrounded = $scope.surroundedBy(str);
                    }
                    if (surrounded) {
                        var range = $scope.editor.getSelectionRange();
                        range.start.column -= str.length;
                        range.end.column += str.length;
                        $scope.editor.selection.setRange(range);
                        snippetManager.insertSnippet($scope.editor, "${0:" + text + "}");
                    } else {
                        snippetManager.insertSnippet($scope.editor, str + "${0:$SELECTION}" + str);
                    }
                };

                $scope.selectWord = function () {
                    var cursor = $scope.editor.getCursorPosition();
                    var wordrange = $scope.editor.getSession().getAWordRange(cursor.row, cursor.column);
                    var word = ($scope.editor.session.getTextRange(wordrange));
                    if (/^\s*$/.test(word)) return;
                    var wordtrim = word.trim();
                    var difference = word.length - wordtrim.length;
                    wordrange.end.column -= difference;
                    $scope.editor.selection.setRange(wordrange);
                }

                $scope.surroundedBy = function (string) {
                    var range = $scope.editor.getSelectionRange();
                    range.start.column -= string.length;
                    range.end.column += string.length;
                    var word = ($scope.editor.session.getTextRange(range));
                    return (word.indexOf(string) === 0 && word.lastIndexOf(string) === (word.length - string.length));
                };

                $scope.surroundedByItalic = function () {
                    return (($scope.surroundedBy('*') && !$scope.surroundedBy('**')) || $scope.surroundedBy('***'));
                };

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

                $scope.codeBlockClicked = function () {
                    snippetManager.insertSnippet($scope.editor, "```\n${0:$SELECTION}\n```");
                };

                $scope.indentClicked = function () {
                    $scope.editor.indent();
                };

                $scope.outdentClicked = function () {
                    $scope.editor.blockOutdent();
                };

                $scope.listClicked = function () {
                    snippetManager.insertSnippet($scope.editor, "- ${0:$SELECTION}");
                };

                $scope.ruleClicked = function () {
                    $scope.editor.navigateLineEnd();
                    snippetManager.insertSnippet($scope.editor, "\n---\n");
                };

                $scope.headerClicked = function (head) {
                    var cursor = $scope.editor.getCursorPosition();
                    var line = $scope.editor.session.getLine(cursor.row);
                    var range = $scope.editor.getSelection().getRange();
                    range.start.column = 0;
                    range.end.column = line.length;
                    while (line.charAt(0) === '#')
                        line = line.substr(1);
                    line = line.trim();
                    $scope.editor.selection.setRange(range);
                    $scope.editor.insert(head + ' ' + line);
                    if (!touchDevice) $scope.editor.focus();
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
                    snippetManager.insertSnippet($scope.editor, "$${0:$SELECTION}$");
                };

                $scope.texBlockClicked = function () {
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
                    if (!touchDevice) $scope.editor.focus();
                }

                //Plugins

                $scope.onFileSelect = function (url, $files) {
                    if (!touchDevice) $scope.editor.focus();
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
                            if (data.image) {
                                $scope.uploadedFile = '/images/' + data.image;
                                $scope.progress = 'Uploading... Done!';
                                $scope.editor.insert("![Image](" + $scope.uploadedFile + ")");
                            } else {
                                $scope.uploadedFile = '/files/' + data.file;
                                $scope.progress = 'Uploading... Done!';
                                $scope.editor.insert("[File](" + $scope.uploadedFile + ")");
                            }
                        }).error(function (data, status, headers, config) {
                            $scope.progress = 'Error while uploading: ' + data.error;
                        });
                    }
                };

                $scope.tabClicked = function ($event, area) {
                    var naviArea = $('#' + area);
                    var buttons = $('.extraButtonArea');
                    var tabs = $('.tab');
                    for (var i = 0; i < buttons.length; i++) {
                        $(buttons[i]).attr("class", 'extraButtonArea hidden');
                    }
                    ;
                    for (var i = 0; i < tabs.length; i++) {
                        $(tabs[i]).removeClass('active');
                    }
                    var active = $($event.target).parent();
                    $(active).attr('class', 'tab active');
                    $(naviArea).attr('class', 'extraButtonArea');
                    if (!touchDevice) $scope.editor.focus();
                };


                $scope.goFullScreen = function () {
                    var div = document.getElementById("pareditor");
                    if (!document.fullscreenElement &&    // alternative standard method
                        !document.mozFullScreenElement && !document.webkitFullscreenElement && !document.msFullscreenElement) {
                        div.setAttribute("style", "width: 100%; height: 100%; position: absolute; top: 0px;" +
                            "padding: 2em 5px 5px 5px; background: rgb(224, 224, 224); -webkit-box-sizing: border-box;" +
                            "-moz-box-sizing: border-box; box-sizing: border-box;");

                        var requestMethod = div.requestFullScreen ||
                            div.webkitRequestFullscreen ||
                            div.webkitRequestFullScreen ||
                            div.mozRequestFullScreen ||
                            div.msRequestFullscreen;

                        if (requestMethod) {
                            requestMethod.apply(div);
                        }
                    } else {
                        if (document.exitFullscreen) {
                            document.exitFullscreen();
                        } else if (document.msExitFullscreen) {
                            document.msExitFullscreen();
                        } else if (document.mozCancelFullScreen) {
                            document.mozCancelFullScreen();
                        } else if (document.webkitExitFullscreen) {
                            document.webkitExitFullscreen();
                        }
                    }
                };
            }
        };
    }]);
