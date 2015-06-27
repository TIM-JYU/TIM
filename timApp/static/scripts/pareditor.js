var angular;
var MENU_BUTTON_CLASS = 'menuButtons';
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
                $scope.editor = CodeMirror.fromTextArea(document.getElementById("codemirror"), {
                    lineNumbers: true,
                    mode: {name: "javascript", globalVars: true},
                    indentUnit: 4
                });

                if ($scope.initialTextUrl) {
                    $scope.editor.setValue('Loading text...');
                    $http.get($scope.initialTextUrl, {
                        params: {"_": Date.now()}
                    }).success(function (data, status, headers, config) {
                        $scope.editor.setValue(data.text);
                    }).error(function (data, status, headers, config) {
                        $window.alert('Failed to get text: ' + data.error);
                    });
                }

                $scope.editor.on('change', function () {
                    $scope.outofdate = true;
                    if ($scope.timer) {
                        $window.clearTimeout($scope.timer);
                    }
                    $scope.timer = $window.setTimeout(function () {
                        var text = $scope.editor.getValue();
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

                });

                $scope.aceLoaded = function (editor) {
                    $scope.editor = editor;
                    var max = 50;
                    if ('ontouchstart' in window || navigator.msMaxTouchPoints) {
                        var line = editor.renderer.lineHeight;
                        var height = $(window).height();
                        max = Math.floor((height / 2) / line) - 4;
                    }

                    editor.$blockScrolling = Infinity;
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
                        $http.get($scope.initialTextUrl, {
                            params: {"_": Date.now()}
                        }).success(function (data, status, headers, config) {
                            $scope.editorText = data.text;
                        }).error(function (data, status, headers, config) {
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

                $scope.tables = {};

                $scope.tables['normal'] = "Otsikko1 Otsikko2 Otsikko3 Otsikko4\n" +
                    "-------- -------- -------- --------\n" +
                    "1.rivi   x        x        x       \n" +
                    "2.rivi   x        x        x       ";

                $scope.tables['example'] = "Table:  Otsikko taulukolle\n\n" +
                    "Otsikko    Vasen laita    Keskitetty    Oikea laita\n" +
                    "---------- ------------- ------------ -------------\n" +
                    "1. rivi      2                  3         4\n" +
                    "2. rivi        1000      2000             30000";

                $scope.tables['noheaders'] = ":  Otsikko taulukolle\n\n" +
                    "---------- ------------- ------------ -------------\n" +
                    "1. rivi      2                  3         4\n" +
                    "2. rivi        1000      2000             30000\n" +
                    "---------- ------------- ------------ -------------\n";


                $scope.tables['multiline'] = "Table:  Otsikko taulukolle voi\n" +
                    "jakaantua usealle riville\n\n" +
                    "-----------------------------------------------------\n" +
                    "Ekan       Toisen\         kolmas\            neljäs\\\n" +
                    "sarkkeen   sarakkeen\     keskitettynä      oikeassa\\\n" +
                    "otsikko    otsikko                           reunassa\n" +
                    "---------- ------------- -------------- -------------\n" +
                    "1. rivi     toki\              3         4\n" +
                    "voi olla    sisältökin\n" +
                    "useita        voi\\\n" +
                    "rivejä      olla \n" +
                    "            monella\\\n" +
                    "            rivillä\n" +
                    "            \n" +
                    "2. rivi        1000      2000             30000\n" +
                    "-----------------------------------------------------\n";

                $scope.tables['strokes'] = ": Viivoilla tehty taulukko\n\n" +
                    "+---------------+---------------+----------------------+\n" +
                    "| Hedelmä       | Hinta         | Edut                 |\n" +
                    "+===============+===============+======================+\n" +
                    "| Banaani       |  1.34 €       | - valmis kääre       |\n" +
                    "|               |               | - kirkas väri        |\n" +
                    "+---------------+---------------+----------------------+\n" +
                    "| Appelsiini    |  2.10 €       | - auttaa keripukkiin |\n" +
                    "|               |               | - makea              |\n" +
                    "+---------------+---------------+----------------------+\n";


                $(document).on('webkitfullscreenchange mozfullscreenchange fullscreenchange MSFullscreenChange', function (event) {
                    var editor = $($element).find("#pareditor").get(0);
                    if (!document.fullscreenElement &&    // alternative standard method
                        !document.mozFullScreenElement && !document.webkitFullscreenElement && !document.msFullscreenElement) {
                        editor.removeAttribute('style');
                    }
                });

                $scope.timer = null;
                $scope.outofdate = false;
                $scope.parCount = 0;
                $scope.snippetManager = ace.require("ace/snippets").snippetManager;

                /*
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
                 //TODO: tähän json tiedoston osoite, josta halutaan täydennyksiä
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
                 */

                var touchDevice = false;

                $scope.wrapFn = function (func) {
                    if (!touchDevice) $scope.editor.focus();
                    if (typeof(func) !== 'undefined') return (func());
                };

                $scope.saveClicked = function () {
                    var text = $scope.editor.getValue();
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
                    $scope.editor.execCommand('goCharLeft');
                };

                $scope.rightClicked = function () {
                    $scope.editor.execCommand('goCharRight');
                };

                $scope.upClicked = function () {
                    $scope.editor.execCommand('goLineUp');
                };

                $scope.downClicked = function () {
                    $scope.editor.execCommand('goLineDown');
                };

                $scope.homeClicked = function () {
                    $scope.editor.execCommand('goLineStart');
                };

                $scope.endClicked = function () {
                    $scope.editor.execCommand('goLineEnd');
                };

                $scope.topClicked = function () {
                   $scope.editor.execCommand('goDocStart');
                };

                $scope.bottomClicked = function () {
                    $scope.editor.execCommand('goDocEnd');
                };

                $scope.insertClicked = function () {
                    $scope.editor.toggleOverwrite();
                };
                //Navigation
                //Style
                $scope.indentClicked = function () {
                    $scope.editor.execCommand('indentMore');
                };

                $scope.outdentClicked = function () {
                    $scope.editor.execCommand('indentLess');
                };

                $scope.surroundClicked = function (str, func) {
                    if ($scope.editor.getSelection() == "") {
                        $scope.selectWord();
                    }
                    var text = $scope.editor.getSelection();
                    var surrounded = (func) ? func() : $scope.surroundedBy(str);
                    if (surrounded) {
                        var from = $scope.editor.getCursor('from');
                        var to = $scope.editor.getCursor('to');
                        from.ch -= str.length;
                        to.ch += str.length;
                        $scope.editor.setSelection(from, to);
                        $scope.editor.replaceSelection(text);
                    } else {
                        $scope.editor.replaceSelection(str + text + str);
                    }
                };

                $scope.selectWord = function () {
                    var A1 = $scope.editor.getCursor().line;
                    var A2 = $scope.editor.getCursor().ch;
                    var B1 = $scope.editor.findWordAt({line: A1, ch: A2}).anchor.ch;
                    var B2 = $scope.editor.findWordAt({line: A1, ch: A2}).head.ch;
                    var word = $scope.editor.getRange({line: A1, ch: B1}, {line: A1, ch: B2});
                    if (/^\s*$/.test(word)) return false;
                    $scope.editor.setSelection({line: A1, ch: B1}, {line: A1, ch: B2});
                    return true;
                };

                $scope.surroundedBy = function (string) {
                    var from = $scope.editor.getCursor('from');
                    var to = $scope.editor.getCursor('to');
                    var word = $scope.editor.getRange({line: from.line, ch: from.ch - string.length},
                        {line: to.line, ch: to.ch + string.length});
                    return (word.indexOf(string) === 0 && word.lastIndexOf(string) === (word.length - string.length));
                };

                $scope.surroundedByItalic = function () {
                    return (($scope.surroundedBy('*') && !$scope.surroundedBy('**')) || $scope.surroundedBy('***'));
                };

                $scope.codeBlockClicked = function () {
                    var text = $scope.editor.getSelection();
                    $scope.editor.replaceSelection( "```\n" + text + "\n```");
                };

                $scope.headerClicked = function (head) {
                    var cursor = $scope.editor.getCursor();
                    var line = $scope.editor.getLine(cursor.line);
                    $scope.editor.setSelection({line: cursor.line, ch: 0}, {line: cursor.line, ch: line.length});
                    while (line.charAt(0) === '#')
                        line = line.substr(1);
                    line = line.trim();
                    $scope.editor.replaceSelection(head + ' ' + line);
                    if (!touchDevice) $scope.editor.focus();
                };

                //Style
                //Insert
                /**
                 * @param descDefault Placeholder for description
                 * @param linkDefault Placeholder for link address
                 * @param isImage true, if link is an image
                 */
                $scope.linkClicked = function (descDefault, linkDefault, isImage) {
                    var image = (isImage) ? '!' : '';
                    if ($scope.editor.getSelection() === "") {
                        if ($scope.selectWord())
                            descDefault = $scope.editor.getSelection();
                    } else
                        descDefault = $scope.editor.getSelection();
                    $scope.editor.replaceSelection(image + "[" + descDefault + "](" + linkDefault + ")");
                };

                $scope.listClicked = function () {
                    $scope.editor.replaceSelection("- " + $scope.editor.getSelection());
                };

                $scope.insertTemplate = function (text) {
                    $scope.closeMenu(null, close);
                    $scope.editor.replaceSelection(text);
                };

                $scope.closeMenu = function (e, force) {
                    var container = $("." + MENU_BUTTON_CLASS);
                    if (force || (!container.is(e.target) && container.has(e.target).length === 0)) {
                        container.remove();
                        $(document).off("mouseup.closemenu");
                    }
                };

                $scope.tableClicked = function ($event) {
                    $scope.closeMenu(null, true);
                    var $button = $($event.target);
                    var coords = {left: $button.position().left, top: $button.position().top};
                    var button_width = 130;
                    var $actionDiv = $("<div>", {class: MENU_BUTTON_CLASS});

                    var createButtonSpan = function (text, clickfunction) {
                        var $span = $("<span>", {class: 'actionButtonRow'});
                        $span.append($("<button>", {
                            class: 'timButton',
                            text: text,
                            'ng-click': clickfunction,
                            width: button_width
                        }));
                        return $span;
                    }

                    for (var key in $scope.tables) {
                        var text = key.charAt(0).toUpperCase() + key.substring(1);
                        var clickfn = 'insertTemplate(tables[\'' + key + '\']); wrapFn()';
                        $actionDiv.append(createButtonSpan(text, clickfn));
                    }
                    $actionDiv.append(createButtonSpan('Close menu', 'closeMenu(null, true); wrapFn()'));
                    $actionDiv.offset(coords);
                    $actionDiv.css('position', 'absolute'); // IE needs this
                    $actionDiv = $compile($actionDiv)($scope);
                    $button.parent().prepend($actionDiv);

                    $(document).on('mouseup.closemenu', $scope.closeMenu);
                };

                $scope.ruleClicked = function () {
                    $scope.editor.navigateLineEnd();
                    $scope.snippetManager.insertSnippet($scope.editor, "\n---\n");
                };
                //Insert
                //Special characters
                $scope.charClicked = function ($event) {
                    var character = $($event.target).text();
                    $scope.editor.insert(character);
                    if (!touchDevice) $scope.editor.focus();
                };
                //Special characters
                //TEX
                $scope.texClicked = function () {
                    $scope.snippetManager.insertSnippet($scope.editor, "$${0:$SELECTION}$");
                };

                $scope.texBlockClicked = function () {
                    $scope.snippetManager.insertSnippet($scope.editor, "$$${0:$SELECTION}$$");
                };

                $scope.indexClicked = function () {
                    $scope.snippetManager.insertSnippet($scope.editor, "_{${0:$SELECTION}}");
                };

                $scope.powerClicked = function () {
                    $scope.snippetManager.insertSnippet($scope.editor, "^{${0:$SELECTION}}");
                };

                $scope.squareClicked = function () {
                    $scope.snippetManager.insertSnippet($scope.editor, "\\sqrt{${0:$SELECTION}}");
                };

                $scope.rootClicked = function () {
                    $scope.snippetManager.insertSnippet($scope.editor, "\\sqrt[$0]{$SELECTION}");
                };
                //TEX
                //Plugins
                $scope.pluginClicked = function (plugin, template) {
                    $.ajax({
                        type: 'GET',
                        url: '/' + plugin + '/template/' + template,
                        success: function (data) {
                            $scope.editor.replaceSelection(data);
                        },
                        error: function () {
                            console.log("Virhe");
                        }
                    });
                    if (!touchDevice) $scope.editor.focus();
                };
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
                                $scope.editor.replaceSelection("![Image](" + $scope.uploadedFile + ")");
                            } else {
                                $scope.uploadedFile = '/files/' + data.file;
                                $scope.progress = 'Uploading... Done!';
                                $scope.editor.replaceSelection("![Image](" + $scope.uploadedFile + ")");
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
                    for (var i = 0; i < tabs.length; i++) {
                        $(tabs[i]).removeClass('active');
                    }
                    var active = $($event.target).parent();
                    $(active).attr('class', 'tab active');
                    $(naviArea).attr('class', 'extraButtonArea');
                    if (!touchDevice) $scope.editor.focus();
                };

                $scope.fullscreenSupported = function () {
                    var div = $($element).get(0);
                    var requestMethod = div.requestFullScreen ||
                        div.webkitRequestFullscreen ||
                        div.webkitRequestFullScreen ||
                        div.mozRequestFullScreen ||
                        div.msRequestFullscreen;
                    return (typeof(requestMethod) !== 'undefined');
                };


                $scope.goFullScreen = function () {
                    var div = $($element).find("#pareditor").get(0);
                    if (!document.fullscreenElement &&    // alternative standard method
                        !document.mozFullScreenElement && !document.webkitFullscreenElement && !document.msFullscreenElement) {

                        var requestMethod = div.requestFullScreen ||
                            div.webkitRequestFullscreen ||
                            div.webkitRequestFullScreen ||
                            div.mozRequestFullScreen ||
                            div.msRequestFullscreen;

                        if (requestMethod) {
                            requestMethod.apply(div);
                            div.setAttribute("style", "width: 100%; height: 100%; position: absolute; top: 0px;" +
                                "padding: 2em 5px 5px 5px; background: rgb(224, 224, 224); -webkit-box-sizing: border-box;" +
                                "-moz-box-sizing: border-box; box-sizing: border-box;");
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

                $scope.editor.setOption("extraKeys", {
                    "Ctrl-S": $scope.saveClicked,
                    "Cmd-S": $scope.saveClicked
                });

                var viewport = {};
                viewport.top = $(window).scrollTop();
                viewport.bottom = viewport.top + $(window).height();
                var bounds = {};
                bounds.top = $element.offset().top;
                bounds.bottom = bounds.top + $element.outerHeight();
                if (bounds.bottom > viewport.bottom || bounds.top < viewport.top) {
                    $('html, body').animate({
                        scrollTop: $element.offset().top
                    }, 2000);
                }
            }
        }
            ;
    }])
;