var angular;
var MENU_BUTTON_CLASS = 'menuButtons';
var timApp = angular.module('timApp');
var plugins = ['csPlugin', 'taunoPlugin', 'csPluginRikki', 'showCode', 'showImage', 'showVideo'];

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

                var $plugintab = $('#pluginButtons');

                function getData(plugin) {
                    $.ajax({
                        cache: false,
                        dataType: "json",
                        type: 'GET',
                        url: '/' + plugin + '/reqs/',
                        success: function (data, status, headers, config) {
                            if (data.templates) {
                                var tabs = data.text || [plugin];
                                for (var i = 0; i < tabs.length; i++) {
                                    var clickfunction = 'pluginClicked($event, \'' + plugin + '\',\'' + i + '\')';
                                    var button = $("<button>", {
                                        class: 'editorButton',
                                        text: tabs[i],
                                        title: tabs[i],
                                        'ng-click': clickfunction
                                    });
                                    button = $compile(button)($scope);
                                    $plugintab.append(button);
                                    $compile($plugintab)($scope);
                                }
                            }
                        },
                        error: function () {
                            console.log("Virhe");
                        }
                    });
                }

                function getPluginsInOrder() {
                    if (plugins.length === 0) {
                        return;
                    }
                    var plugin = plugins.pop();
                    console.log(plugin);
                    var delay = Math.random() * 3;

                    $.ajax({
                        cache: false,
                        dataType: "json",
                        type: 'GET',
                        url: '/' + plugin + '/reqs/',
                        success: function (data, status, headers, config) {
                            if (data.templates) {
                                var tabs = data.text || [plugin];
                                for (var i = 0; i < tabs.length; i++) {
                                    var clickfunction = 'pluginClicked($event, \'' + plugin + '\',\'' + i + '\')';
                                    var button = $("<button>", {
                                        class: 'editorButton',
                                        text: tabs[i],
                                        title: tabs[i],
                                        'ng-click': clickfunction
                                    });
                                    button = $compile(button)($scope);
                                    $plugintab.append(button);
                                    //$compile($plugintab)($scope);
                                }
                            }
                            getPluginsInOrder();
                        },
                        error: function () {
                            console.log("Virhe");
                            getPluginsInOrder();
                        }
                    });
                }

                getPluginsInOrder();
                //$compile('#pluginButtons')($scope);

                //for (var i = 0; i < plugins.length; i++) {
                //    getData(plugins[i]);
                //}

                if ((navigator.userAgent.match(/Trident/i))) {
                    $scope.isIE = true;
                }

                $scope.getEditorText = function () {
                    return "";
                };

                $scope.setEditorText = function () {
                    return;
                };

                $scope.setInitialText = function () {
                    $scope.setEditorText('Loading text...');
                    $http.get($scope.initialTextUrl, {
                        params: {"_": Date.now()}
                    }).success(function (data, status, headers, config) {
                        $scope.setEditorText(data.text);
                        $scope.aceChanged();
                    }).error(function (data, status, headers, config) {
                        $window.alert('Failed to get text: ' + data.error);
                    });
                };

                $scope.adjustPreview = function () {
                    window.setTimeout(function () {
                            var height = parseInt($('pareditor').css('max-height')) - 15;
                            var $preview = $('.previewcontent');
                            var offset = $($preview).position().top;
                            $($preview).css('max-height', (height - offset) + 'px');
                        }, 50
                    )
                };


                $('.editorContainer').on('resize', $scope.adjustPreview);

                $scope.aceChanged = function () {
                    $scope.outofdate = true;
                    if ($scope.timer) {
                        $window.clearTimeout($scope.timer);
                    }
                    $scope.timer = $window.setTimeout(function () {
                        var text = $scope.getEditorText();
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
                    $('.editorContainer').resize();
                };

                if ($scope.options.touchDevice) {
                    $scope.getEditorText = function () {
                        return $scope.editor.val();
                    };

                    $scope.setEditorText = function (text) {
                        $scope.editor.val(text);
                    };
                    var $container = ('.editorContainer');
                    var $textarea = $.parseHTML('<textarea rows="10" ng-model="editorText" ng-change="aceChanged()" ng-trim="false" id="teksti"></textarea>');
                    $compile($textarea)($scope);
                    $($container).append($textarea);

                    $scope.editor = $('#teksti');

                    if (!$scope.options.metaset) {
                        var $meta = $("meta[name='viewport']");
                        $scope.oldmeta = $meta[0];
                        $meta.remove();
                        $('head').append('<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=0">');
                    }

                    $scope.options.metaset = true;

                    $scope.editor.keydown(function (e) {
                        if (e.keyCode === 9) {
                            var outdent = e.shiftKey;
                            $scope.indent(outdent);
                            e.preventDefault();
                        } else if (e.keyCode === 83 && e.ctrlKey) {
                            $scope.saveClicked();
                            e.preventDefault();
                        }
                    });

                    if ($scope.initialTextUrl) $scope.setInitialText();

                } else {
                    $scope.getEditorText = function () {
                        return $scope.editor.getSession().getValue();
                    };

                    $scope.setEditorText = function (text) {
                        $scope.editor.getSession().setValue(text);
                    };

                    $scope.aceLoaded = function (editor) {
                        $scope.editor = editor;

                        var max = 50;
                        var line = editor.renderer.lineHeight;
                        var containertop = $('.editorContainer').position().top;
                        var height = $(window).innerHeight() - containertop;
                        max = Math.floor((height / 2) / line);

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

                        if ($scope.initialTextUrl) $scope.setInitialText();

                        /* iPad does not open the keyboard if not manually focused to editable area
                         var iOS = /(iPad|iPhone|iPod)/g.test($window.navigator.platform);
                         if (!iOS) editor.focus();
                         */
                    };
                }
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
                var touchDevice = false;

                $scope.wrapFn = function (func) {
                    if (!touchDevice) $scope.editor.focus();
                    if (typeof(func) !== 'undefined') (func());
                    if ($scope.isIE) $scope.aceChanged();
                };

                $scope.changeMeta = function () {
                    $("meta[name='viewport']").remove();
                    var $meta = $($scope.oldmeta);
                    $('head').append($meta);
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
                    if ($scope.options.touchDevide) $scope.changeMeta();
                };

                $scope.cancelClicked = function () {
                    if ($scope.options.touchDevide) $scope.changeMeta();
                    $element.remove();
                    $scope.afterCancel({
                        extraData: $scope.extraData
                    });
                };

                $scope.releaseClicked = function () {
                    var div = $("#previewDiv");
                    var content = $('.previewcontent');
                    var editor = $('.editorArea');

                    if (div.css("position") == "absolute") {
                        div.css("position", "static");
                        div.find(".draghandle").css("visibility", "hidden");
                        editor.css('overflow', '');
                        document.getElementById("releaseButton").innerHTML = "&#8594;";
                        $scope.adjustPreview();
                    }
                    else {
                        div.css("position", "absolute");
                        editor.css('overflow', 'visible');
                        div.find(".draghandle").css("visibility", "visible");
                        var height = window.innerHeight - 90;
                        content.css('max-height', height);
                        document.getElementById("releaseButton").innerHTML = "&#8592;";
                    }
                };

                $scope.saveClicked = function () {
                    var text = $scope.getEditorText();
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
                    if ($scope.options.touchDevide) $scope.changeMeta();
                };

                if ($scope.options.touchDevice) {

                    $scope.wrapFn = function (func) {
                        if (!touchDevice) $scope.editor.focus();
                        if (typeof(func) !== 'undefined') (func());
                        $scope.aceChanged();
                    };

                    //Navigation
                    $scope.undoClicked = function () {
                        document.execCommand("undo", false, null);
                    };

                    $scope.redoClicked = function () {
                        document.execCommand("redo", false, null);
                    };

                    $scope.scrollToCaret = function () {
                        var editor = $scope.editor.get(0);
                        var text = $scope.editor.val();
                        var lineHeight = parseInt($scope.editor.css('line-height'));
                        var height = $scope.editor.height();
                        var currentLine = text.substr(0, editor.selectionStart).split("\n").length;
                        var currentScroll = $scope.editor.scrollTop();
                        var currentLineY = currentLine * lineHeight;
                        if (currentLineY > currentScroll + height) {
                            $scope.editor.scrollTop(currentLineY - height);
                        } else if ((currentLineY - lineHeight) < currentScroll) {
                            $scope.editor.scrollTop(currentLineY - lineHeight);
                        }
                    };

                    $scope.leftClicked = function () {
                        var editor = $scope.editor.get(0);
                        editor.selectionStart = editor.selectionEnd -= 1;
                        $scope.scrollToCaret();
                    };

                    $scope.rightClicked = function () {
                        var editor = $scope.editor.get(0);
                        editor.selectionStart = editor.selectionEnd += 1;
                        $scope.scrollToCaret();
                    };

                    $scope.upClicked = function () {
                        var editor = $scope.editor.get(0);
                        var pos = editor.selectionEnd,
                            prevLine = editor.value.lastIndexOf('\n', pos),
                            TwoBLine = editor.value.lastIndexOf('\n', prevLine - 1);
                        if (prevLine === -1) return;
                        pos = pos - prevLine;
                        editor.selectionStart = editor.selectionEnd = TwoBLine + pos;
                        $scope.scrollToCaret();
                    };

                    $scope.downClicked = function () {
                        var editor = $scope.editor.get(0);
                        var pos = editor.selectionEnd,
                            prevLine = editor.value.lastIndexOf('\n', pos),
                            nextLine = editor.value.indexOf('\n', pos + 1);
                        if (nextLine === -1) return;
                        pos = pos - prevLine;
                        editor.selectionStart = editor.selectionEnd = nextLine + pos;
                        $scope.scrollToCaret();
                    };

                    $scope.homeClicked = function () {
                        var editor = $scope.editor.get(0);
                        editor.selectionEnd = editor.selectionStart =
                            editor.value.lastIndexOf('\n', editor.selectionEnd - 1) + 1;
                        $scope.scrollToCaret();
                    };

                    $scope.endClicked = function () {
                        var editor = $scope.editor.get(0);
                        var pos = editor.selectionEnd,
                            i = editor.value.indexOf('\n', pos);
                        if (i === -1) i = editor.value.length;
                        editor.selectionStart = editor.selectionEnd = i;
                        $scope.scrollToCaret();
                    };

                    $scope.topClicked = function () {
                        var editor = $scope.editor.get(0);
                        editor.selectionStart = editor.selectionEnd = 0;
                        $scope.editor.scrollTop(0);
                    };

                    $scope.bottomClicked = function () {
                        var editor = $scope.editor.get(0);
                        editor.selectionStart = editor.selectionEnd = editor.value.length;
                        $scope.editor.scrollTop($scope.editor.prop('scrollHeight'));
                    };

                    $scope.insertClicked = function () {
                        var input = document.getElementById('teksti');
                        input.addEventListener('keypress', function (event) {
                            var s = this.selectionStart;
                            this.value = this.value.substr(0, s) + this.value.substr(s + 1);
                            this.selectionEnd = s;
                        }, false);
                    };
                    //Navigation
                    //Style
                    $scope.indentClicked = function () {
                        $scope.indent();
                    };

                    $scope.outdentClicked = function () {
                        $scope.indent(true);
                    };

                    $scope.indent = function (outdent) {
                        var tab = "    ";
                        var tablength = tab.length;
                        var selection = $scope.editor.getSelection();
                        var pos = selection.start;
                        var value = $scope.editor.val();

                        if (selection.text != "") {
                            var tempStart = selection.start;
                            while (tempStart--) {
                                if (value.charAt(tempStart) == "\n") {
                                    selection.start = tempStart + 1;
                                    break;
                                }
                            }

                            var toIndent = value.substring(selection.start, selection.end),
                                lines = toIndent.split("\n"),
                                i;

                            if (outdent) {
                                for (i = 0; i < lines.length; i++) {
                                    if (lines[i].substring(0, tablength) == tab) {
                                        lines[i] = lines[i].substring(tablength);
                                    }
                                }
                                toIndent = lines.join("\n");
                                $scope.editor.setSelection(selection.start, selection.end);
                                $scope.editor.replaceSelectedText(toIndent);
                                $scope.editor.setSelection(selection.start, selection.start + toIndent.length);
                            } else {
                                for (i in lines) {
                                    lines[i] = tab + lines[i];
                                }
                                toIndent = lines.join("\n");
                                $scope.editor.setSelection(selection.start, selection.end);
                                $scope.editor.replaceSelectedText(toIndent);
                                $scope.editor.setSelection(selection.start, selection.start + toIndent.length);
                            }

                        } else {
                            var left = value.substring(0, pos),
                                right = value.substring(pos),
                                edited = left + tab + right;

                            if (outdent) {
                                if (value.substring(pos - tablength, pos) == tab) {
                                    edited = value.substring(0, pos - tablength) + right;
                                    $scope.editor.val(edited);
                                    $scope.editor.setSelection(pos - tablength, pos - tablength);
                                }
                            } else {
                                $scope.editor.val(edited);
                                $scope.editor.setSelection(pos + tablength, pos + tablength);
                            }
                        }
                    };

                    $scope.surroundClicked = function (str, func) {
                        if ($scope.editor.getSelection().text == "") {
                            $scope.selectWord();
                        }
                        var surrounded = (func) ? func() : $scope.surroundedBy(str);
                        if (surrounded) {
                            var selection = $scope.editor.getSelection();
                            var word = selection.text;
                            var start = selection.start - str.length;
                            var end = selection.end + str.length;
                            $scope.editor.setSelection(start, end);
                            $scope.editor.replaceSelectedText(word, 'select');
                        } else {
                            $scope.editor.surroundSelectedText(str, str, 'select');
                        }
                    };

                    $scope.codeBlockClicked = function () {
                        $scope.editor.surroundSelectedText("```\n", "\n```", 'select');
                    };

                    $scope.headerClicked = function (head) {
                        var selection = $scope.selectLine(true);
                        var tempEnd = selection.end;
                        var line = $scope.editor.getSelection().text;
                        var original = 0;
                        while (line.charAt(0) === '#') {
                            original++;
                            line = line.substr(1);
                        }
                        line = line.substr(1);
                        line = line.trim();
                        $scope.editor.replaceSelectedText(head + ' ' + line);
                        $scope.editor.setSelection(tempEnd + (head.length - original), tempEnd + (head.length - original));
                        $scope.wrapFn();
                    };

                    $scope.selectLine = function (select) {
                        var selection = $scope.editor.getSelection();
                        var value = $scope.editor.val();
                        var tempStart = selection.start;
                        while (tempStart > 0) {
                            tempStart--;
                            if (value.charAt(tempStart) == "\n" || tempStart < 0) {
                                tempStart += 1;
                                break;
                            }
                        }
                        var tempEnd = selection.start;
                        while (tempEnd < value.length) {
                            if (value.charAt(tempEnd) == "\n" || tempEnd >= value.length) {
                                break;
                            }
                            tempEnd++;
                        }
                        if (select) $scope.editor.setSelection(tempStart, tempEnd);
                        return {start: tempStart, end: tempEnd};
                    };

                    $scope.selectWord = function () {
                        var nonASCIISingleCaseWordChar = /[\u00df\u0587\u0590-\u05f4\u0600-\u06ff\u3040-\u309f\u30a0-\u30ff\u3400-\u4db5\u4e00-\u9fcc\uac00-\ud7af]/;
                        var isWordCharBasic = function (ch) {
                            return /\w/.test(ch) || ch > "\x80" &&
                                (ch.toUpperCase() != ch.toLowerCase() || nonASCIISingleCaseWordChar.test(ch)) && !/\s/.test(ch);
                        };
                        var selection = $scope.editor.getSelection();
                        var doc = $scope.editor.val(), coords = $scope.selectLine(false);
                        var line = doc.substring(coords.start, coords.end);
                        var linestart = coords.start, lineend = coords.end;
                        if (line) {
                            var tempStart = selection.start;
                            while (tempStart > linestart && isWordCharBasic(doc.charAt(tempStart - 1))) {
                                tempStart--;
                            }
                            var tempEnd = selection.start;
                            while (tempEnd < lineend && isWordCharBasic(doc.charAt(tempEnd))) {
                                tempEnd++;
                            }
                            if (tempStart != tempEnd) {
                                $scope.editor.setSelection(tempStart, tempEnd);
                                return true;
                            }
                        }
                        return false;
                    };

                    $scope.surroundedBy = function (string) {
                        var value = $scope.editor.val();
                        var selection = $scope.editor.getSelection();
                        var word = value.substring(selection.start - string.length, selection.end + string.length);
                        return (word.indexOf(string) === 0 && word.lastIndexOf(string) === (word.length - string.length));
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
                        if ($scope.editor.getSelection().text === "") {
                            if ($scope.selectWord())
                                descDefault = $scope.editor.getSelection().text;
                        } else
                            descDefault = $scope.editor.getSelection().text;
                        $scope.editor.replaceSelectedText(image + "[" + descDefault + "](" + linkDefault + ")");
                    };

                    $scope.listClicked = function () {
                        $scope.selectLine(true);
                        $scope.editor.replaceSelectedText("- " + $scope.editor.getSelection().text);
                    };

                    $scope.insertTemplate = function (text) {
                        $scope.closeMenu(null, close);
                        $scope.editor.replaceSelectedText(text);
                        $scope.wrapFn();
                    };

                    $scope.ruleClicked = function () {
                        $scope.endClicked();
                        $scope.editor.replaceSelectedText("\n---\n");
                    };
                    //Insert
                    //Special characters
                    $scope.charClicked = function ($event) {
                        var character = $($event.target).text();
                        $scope.editor.replaceSelectedText(character);
                        $scope.wrapFn();
                    };
                    //Special characters
                    //TEX
                    $scope.indexClicked = function () {
                        $scope.editor.surroundSelectedText('_{', '}', 'select');
                    };

                    $scope.powerClicked = function () {
                        $scope.editor.surroundSelectedText('^{', '}', 'select');
                    };

                    $scope.squareClicked = function () {
                        $scope.editor.surroundSelectedText('\\sqrt {', '}', 'select');
                    };
                    //TEX
                } else {
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
                     //TODO: tähän json tiedoston osoite, josta halutaan tädennyksiä
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

                    //Navigation
                    $scope.undoClicked = function () {
                        $scope.editor.undo();
                    };

                    $scope.redoClicked = function () {
                        $scope.editor.redo();
                    };

                    $scope.gotoCursor = function () {
                        var firstrow = $scope.editor.renderer.getFirstFullyVisibleRow();
                        var lastrow = $scope.editor.renderer.getLastFullyVisibleRow();
                        var cursor = $scope.editor.getCursorPosition();
                        if (cursor.row < firstrow) {
                            $scope.editor.renderer.scrollToLine(cursor.row, false, true, function () {
                            });
                        } else if (cursor.row > lastrow) {
                            $scope.editor.renderer.scrollToLine(cursor.row - (lastrow - firstrow), false, true, function () {
                            });
                        }
                    };

                    $scope.leftClicked = function () {
                        $scope.editor.navigateLeft(1);
                        $scope.gotoCursor();
                    };

                    $scope.rightClicked = function () {
                        $scope.editor.navigateRight(1);
                        $scope.gotoCursor();
                    };

                    $scope.upClicked = function () {
                        $scope.editor.navigateUp(1);
                        $scope.gotoCursor();
                    };

                    $scope.downClicked = function () {
                        $scope.editor.navigateDown(1);
                        $scope.gotoCursor();
                    };

                    $scope.homeClicked = function () {
                        $scope.editor.navigateLineStart();
                        $scope.gotoCursor();
                    };

                    $scope.endClicked = function () {
                        $scope.editor.navigateLineEnd();
                        $scope.gotoCursor();
                    };

                    $scope.topClicked = function () {
                        $scope.editor.navigateFileStart();
                        $scope.gotoCursor();
                    };

                    $scope.bottomClicked = function () {
                        $scope.editor.navigateFileEnd();
                        $scope.gotoCursor();
                    };

                    $scope.insertClicked = function () {
                        $scope.editor.setOverwrite(!$scope.editor.getOverwrite());
                    };
                    //Navigation
                    //Style
                    $scope.indentClicked = function () {
                        $scope.editor.indent();
                    };

                    $scope.outdentClicked = function () {
                        $scope.editor.blockOutdent();
                    };

                    $scope.surroundClicked = function (str, func) {
                        if (($scope.editor.session.getTextRange($scope.editor.getSelectionRange()) === "")) {
                            $scope.selectWord();
                        }
                        var text = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                        var surrounded = (func) ? func() : $scope.surroundedBy(str);
                        if (surrounded) {
                            var range = $scope.editor.getSelectionRange();
                            range.start.column -= str.length;
                            range.end.column += str.length;
                            $scope.editor.selection.setRange(range);
                            $scope.snippetManager.insertSnippet($scope.editor, "${0:" + text + "}");
                        } else {
                            $scope.snippetManager.insertSnippet($scope.editor, str + "${0:$SELECTION}" + str);
                        }
                    };

                    $scope.selectWord = function () {
                        var cursor = $scope.editor.getCursorPosition();
                        var wordrange = $scope.editor.getSession().getAWordRange(cursor.row, cursor.column);
                        var word = ($scope.editor.session.getTextRange(wordrange));
                        if (/^\s*$/.test(word)) return false;
                        var wordtrim = word.trim();
                        var difference = word.length - wordtrim.length;
                        wordrange.end.column -= difference;
                        $scope.editor.selection.setRange(wordrange);
                        return true;
                    }

                    $scope.surroundedBy = function (string) {
                        var range = $scope.editor.getSelectionRange();
                        range.start.column -= string.length;
                        range.end.column += string.length;
                        var word = ($scope.editor.session.getTextRange(range));
                        return (word.indexOf(string) === 0 && word.lastIndexOf(string) === (word.length - string.length));
                    };

                    $scope.codeBlockClicked = function () {
                        $scope.snippetManager.insertSnippet($scope.editor, "```\n${0:$SELECTION}\n```");
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
                        $scope.wrapFn();
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
                        if (($scope.editor.session.getTextRange($scope.editor.getSelectionRange()) === "")) {
                            if ($scope.selectWord())
                                descDefault = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                        } else
                            descDefault = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                        $scope.snippetManager.insertSnippet($scope.editor, image + "[" + descDefault + "](${0:" + linkDefault + "})");
                    };

                    $scope.listClicked = function () {
                        $scope.snippetManager.insertSnippet($scope.editor, "- ${0:$SELECTION}");
                    };

                    $scope.insertTemplate = function (text) {
                        $scope.closeMenu(null, close);
                        $scope.snippetManager.insertSnippet($scope.editor, text);
                        $scope.wrapFn();
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
                        $scope.wrapFn();
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
                    //TEX
                }

                $scope.surroundedByItalic = function () {
                    return (($scope.surroundedBy('*') && !$scope.surroundedBy('**')) || $scope.surroundedBy('***'));
                };

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
                                $scope.insertTemplate("![Image](" + $scope.uploadedFile + ")");
                            } else {
                                $scope.uploadedFile = '/files/' + data.file;
                                $scope.progress = 'Uploading... Done!';
                                $scope.insertTemplate("[File](" + $scope.uploadedFile + ")");
                            }
                        }).error(function (data, status, headers, config) {
                            $scope.progress = 'Error while uploading: ' + data.error;
                        });
                    }
                };

                $scope.closeMenu = function (e, force) {
                    var container = $("." + MENU_BUTTON_CLASS);
                    if (force || (!container.is(e.target) && container.has(e.target).length === 0)) {
                        container.remove();
                        $(document).off("mouseup.closemenu");
                    }
                };

                $scope.createMenuButton = function (text, title, clickfunction) {
                    var $span = $("<span>", {class: 'actionButtonRow'});
                    var button_width = 130;
                    $span.append($("<button>", {
                        class: 'timButton',
                        text: text,
                        title: title,
                        'ng-click': clickfunction,
                        width: button_width
                    }));
                    return $span;
                };

                $scope.createMenu = function ($event, buttons) {
                    $scope.closeMenu(null, true);
                    var $button = $($event.target);
                    var coords = {left: $button.position().left, top: $button.position().top};
                    var $actionDiv = $("<div>", {class: MENU_BUTTON_CLASS});

                    for (var i = 0; i < buttons.length; i++) {
                        $actionDiv.append(buttons[i]);
                    }

                    $actionDiv.append($scope.createMenuButton('Close menu', '', 'closeMenu(null, true); wrapFn()'));
                    $actionDiv.offset(coords);
                    $actionDiv.css('position', 'absolute'); // IE needs this
                    $actionDiv = $compile($actionDiv)($scope);
                    $button.parent().prepend($actionDiv);
                    $(document).on('mouseup.closemenu', $scope.closeMenu);
                };

                $scope.tableClicked = function ($event) {
                    var buttons = [];
                    for (var key in $scope.tables) {
                        var text = key.charAt(0).toUpperCase() + key.substring(1);
                        var clickfn = 'insertTemplate(tables[\'' + key + '\']); wrapFn()';
                        buttons.push($scope.createMenuButton(text, '', clickfn));
                    }
                    $scope.createMenu($event, buttons);
                };

                $scope.pluginClicked = function ($event, plugin, index) {
                    $.ajax({
                        dataType: "json",
                        type: 'GET',
                        url: '/' + plugin + '/reqs/',
                        success: function (data) {
                            if (data.templates[index] && data.templates[index].length > 0) {
                                var buttons = [];
                                for (var i = 0; i < data.templates[index].length; i++) {
                                    var template = data.templates[index][i];
                                    var text = (template.text || template.file);
                                    var file = template.file;
                                    var title = template.expl;
                                    var clickfn = 'getTemplate(\'' + plugin + '\',\'' + file + '\', \'' + index + '\'); wrapFn()';
                                    buttons.push($scope.createMenuButton(text, title, clickfn));
                                }
                                $scope.createMenu($event, buttons);
                            }
                        },
                        error: function () {
                            console.log("Virhe");
                        }
                    });
                };

                $scope.getTemplate = function (plugin, template, index) {
                    // $scope.editor.setReadOnly(!$scope.editor.getReadOnly());
                    $.ajax({
                        type: 'GET',
                        url: '/' + plugin + '/template/' + template + '/' + index,
                        success: function (data) {
                            $scope.insertTemplate(data);
                        },
                        error: function () {
                            console.log("Virhe");
                        }
                    });
                    if (!touchDevice) $scope.editor.focus();
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
                    $scope.wrapFn();
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

                var height = window.innerHeight - 15 + 'px';
                $($element).css('max-height', height);
            }
        };
    }]);
