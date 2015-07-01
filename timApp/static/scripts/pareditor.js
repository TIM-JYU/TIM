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
                $scope.editor = $('#teksti');

                if (!$scope.options.metaset) {
                    var $meta = $("meta[name='viewport']");
                    $scope.oldmeta = $meta[0];
                    $meta.remove();
                    $('head').append('<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=0">');
                }
                $scope.options.metaset = true;

                if ($scope.initialTextUrl) {
                    $scope.editor.val('Loading text...');
                    $http.get($scope.initialTextUrl, {
                        params: {"_": Date.now()}
                    }).success(function (data, status, headers, config) {
                        $scope.editor.val(data.text);
                        $scope.textchanged();
                    }).error(function (data, status, headers, config) {
                        $window.alert('Failed to get text: ' + data.error);
                    });
                }

                /*
                 $(".editorButton").click(function (event) {
                 event.preventDefault();
                 event.stopPropagation();
                 });*/

                $scope.textchanged = function () {
                    $scope.outofdate = true;
                    if ($scope.timer) {
                        $window.clearTimeout($scope.timer);
                    }
                    $scope.timer = $window.setTimeout(function () {
                        var text = $scope.editor.val();
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
                    $('#teksti').focus();
                    //if (!touchDevice) $scope.editor.focus();
                    if (typeof(func) !== 'undefined') (func());
                };

                $scope.changeMeta = function () {
                    $("meta[name='viewport']").remove();
                    var $meta = $($scope.oldmeta);
                    $('head').append($meta);
                };

                $scope.saveClicked = function () {
                    var text = $scope.editor.val();
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
                    $scope.changeMeta();
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
                    $scope.changeMeta();
                };

                $scope.cancelClicked = function () {
                    $scope.changeMeta();
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
                    if (!touchDevice) $scope.editor.focus();
                    $scope.editor.setSelection(tempEnd + (head.length - original), tempEnd + (head.length - original));
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
                        $scope.editor.setSelection(tempStart, tempEnd);
                    }
                };

                $scope.surroundedBy = function (string) {
                    var value = $scope.editor.val();
                    var selection = $scope.editor.getSelection();
                    var word = value.substring(selection.start - string.length, selection.end + string.length);
                    return (word.indexOf(string) === 0 && word.lastIndexOf(string) === (word.length - string.length));
                };

                $scope.surroundedByItalic = function () {
                    return (($scope.surroundedBy('*') && !$scope.surroundedBy('**')) || $scope.surroundedBy('***'));
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
                    };

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
                    $scope.endClicked();
                    $scope.editor.replaceSelectedText("\n---\n");
                };
                //Insert
                //Special characters
                $scope.charClicked = function ($event) {
                    var character = $($event.target).text();
                    $scope.editor.replaceSelectedText(character);
                    if (!touchDevice) $scope.editor.focus();
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

                /*
                 $scope.rootClicked = function () {
                 $scope.snippetManager.insertSnippet($scope.editor, "\\sqrt[$0]{$SELECTION}");
                 };*/
                //TEX
                //Plugins
                $scope.pluginClicked = function (plugin, template) {
                    $.ajax({
                        type: 'GET',
                        url: '/' + plugin + '/template/' + template,
                        success: function (data) {
                            $scope.insertTemplate(data);
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
                                $scope.editor.replaceSelectedText("![Image](" + $scope.uploadedFile + ")");
                            } else {
                                $scope.uploadedFile = '/files/' + data.file;
                                $scope.progress = 'Uploading... Done!';
                                $scope.editor.replaceSelectedText("[File](" + $scope.uploadedFile + ")");
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
        };
    }]);
