var angular;
var MENU_BUTTON_CLASS = 'menuButtons';
var timApp = angular.module('timApp');

timApp.directive("pareditor", ['Upload', '$http', '$sce', '$compile', '$window', '$localStorage', '$timeout',
    function (Upload, $http, $sce, $compile, $window, $localStorage, $timeout) {
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
                isAce: '@',
                initialTextUrl: '@'
            },
            controller: function ($scope) {
                $scope.setEditorMinSize = function() {
                    var editor = $('pareditor');
                    editor.css('min-height', editor.height());
                    $scope.minSizeSet = true;
                    $scope.previewReleased = false;
                };

                $scope.deleteAttribute = function(key) {
                    delete $scope.extraData.attrs[key];
                };

                $scope.deleteClass = function(classIndex) {
                    $scope.extraData.attrs.classes.splice(classIndex, 1);
                };

                $scope.addClass = function() {
                    $scope.extraData.attrs.classes.push('');
                };

                $scope.addAttribute = function() {
                    if ($scope.newAttr === 'classes') {
                        $scope.extraData.attrs[$scope.newAttr] = [];
                    } else {
                        $scope.extraData.attrs[$scope.newAttr] = '';
                    }
                    $scope.newAttr = '';
                };

                $scope.settings = $window.settings;
                var $plugintab;
                $scope.pluginButtonList = {};

                function getPluginsInOrder() {
                    for (var plugin in reqs) {
                        var data = reqs[plugin];
                        if (data.templates) {
                            var tabs = data.text || [plugin];
                            for (var j = 0; j < tabs.length; j++) {
                                var tab = tabs[j];
                                if (!$scope.pluginButtonList[tab]) $scope.pluginButtonList[tab] = [];
                                for (var k = 0; k < data.templates[j].length; k++) {
                                    var template = data.templates[j][k];
                                    var text = (template.text || template.file);
                                    var clickfn = 'getTemplate(\'' + plugin + '\',\'' + template.file + '\', \'' + j + '\'); wrapFn()';
                                    $scope.pluginButtonList[tab].push($scope.createMenuButton(text, template.expl, clickfn));
                                }
                            }
                        }
                    }

                    for (var key in $scope.pluginButtonList) {
                        var clickfunction = 'pluginClicked($event, \'' + key + '\')';
                        var button = $("<button>", {
                            class: 'editorButton',
                            text: key,
                            title: key,
                            'ng-click': clickfunction
                        });
                        $plugintab.append($compile(button)($scope));
                    }
                }

                window.setTimeout(function () {
                    $plugintab = $('#pluginButtons');
                    getPluginsInOrder()
                }, 0);

                if ((navigator.userAgent.match(/Trident/i))) {
                    $scope.isIE = true;
                }

                $scope.setInitialText = function () {
                    $scope.setEditorText('Loading text...');
                    $http.get($scope.initialTextUrl, {
                        params: angular.extend({
                            "_": Date.now()
                        }, $scope.extraData)
                    }).success(function (data, status, headers, config) {
                        $scope.setEditorText(data.text);
                        angular.extend($scope.extraData, data.extraData);
                        $scope.aceChanged();
                        $scope.aceReady();
                    }).error(function (data, status, headers, config) {
                        $window.alert('Failed to get text: ' + data.error);
                    });
                };

                $scope.adjustPreview = function () {
                    window.setTimeout(function () {
                        var $editor = $('.editorArea');
                        var editorMaxHeight = $editor.cssUnit('max-height')[0];
                        var $previewContent = $('.previewcontent');
                        var newHeight = $scope.calculateEditorSize();
                        if (newHeight < editorMaxHeight) {
                            $editor.css('height', newHeight);
                        } else {
                            $editor.css('height', editorMaxHeight);
                        }
                        if ($scope.previewReleased) {
                            var previewDiv = $('#previewDiv');
                            var previewOffset = previewDiv.offset();
                            if (previewOffset.top < 0) {
                                previewDiv.offset({'top': 0, 'left': previewDiv.offset().left});
                            }
                            if (previewOffset.left < 0) {
                                previewDiv.offset({'top': previewDiv.offset().top, 'left': 0 });
                            }

                        }
                        var editorOffset = $editor.offset();
                        if (editorOffset.top < 0) {
                            $editor.offset({'top': 0, 'left': $editor.offset().left});
                        }
                        if (editorOffset.left < 0) {
                            $editor.offset({'top': $editor.offset().top, 'left': 0});
                        }
                        $previewContent.scrollTop($scope.scrollPos);
                    }, 25);

                };

                // Calculates what the size of the editor should be
                $scope.calculateEditorSize = function() {
                    var newSize = ($('.draghandle').cssUnit('min-height')[0] +
                    $('#pareditor').cssUnit('height')[0] + 33);
                    if(!$scope.previewReleased) {
                        return newSize;
                    }
                    if($scope.previewReleased) {
                        return newSize + 10;
                    }
                };

                $scope.createAce = function (editor, text) {
                    if (!$scope.minSizeSet) {
                        $scope.setEditorMinSize();
                    }
                    $scope.isAce = true;
                    var line = editor.renderer.lineHeight;
                    var containertop = $('.editorContainer').position().top;
                    var height = $(window).innerHeight() - containertop;
                    var max = Math.floor((height / 2) / line);

                    editor.$blockScrolling = Infinity;
                    editor.renderer.setPadding(10, 10, 10, 30);
                    editor.renderer.setScrollMargin(2, 2, 2, 40);
                    editor.renderer.setVScrollBarAlwaysVisible(true);
                    editor.getSession().setMode("markdown");
                    editor.getSession().setUseWrapMode(false);
                    editor.getSession().setWrapLimitRange(0, 79);
                    //$('.ace_scrollbar-v').css('display', '');

                    editor.setOptions({
                        maxLines: max,
                        minLines: 5,
                        autoScrollEditorIntoView: true,
                        hScrollBarAlwaysVisible: false,
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
                    editor.commands.addCommand({
                        name: 'bold',
                        bindKey: {
                            win: 'Ctrl-B',
                            mac: 'Command-B',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.surroundClicked('**', '**');
                        }
                    });
                    editor.commands.addCommand({
                        name: 'italic',
                        bindKey: {
                            win: 'Ctrl-I',
                            mac: 'Command-I',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.surroundClicked('*', '*', $scope.surroundedByItalic);
                        }
                    });
                    editor.commands.addCommand({
                        name: 'code',
                        bindKey: {
                            win: 'Ctrl-O',
                            mac: 'Command-O',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.surroundClicked('`', '`');
                        }
                    });
                    editor.commands.addCommand({
                        name: 'codeBlock',
                        bindKey: {
                            win: 'Ctrl-Alt-O',
                            mac: 'Command-Alt-O',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.codeBlockClicked();
                        }
                    });
                    editor.commands.addCommand({
                        name: 'h1',
                        bindKey: {
                            win: 'Ctrl-1',
                            mac: 'Command-1',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.headerClicked('#');
                        }
                    });
                    editor.commands.addCommand({
                        name: 'h2',
                        bindKey: {
                            win: 'Ctrl-2',
                            mac: 'Command-2',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.headerClicked('##');
                        }
                    });
                    editor.commands.addCommand({
                        name: 'h3',
                        bindKey: {
                            win: 'Ctrl-3',
                            mac: 'Command-3',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.headerClicked('###');
                        }
                    });
                    editor.commands.addCommand({
                        name: 'h4',
                        bindKey: {
                            win: 'Ctrl-4',
                            mac: 'Command-4',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.headerClicked('####');
                        }
                    });
                    editor.commands.addCommand({
                        name: 'endLine',
                        bindKey: {
                            win: 'Ctrl-Enter',
                            mac: 'Command-Enter',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.endLineClicked();
                        }
                    });
                    editor.commands.addCommand({
                        name: 'insertParagraph',
                        bindKey: {
                            win: 'Shift-Enter',
                            mac: 'Shift-Enter',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.paragraphClicked();
                        }
                    });

                    if (text) editor.getSession().setValue(text);
                };

                $scope.setAceControllerFunctions = function () {
                    $scope.getEditorText = function () {
                        return $scope.editor.getSession().getValue();
                    };
                    $scope.setEditorText = function (text) {
                        $scope.editor.getSession().setValue(text);
                    };
                };

                $scope.createTextArea = function (text) {
                    $scope.isAce = false;
                    var $container = ('.editorContainer');
                    var $textarea = $.parseHTML('<textarea rows="10" ng-model="editorText" ng-change="aceChanged()" ng-trim="false" id="teksti" wrap="off"></textarea>');
                    $compile($textarea)($scope);
                    $($container).append($textarea);
                    $scope.editor = $('#teksti');

                    $scope.editor.keydown(function (e) {
                        if (e.ctrlKey) {
                            if (e.keyCode === 83) {
                                $scope.saveClicked();
                                e.preventDefault();
                            } else if (e.keyCode === 66) {
                                $scope.surroundClicked('**', '**');
                                e.preventDefault();
                            } else if (e.keyCode === 73) {
                                $scope.surroundClicked('*', '*', surroundedByItalic);
                                e.preventDefault();
                            } else if (e.altKey) {
                                if (e.keyCode === 79) {
                                    $scope.codeBlockClicked();
                                    e.preventDefault();
                                }
                            } else if (e.keyCode === 79) {
                                $scope.surroundClicked('`', '`');
                                e.preventDefault();
                            } else if (e.keyCode === 49) {
                                $scope.headerClicked('#');
                                e.preventDefault();
                            } else if (e.keyCode === 50) {
                                $scope.headerClicked('##');
                                e.preventDefault();
                            } else if (e.keyCode === 51) {
                                $scope.headerClicked('###');
                                e.preventDefault();
                            } else if (e.keyCode === 52) {
                                $scope.headerClicked('####');
                                e.preventDefault();
                            } else if (e.keyCode === 13) {
                                $scope.endLineClicked();
                                e.preventDefault();
                            }
                        } else if (e.keyCode === 9) {
                            var outdent = e.shiftKey;
                            $scope.indent(outdent);
                            e.preventDefault();
                        } else if (e.shiftKey) {
                            if (e.keyCode === 13) {
                                $scope.paragraphClicked();
                                e.preventDefault();
                            }
                        }
                    });
                    if (text) $scope.editor.val(text);
                };

                $scope.setTextAreaControllerFunctions = function () {
                    $scope.getEditorText = function () {
                        return $scope.editor.val();
                    };
                    $scope.setEditorText = function (text) {
                        $scope.editor.val(text);
                    };
                };

                $scope.aceLoaded = function (editor) {
                    $scope.editor = editor;
                    $scope.createAce(editor);
                    if ($scope.initialTextUrl) $scope.setInitialText();
                    editor.focus();

                    /*iPad does not open the keyboard if not manually focused to editable area
                     var iOS = /(iPad|iPhone|iPod)/g.test($window.navigator.platform);
                     if (!iOS) editor.focus();*/
                };

                $scope.aceReady = function () {
                    $scope.editor.focus();
                    $scope.bottomClicked();
                };

                $('.editorContainer').on('resize', $scope.adjustPreview);

                $scope.aceChanged = function () {
                    $scope.outofdate = true;
                    if ($scope.timer) {
                        $window.clearTimeout($scope.timer);
                    }

                    $scope.timer = $window.setTimeout(function () {
                        var text = $scope.getEditorText();
                        $scope.scrollPos = $('.previewcontent').scrollTop();
                        $http.post($scope.previewUrl, angular.extend({
                            text: text
                        }, $scope.extraData)).success(function (data, status, headers, config) {
                            var $previewDiv = angular.element(".previewcontent");
                            $previewDiv.html($compile(data.texts)($scope));
                            var len = $previewDiv.children().length;
                            $scope.$parent.processAllMathDelayed($previewDiv);
                            $scope.outofdate = false;
                            $scope.parCount = len;
                            $('.editorContainer').resize();
                        }).error(function (data, status, headers, config) {
                            $window.alert("Failed to show preview: " + data.error);
                        });
                    }, 500);
                };

                if ($scope.options.touchDevice) {
                    $scope.createTextArea();
                    $scope.setTextAreaControllerFunctions();

                    if (!$scope.options.metaset) {
                        var $meta = $("meta[name='viewport']");
                        $scope.oldmeta = $meta[0];
                        $meta.remove();
                        $('head').prepend('<meta name="viewport" content="width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=0">');
                    }
                    $scope.options.metaset = true;
                    if ($scope.initialTextUrl) $scope.setInitialText();

                } else {
                    $scope.setAceControllerFunctions();
                }

                /* Add citation info to help tab */
                document.getElementById('helpCite').setAttribute('value', '#- {rd="' + $scope.extraData.docId + '" rl="no" rp="' + $scope.extraData.par +'"}');

            },
            link: function ($scope, $element, $attrs) {

                $scope.$storage = $localStorage;

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
                   
                $scope.tables['pipe'] = ": Taulukko, jossa tolpat määräävat sarkkeiden paikat.\n\n" +
                    "|Oikea  | Vasen | Oletus | Keskitetty |\n" +
                    "|------:|:-----|---------|:------:|\n" + 
                    "|   12  |  12  |    12   |    12  |\n" +
                    "|  123  |  123 |   123   |   123  |\n" +
                    "|    1  |    1 |     1   |     1  |\n";
                    

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
                    $('head').prepend($meta);
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
                    if ($scope.options.touchDevice) $scope.changeMeta();
                };

                $scope.cancelClicked = function () {
                    if ($scope.options.touchDevice) $scope.changeMeta();
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
                        $scope.savePreviewPosition();
                        div.css("position", "static");
                        div.find(".draghandle").css("visibility", "hidden");
                        div.css("max-width", editor.width() - 8);
                        div.css("width", editor.width() - 8);
                        div.css("display", "default");
                        editor.css('overflow', 'hidden');
                        content.css('max-height', '');
                        content.css('max-width', editor.width() - 14);
                        content.css('overflow-x', '');
                        content.css('width', '');
                        div.css("padding", 0);
                        document.getElementById("releaseButton").innerHTML = "&#8594;";
                    }
                    else {
                        var top = div.offset().top;
                        var left = div.offset().left;
                        if ($window.localStorage.getItem('previewReleasedOffset')) {
                            var savedOffset = (JSON.parse(localStorage.getItem('previewReleasedOffset')));
                            left = savedOffset.left;
                            top = editor.offset().top - savedOffset.top;
                        } else {
                            if ($(window).width() < editor.width() + div.width()) {
                                top += 5;
                                left += 5;
                            } else {
                                top = editor.offset().top;
                                left = editor.offset().left + editor.width() + 3;
                            }
                        }
                        div.css("position", "absolute");
                        editor.css("overflow", "visible");
                        div.find(".draghandle").css("visibility", "visible");
                        div.css("display", "table");
                        //div.css("overflow", "visible");
                        //div.css("width", editor.width());

                        div.css("width", "100%");
                        //div.css("height", "100%");
                        div.css("padding", 5);
                        var height = window.innerHeight - 90;
                        content.css('max-height', height);
                        content.css("max-width", window.innerWidth - 90);
                        content.css('overflow-x', 'auto');
                        //div.css('max-height', )
                        document.getElementById("releaseButton").innerHTML = "&#8592;";
                        div.offset({'left': left, 'top': top});
                    }
                    $scope.previewReleased = !($scope.previewReleased);
                    $scope.adjustPreview();
                };

                $scope.savePreviewPosition = function () {
                    if($scope.previewReleased) {
                        // Calculate distance from editor's top and left
                        var editorOffset = $('.editorArea').offset();
                        var previewOffset = $('#previewDiv').offset();
                        var left = previewOffset.left;
                        var top = editorOffset.top - previewOffset.top;
                        $window.localStorage.setItem('previewReleasedOffset', JSON.stringify({'left': left, 'top': top}));
                    }
                };

                $scope.saveClicked = function () {
                    $scope.savePreviewPosition();
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
                        if (angular.isDefined($scope.extraData.access)) {
                            $scope.$storage.noteAccess = $scope.extraData.access;
                        }
                    }).error(function (data, status, headers, config) {
                        $window.alert("Failed to save: " + data.error);
                    });
                    if ($scope.options.touchDevice) $scope.changeMeta();
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

                $scope.setTextAreaFunctions = function () {

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

                    $scope.surroundClicked = function (before, after, func) {
                        if ($scope.editor.getSelection().text == "") {
                            $scope.selectWord();
                        }
                        var surrounded = (func) ? func() : $scope.surroundedBy(before, after);
                        if (surrounded) {
                            var selection = $scope.editor.getSelection();
                            var word = selection.text;
                            var start = selection.start - before.length;
                            var end = selection.end + after.length;
                            $scope.editor.setSelection(start, end);
                            $scope.editor.replaceSelectedText(word, 'select');
                        } else {
                            $scope.editor.surroundSelectedText(before, after, 'select');
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

                    $scope.surroundedBy = function (before, after) {
                        var value = $scope.editor.val();
                        var selection = $scope.editor.getSelection();
                        var word = value.substring(selection.start - before.length, selection.end + after.length);
                        return (word.indexOf(before) === 0 && word.lastIndexOf(after) === (word.length - after.length));
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

                    $scope.paragraphClicked = function () {
                        $scope.endClicked();
                        $scope.editor.replaceSelectedText("\n#-\n");
                    };
                    /*
                    $scope.slideClicked = function () {
                        $scope.endClicked();
                        $scope.editor.replaceSelectedText($scope.editor.getSelection().text + "\n---------------\n");
                    };
                    */
                    $scope.endLineClicked = function () {
                        var editor = $scope.editor.get(0);
                        var selection = $scope.editor.getSelection();
                        var value = $scope.editor.val();
                        var pos = selection.start;
                        $scope.selectLine(true);
                        var lineToBreak = $scope.editor.getSelection().text;
                        if(lineToBreak.length > 0) {
                            var toKeepInLine = value.substring(editor.selectionStart, pos)
                        } else {
                            var toKeepInLine = "";
                        }
                        if ((editor.selectionEnd - pos) > 0) {
                            var toNextLine = value.substring(pos, editor.selectionEnd);
                        } else {
                            var toNextLine = "";
                        }
                        toNextLine = toNextLine.trim();
                        $scope.editor.replaceSelectedText(toKeepInLine + "\\\n" + toNextLine);
                        $scope.endClicked();
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
                    $scope.setTextAreaControllerFunctions();
                };

                $scope.setAceFunctions = function () {
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

                    $scope.surroundClicked = function (before, after, func) {
                        if (($scope.editor.session.getTextRange($scope.editor.getSelectionRange()) === "")) {
                            $scope.selectWord();
                        }
                        var text = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                        var surrounded = (func) ? func() : $scope.surroundedBy(before, after);
                        if (surrounded) {
                            var range = $scope.editor.getSelectionRange();
                            range.start.column -= before.length;
                            range.end.column += after.length;
                            $scope.editor.selection.setRange(range);
                            $scope.snippetManager.insertSnippet($scope.editor, "${0:" + text + "}");
                        } else {
                            $scope.snippetManager.insertSnippet($scope.editor, before + "${0:$SELECTION}" + after);
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
                    };

                    $scope.surroundedBy = function (before, after) {
                        var range = $scope.editor.getSelectionRange();
                        range.start.column -= before.length;
                        range.end.column += after.length;
                        var word = ($scope.editor.session.getTextRange(range));
                        return (word.indexOf(before) === 0 && word.lastIndexOf(after) === (word.length - after.length));
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

                    $scope.paragraphClicked = function () {
                        $scope.editor.navigateLineEnd();
                        $scope.snippetManager.insertSnippet($scope.editor, "\n#-\n");
                    };
                    /*
                    $scope.slideClicked = function () {
                        $scope.editor.navigateLineEnd();
                        $scope.snippetManager.insertSnippet($scope.editor, "${0:$SELECTION}\n---------------\n");
                    };
                    */
                    $scope.endLineClicked = function () {
                        var pos = $scope.editor.getCursorPosition();
                        var line = $scope.editor.session.getLine(pos.row);
                        var range = $scope.editor.getSelection().getRange();
                        range.start.column = 0;
                        range.end.column = line.length;
                        if(line.length > 0) {
                            var toKeepInLine = line.substring(0, pos.column)
                        } else {
                            var toKeepInLine = "";
                        }
                        if ((line.length - pos.column) > 0) {
                            var toNextLine = line.substring(pos.column, line.end);
                        } else {
                            var toNextLine = "";
                        }
                        toNextLine = toNextLine.trim();
                        $scope.editor.selection.setRange(range);
                        $scope.editor.insert(toKeepInLine + "\\" +"\n" + toNextLine);
                        $scope.wrapFn();
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
                    $scope.setAceControllerFunctions();
                };

                if ($scope.options.touchDevice) {
                    $scope.setTextAreaFunctions();
                } else {
                    $scope.setAceFunctions();
                }

                $scope.surroundedByItalic = function () {
                    return (($scope.surroundedBy('*', '*') && !$scope.surroundedBy('**', '**')) || $scope.surroundedBy('***', '***'));
                };

                $scope.onFileSelect = function (file) {
                    if (!touchDevice) $scope.editor.focus();
                    $scope.file = file;
                    console.log(file);

                    if (file) {
                        $scope.file.progress = 0;
                        $scope.file.error = null;
                        file.upload = Upload.upload({
                            url: '/upload/',
                            data: {
                                file: file
                            }
                        });

                        file.upload.then(function (response) {
                            $timeout(function () {
                                if (response.data.image) {
                                    $scope.uploadedFile = '/images/' + response.data.image;
                                    $scope.insertTemplate("![Image](" + $scope.uploadedFile + ")");
                                } else {
                                    $scope.uploadedFile = '/files/' + response.data.file;
                                    $scope.insertTemplate("[File](" + $scope.uploadedFile + ")");
                                }
                            });
                        }, function (response) {
                            if (response.status > 0)
                                $scope.file.error = response.data.error;
                        }, function (evt) {
                                $scope.file.progress = Math.min(100, parseInt(100.0 *
                                evt.loaded / evt.total));
                        });

                        file.upload.finally(function () {
                        })
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

                $scope.pluginClicked = function ($event, key) {
                    $scope.createMenu($event, $scope.pluginButtonList[key]);
                };

                $scope.getTemplate = function (plugin, template, index) {
                    // $scope.editor.setReadOnly(!$scope.editor.getReadOnly());
                    $.ajax({
                        type: 'GET',
                        url: '/' + plugin + '/template/' + template + '/' + index,
                        dataType: "text",
                        processData: false,
                        success: function (data) {
                            data = data.replace(/\\/g, "\\\\");
                            $scope.insertTemplate(data);
                        },
                        error: function () {
                            console.log("Virhe");
                        }
                    });
                    if (!touchDevice) $scope.editor.focus();
                };

                $scope.tabClicked = function ($event, area) {
                    var active = $($event.target).parent();
                    setsetting('editortab', area);
                    $scope.setActiveTab(active, area);
                    $scope.wrapFn();
                };

                /**
                 * Sets active tab
                 * @param active tab <li> element
                 * @param area area to make visible
                 */
                $scope.setActiveTab = function (active, area) {
                    var naviArea = $('#' + area);
                    var buttons = $('.extraButtonArea');
                    var tabs = $('.tab');
                    for (var i = 0; i < buttons.length; i++) {
                        $(buttons[i]).attr("class", 'extraButtonArea hidden');
                    }
                    for (var i = 0; i < tabs.length; i++) {
                        $(tabs[i]).removeClass('active');
                    }
                    $(active).attr('class', 'tab active');
                    $(naviArea).attr('class', 'extraButtonArea');
                };

                if ($scope.settings['editortab']) {
                    //Timeout is used to ensure that ng-ifs are executed before this
                    window.setTimeout(function () {
                        var tab = $scope.settings['editortab'].substring(0, $scope.settings['editortab'].lastIndexOf('Buttons'));
                        var tabelement = $('#' + tab);
                        if (tabelement.length) {
                            $scope.setActiveTab(tabelement, $scope.settings['editortab']);
                        }
                    }, 0);
                }

                /**
                 * @returns {boolean} true if device supports fullscreen, otherwise false
                 */
                $scope.fullscreenSupported = function () {
                    var div = $($element).get(0);
                    var requestMethod = div.requestFullScreen ||
                        div.webkitRequestFullscreen ||
                        div.webkitRequestFullScreen ||
                        div.mozRequestFullScreen ||
                        div.msRequestFullscreen;
                    return (typeof(requestMethod) !== 'undefined');
                };

                /**
                 * Makes editor div fullscreen
                 */
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

                /**
                 * Switched editor between ace and textarea
                 */
                $scope.changeEditor = function () {
                    var text = $scope.getEditorText();
                    $scope.editorText = text;
                    if ($scope.isAce) {
                        var oldeditor = $('#ace_editor');
                        $scope.setTextAreaFunctions();
                        $scope.createTextArea(text);
                    } else {
                        var oldeditor = $('#teksti');
                        $scope.setAceFunctions();
                        var neweditor = $("<div>", {
                            class: 'editor',
                            id: 'ace_editor',
                            'ng-model': 'editorText'
                        });
                        $('.editorContainer').append($compile(neweditor)($scope));
                        var neweditor = ace.edit("ace_editor");
                        $scope.editor = neweditor;
                        $scope.createAce($scope.editor, text);
                        $scope.editor.getSession().on('change', $scope.aceChanged);
                    }
                    oldeditor.remove();
                    $scope.editor.focus();
                    $scope.adjustPreview();
                };

                var viewport = {};
                viewport.top = $(window).scrollTop();
                viewport.bottom = viewport.top + $(window).height();
                var bounds = {};
                bounds.top = $element.offset().top;
                bounds.bottom = bounds.top + $element.outerHeight();
                if (bounds.bottom > viewport.bottom || bounds.top < viewport.top) {
                    /*$('html, body').animate({
                        scrollTop: $element.offset().top
                    }, 2000);*/
                    $('html, body').scrollTop($element.offset().top);
                }
            }
        };

    }]);
