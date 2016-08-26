var angular;
var MENU_BUTTON_CLASS = 'menuButtons';
var timApp = angular.module('timApp');

var currentEditorScope = null;
var editorChangeValue = function(attributes, text) {
   if ( !currentEditorScope ) return;
   currentEditorScope.changeValue(attributes, text);
};

timApp.directive("pareditor", ['Upload', '$http', '$sce', '$compile',
    '$window', '$localStorage', '$timeout', '$ocLazyLoad',
    function (Upload, $http, $sce, $compile, $window, $localStorage, $timeout, $ocLazyLoad) {
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
                currentEditorScope = $scope;
                $scope.setEditorMinSize = function() {
                    var editor = $('pareditor');
                    editor.css('min-height', '40vh');
                    $scope.previewReleased = false;
                    if (JSON.parse($window.localStorage.getItem('previewIsReleased')) === true) {
                        $scope.releaseClicked();
                    }
                    $scope.minSizeSet = true;
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

                $scope.settings = $window.sessionsettings;
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
                        var $previewContent = $('.previewcontent');
                        var previewDiv = $('#previewDiv');
                        // If preview is released make sure that preview doesn't go out of bounds
                        if ($scope.previewReleased) {
                            var previewOffset = previewDiv.offset();
                            if (previewOffset.top < 0 || previewOffset.top > $window.innerHeight) {
                                previewDiv.offset({'top': 0, 'left': previewDiv.offset().left});
                            }
                            if (previewOffset.left < 0 || previewOffset.left > $window.innerWidth) {
                                previewDiv.offset({'top': previewDiv.offset().top, 'left': 0 });
                            }
                        }
                        // Check that editor doesn't go out of bounds
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
                    editor.commands.addCommand({
                        name: 'commentBlock',
                        bindKey: {
                            win: 'Ctrl-Y',
                            mac: 'Command-Y',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.commentClicked();
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
                            } else if (e.keyCode === 89) {
                                $scope.commentClicked();
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
                            var simpleDirectiveUrl = '/mmcq/SimpleDirective.js';
                            var loadingFn = function () {
                                $ocLazyLoad.load(data.js.concat(data.css)).then(function () {
                                    $previewDiv.html($compile(data.texts)($scope));
                                    var len = $previewDiv.children().length;
                                    $scope.$parent.processAllMathDelayed($previewDiv);
                                    $scope.outofdate = false;
                                    $scope.parCount = len;
                                    $('.editorContainer').resize();
                                });
                            };
                            // Workaround: load SimpleDirective.js before other scripts; otherwise there
                            // will be a ReferenceError.
                            if (angular.isUndefined($window.standardDirective)
                                && data.js.indexOf(simpleDirectiveUrl) >= 0) {
                                $.ajax({
                                    dataType: "script",
                                    cache: true,
                                    url: simpleDirectiveUrl
                                }).done(loadingFn);
                            } else {
                                loadingFn();
                            }
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
                    $scope.previewReleased = !($scope.previewReleased);

                    if (div.css("position") == "absolute") {
                        // If preview has been clicked back in, save the preview position before making it static again
                        if ($scope.minSizeSet) {
                            $scope.savePreviewData(true);
                        }
                        div.css("position", "static");
                        div.find(".draghandle").css("visibility", "hidden");
                        content.css("max-width", '');
                        //div.css("width", editor.width() - 8);
                        div.css("display", "default");
                        editor.css('overflow', 'hidden');
                        content.css('max-height', '40vh');
                        //content.css('max-width', editor.width() - 14);
                        content.css('overflow-x', '');
                        content.css('width', '');
                        div.css("padding", 0);
                        document.getElementById("releaseButton").innerHTML = "&#8594;";
                    }
                    else {
                        // If preview has just been released or it was released last time editor was open
                        if ($scope.minSizeSet || JSON.parse($window.localStorage.getItem('previewIsReleased')) === true) {
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
                        }
                        div.css("position", "absolute");
                        editor.css("overflow", "visible");
                        div.find(".draghandle").css("visibility", "visible");
                        div.css("display", "table");
                        //div.css("overflow", "visible");

                        div.css("width", "100%");
                        div.css("padding", 5);
                        var height = window.innerHeight - 90;
                        content.css('max-height', height);
                        content.css("max-width", window.innerWidth - 90);
                        content.css('overflow-x', 'auto');
                        document.getElementById("releaseButton").innerHTML = "&#8592;";
                        div.offset({'left': left, 'top': top});
                    }
                    $scope.adjustPreview();
                };

                $scope.savePreviewData = function (savePreviewPosition) {
                    if (savePreviewPosition) {
                        // Calculate distance from editor's top and left
                        var editorOffset = $('.editorArea').offset();
                        var previewOffset = $('#previewDiv').offset();
                        var left = previewOffset.left;
                        var top = editorOffset.top - previewOffset.top;
                        $window.localStorage.setItem('previewReleasedOffset', JSON.stringify({'left': left, 'top': top}));

                    }
                    if ($scope.previewReleased) {
                        $window.localStorage.setItem('previewIsReleased', JSON.stringify(true));
                    } else {
                        $window.localStorage.setItem('previewIsReleased', JSON.stringify(false));
                    }
                };

                /**
                 * Called when user wants to cancel changes after entering duplicate task-ids
                 */
                $scope.cancelPluginRenameClicked = function () {
                    // Cancels recent changes to paragraph/document
                    $http.post('/cancelChanges/', angular.extend({
                        newPars: $scope.newPars,
                        originalPar: $scope.originalPar,
                        docId: $scope.extraData.docId,
                        parId: $scope.extraData.par
                    }, $scope.extraData)).success(function (data, status, headers, config) {
                        // Remove the form and return to editor
                        $element.find("#pluginRenameForm").get(0).remove();
                        $scope.renameFormShowing = false;
                        $scope.saving = false;
                    }).error(function (data, status, headers, config) {
                        $window.alert("Failed to cancel save: " + data.error);
                    });
                };

                /**
                 * Function that handles different cases of user input in plugin rename form
                 * after user has saved multiple plugins with the same taskname
                 * @param inputs - The input fields in plugin rename form
                 * @param duplicates - The duplicate tasks, contains duplicate taskIds and relevant parIds
                 * @param renameDuplicates - Whether user wants to rename task names or not
                 */
                $scope.renameTaskNamesClicked = function (inputs, duplicates, renameDuplicates) {
                    // If user wants to ignore duplicates proceed like normal after saving
                    if (typeof renameDuplicates === 'undefined' || renameDuplicates === false) {
                        $scope.renameFormShowing = false;
                        if ($scope.options.destroyAfterSave) {
                            $scope.afterSave({
                                extraData: $scope.extraData,
                                saveData: $scope.data
                            });
                            $element.remove();
                            return;
                        }
                    }
                    var duplicateData = [];
                    var duplicate;

                    // if duplicates are to be renamed automatically (user pressed "rename automatically")
                    if (typeof inputs === 'undefined') {
                        if (renameDuplicates) {
                            if (duplicates.length > 0) {
                                for (var i = 0; i < duplicates.length; i++) {
                                    duplicate = [];
                                    duplicate.push(duplicates[i][0]);
                                    duplicate.push("");
                                    duplicate.push(duplicates[i][1]);
                                    duplicateData.push(duplicate);
                                }
                            }
                        }
                    } else {
                        // use given names from the input fields
                        for (var i = 0; i < duplicates.length; i++) {
                            duplicate = [];
                            duplicate.push(duplicates[i][0]);
                            duplicate.push(inputs[i][0].value);
                            duplicate.push(duplicates[i][1]);
                            duplicateData.push(duplicate);
                        }
                    }
                    // Save the new task names for duplicates
                    $http.post('/postNewTaskNames/', angular.extend({
                        duplicates: duplicateData,
                        renameDuplicates: renameDuplicates
                    }, $scope.extraData)).success(function (data, status, headers, config) {
                        // If no new duplicates were founds
                        if(data.duplicates.length <= 0) {
                            $scope.renameFormShowing = false;
                            $scope.afterSave({
                                extraData: $scope.extraData,
                                saveData: $scope.data
                            });
                            if ($scope.options.destroyAfterSave) {
                                $element.remove();
                            }
                        }
                        // If there still are duplicates remake the form
                        if(data.duplicates.length > 0) {
                            $element.find("#pluginRenameForm").get(0).remove();
                            $scope.createPluginRenameForm(data);
                        }
                        if (angular.isDefined($scope.extraData.access)) {
                            $scope.$storage.noteAccess = $scope.extraData.access;
                        }
                    }).error(function (data, status, headers, config) {
                        $window.alert("Failed to save: " + data.error);
                    });
                    if ($scope.options.touchDevice) $scope.changeMeta();
                };


                /**
                 * Function that creates a form for renaming plugins with duplicate tasknames
                 * @param data - The data received after saving editor text
                 */
                $scope.createPluginRenameForm = function (data) {
                    // Hides other texteditor elements when form is created
                    $scope.renameFormShowing = true;
                    $scope.duplicates = data.duplicates;
                    // Get the editor div
                    var $editorTop = $('.editorArea');
                    // Create a new div
                    var $actionDiv = $("<div>", {class: "pluginRenameForm", id: "pluginRenameForm"});
                    $actionDiv.css("position", "relative");
                    // Add warning and info texts
                    $actionDiv.append($("<strong>", {
                        text: 'Warning!'
                    }));
                    $actionDiv.append($("<p>", {
                        text: 'There are multiple objects with the same task name in this document.'
                    }));
                    $actionDiv.append($("<p>", {
                        text: 'Plugins with duplicate task names might not work properly.'
                    }));
                    $actionDiv.append($("<p>", {
                        text: 'Rename the duplicates by writing new names in the field(s) below and click "Save",'
                    }));
                    $actionDiv.append($("<p>", {
                        text: 'choose "Rename automatically" or "Ignore" to proceed without renaming.'
                    }));
                    $actionDiv.append($("<strong>", {
                        text: 'Rename duplicates:'
                    }));

                    // Create the rename form
                    var $form = $("<form>");
                    $scope.inputs = [];
                    var input;
                    var span;

                    // Add inputs and texts for each duplicate
                    for(var i = 0; i < data.duplicates.length; i++) {
                        // Make a span element
                        span = $("<span>");
                        span.css('display', 'block');
                        // Add a warning if the plugin has answers related to it
                        var $warningSpan = $("<span>", {
                            class: "pluginRenameExclamation",
                            text: "!",
                            title: "There are answers related to this task. Those answers might be lost upon renaming this task."
                        });
                        if (data.duplicates[i][2] != 'hasAnswers') {
                            $warningSpan.css('visibility', 'hidden');
                        }
                        span.append($warningSpan);
                        // Add the duplicate name
                        span.append($("<label>", {
                            class: "pluginRenameObject",
                            text: data.duplicates[i][0],
                            for: "newName" + i,
                        }));
                        // Add input field for a new name to the duplicate
                        input = $("<input>", {
                            class: "pluginRenameObject",
                            type: "text",
                            id: data.duplicates[i][1],
                        });
                        // Add the span to the form
                        $scope.inputs.push(input);
                        span.append(input);
                        $form.append(span);
                    }
                    // Make a new div for buttons
                    var $buttonDiv = $("<div>");
                    // A button for saving with input field values or automatically if no values given
                    $buttonDiv.append($("<button>", {
                        class: 'timButton, pluginRenameObject',
                        text: "Save",
                        title: "Rename task names with given names (Ctrl + S)",
                        'ng-click': "renameTaskNamesClicked(inputs, duplicates, true)",
                    }));
                    // Button for renaming duplicates automatically
                    $buttonDiv.append($("<button>", {
                        class: 'timButton, pluginRenameObject',
                        text: "Rename Automatically",
                        title: "Rename duplicate task names automatically",
                        'ng-click': "renameTaskNamesClicked(undefined, duplicates, true)",
                    }));
                    // Button for ignoring duplicates
                    $buttonDiv.append($("<button>", {
                        class: 'timButton, pluginRenameObject',
                        text: "Ignore",
                        title: "Proceed without renaming",
                        'ng-click': "renameTaskNamesClicked(undefined, undefined, false)",
                    }));
                    // Button that allows user to return to edit and cancel save
                    $buttonDiv.append($("<button>", {
                        class: 'timButton, pluginRenameObject',
                        text: "Cancel",
                        title: "Return to editor",
                        'ng-click': "cancelPluginRenameClicked()",
                    }));
                    // Add the new divs to editor container
                    $actionDiv.append($form);
                    $actionDiv.append($buttonDiv);
                    $actionDiv = $compile($actionDiv)($scope);
                    $editorTop.append($actionDiv);
                    // Focus the first input element
                    $scope.inputs[0].focus();
                    $scope.pluginRenameForm = $actionDiv;
                    // Add hotkey for quick saving (Ctrl + S)
                    $scope.pluginRenameForm.keydown( function(e) {
                        if (e.ctrlKey) {
                            if (e.keyCode === 83) {
                                $scope.renameTaskNamesClicked($scope.inputs, $scope.duplicates, true);
                                e.preventDefault();
                            }
                        }
                    });
                    // Scroll the rename form to view
                    $('html, body').scrollTop($editorTop.offset().top);
                };


                $scope.saveClicked = function () {
                    saving = true;
                    if ($scope.renameFormShowing) {
                        $scope.renameTaskNamesClicked($scope.inputs, $scope.duplicates, true);
                    }
                    if ($scope.previewReleased) {
                        $scope.savePreviewData(true);
                    } else $scope.savePreviewData(false);
                    var text = $scope.getEditorText();
                    $http.post($scope.saveUrl, angular.extend({
                        text: text
                    }, $scope.extraData)).success(function (data, status, headers, config) {
                        if (data.duplicates.length > 0) {
                            $scope.data = data;
                            $scope.createPluginRenameForm(data);
                            if (data.original_par !== 'undefined') {
                                $scope.originalPar = data.original_par;
                            }
                            if (data.new_par_ids !== 'undefined') {
                                $scope.newPars = data.new_par_ids;
                            }
                        }
                        if (data.duplicates.length <= 0) {
                            $scope.afterSave({
                                extraData: $scope.extraData,
                                saveData: data
                            });
                            if ($scope.options.destroyAfterSave) {
                                $element.remove();
                            }
                        }
                        if (angular.isDefined($scope.extraData.access)) {
                            $scope.$storage.noteAccess = $scope.extraData.access;
                        }
                    }).error(function (data, status, headers, config) {
                        $window.alert("Failed to save: " + data.error);
                        saving = false;
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

                    $scope.insertTemplate = function (text) {  // for textArea
                        $scope.closeMenu(null, close);
                        var pluginnamehere = "PLUGINNAMEHERE";
                        var searchEndIndex = $scope.editor.getSelection().start;
                        $scope.editor.replaceSelectedText(text);
                        var searchStartIndex = $scope.editor.getSelection().start;
                        var index = $scope.editor.val().lastIndexOf(pluginnamehere, searchStartIndex);
                        if (index > searchEndIndex) {
                            $scope.editor.setSelection(index, index + pluginnamehere.length);
                        }
                        $scope.wrapFn();
                    };
                    
                    $scope.editorStartsWith = function(text)
                    {
                        return  ($scope.editor.val().startsWith(text) );
                    }

                    $scope.changeValue = function(attributes, text) {
                        var sel = $scope.editor.getSelection();
                        var t = $scope.editor.val();
                        if ( t.length == 0 ) return;
                        var st = sel.start;
                        if ( t[st] === "\n" ) st--;
                        var b = t.lastIndexOf("\n", st);
                        var e = t.indexOf("\n", st);
                        if ( b < 0 ) b = 0; else b++;
                        if ( e < 0 ) e = t.length;
                        if ( e <= b ) return;
                        
                        var line = t.substring(b,e);
                        for (var i = 0; i < attributes.length; i++) {
                            var ma = line.match(" *" + attributes[i]);
                            if ( ma ) {
                                line = ma[0] + " " + text;
                                $scope.editor.setSelection(b, e);
                                $scope.editor.replaceSelectedText(line);
                                break;
                            }
                        }
                    };

                    $scope.ruleClicked = function () {
                        $scope.endClicked();
                        $scope.editor.replaceSelectedText("\n#-\n---\n#-\n");
                    };

                    $scope.paragraphClicked = function () {
                        $scope.endClicked();
                        $scope.editor.replaceSelectedText("\n#-\n");
                    };

                    /*
                     * Creates a comment section of selected text, comment block or comments the cursor line
                     */
                    $scope.commentClicked = function () {
                        var editor = $scope.editor.get(0);
                        // If text is selected surround selected text with comment brackets
                        if (editor.selectionEnd - editor.selectionStart == 0) {
                            var pos = editor.selectionEnd;
                            var endOfLastLine = editor.value.lastIndexOf('\n', pos-1);
                            // If the cursor is in the beginning of a line, make the whole line a comment
                            if (pos - endOfLastLine == 1) {
                                $scope.selectLine(true);
                            }
                        }
                        $scope.surroundClicked('{!!!', '!!!}');
                        /*
                        var editor = $scope.editor.get(0);
                        // If text is selected surround selected text with comment brackets
                        if (editor.selectionEnd - editor.selectionStart > 0) {
                            $scope.editor.replaceSelectedText("{!!!" + $scope.editor.getSelection().text + "!!!}");
                        }
                        else {
                            var pos = editor.selectionEnd;
                            var endOfLastLine = editor.value.lastIndexOf('\n', pos-1);
                            // If the cursor is in the beginning of a line, make the whole line a comment
                            if (pos - endOfLastLine == 1) {
                                var selection = $scope.selectLine(true);
                                $scope.editor.replaceSelectedText("{!!!" + $scope.editor.getSelection().text + "!!!}");
                                $scope.editor.setSelection(selection.end + 4, selection.end + 4);
                            }
                            else {
                                // If the cursor is in another position place a comment block
                                $scope.editor.replaceSelectedText("{!!!!!!}");
                                $scope.editor.setSelection(pos+4, pos+4);
                            }
                        }*/
                    };

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

                    $scope.insertTemplate = function (text) { // for ACE-editor
                        $scope.closeMenu(null, close);
                        var range = $scope.editor.getSelectionRange();
                        var start = range.start;
                        $scope.snippetManager.insertSnippet($scope.editor, text);
                        var line = $scope.editor.session.getLine(start.row);
                        var pluginnamehere = "PLUGINNAMEHERE";
                        var index = line.lastIndexOf(pluginnamehere);
                        if (index > -1) {
                            range.start.column = index;
                            range.end.row = start.row;
                            range.end.column = index + pluginnamehere.length;
                            $scope.editor.selection.setRange(range);
                        }
                        $scope.wrapFn();
                    };

                    $scope.editorStartsWith = function(text)
                    {
                        return  ($scope.editor.session.getLine(0).startsWith(text) );
                    }

                    $scope.changeValue = function(attributes, text) {
                        var pos = $scope.editor.getCursorPosition();
                        var line = $scope.editor.session.getLine(pos.row);
                        for (var i = 0; i < attributes.length; i++) {
                            var ma = line.match(" *" + attributes[i]);
                            if ( ma ) {
                                var len = line.length;
                                line = ma[0] + " " + text;
                                var range = $scope.editor.getSelectionRange();
                                range.start.column = 0; 
                                range.end.column = len+1;
                                $scope.editor.session.replace(range, line);
                                break;
                            }
                        }

                    };

                    $scope.ruleClicked = function () {
                        $scope.editor.navigateLineEnd();
                        $scope.snippetManager.insertSnippet($scope.editor, "\n#-\n---\n#-\n");
                    };

                    /*
                     * Creates a comment section of selected text, comment block or comments the cursor line
                     */
                    $scope.commentClicked = function () {
                        var selection = $scope.editor.getSelection();
                        var range = selection.getRange();
                        var pos = $scope.editor.getCursorPosition();
                        // If cursor is at the start of a line and there is no selection
                        if (pos.column == 0 && (range.start.row == range.end.row && range.start.column == range.end.column)) {
                            $scope.editor.selection.selectLine(true);
                        }
                        else {
                            // If there is nothing but a comment block in line erase it
                            range.start.column -= 4;
                            range.end.column += 4;
                            var text = $scope.editor.session.getTextRange(range);
                            if (text == "{!!!!!!}") {
                                $scope.editor.selection.setRange(range);
                                $scope.snippetManager.insertSnippet($scope.editor, "");
                                return $scope.wrapFn();
                            }
                        }
                        $scope.surroundClicked('{!!!', '!!!}');
                        $scope.wrapFn();
                        /*

                        if (!selection.isEmpty()) {
                            $scope.editor.insert("{!!!" + $scope.editor.getSelectedText() + "!!!}");
                            //$scope.snippetManager.insertSnippet($scope.editor, "{!!!${0:$SELECTION}!!!}");
                            $scope.wrapFn();
                            return;
                        }
                        else {
                            var pos = $scope.editor.getCursorPosition();
                            if (pos.column == 0) {
                                var line = $scope.editor.session.getLine(pos.row);
                                $scope.editor.selection.selectLine();
                                $scope.editor.insert("{!!!" + line + "!!!}\n");
                                pos.column = line.length + 4;
                                $scope.editor.moveCursorToPosition(pos);
                                $scope.wrapFn();
                            }
                            else {
                                $scope.editor.insert("{!!!!!!}");
                                pos.column = pos.column + 4;
                                $scope.editor.moveCursorToPosition(pos);
                                $scope.wrapFn();
                            }
                        }*/
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
                    $scope.uploadedFile = "";
                    //if (!touchDevice)
                        $scope.editor.focus();
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
                                    // if ( $scope.editor.session.getLine(0).startsWith("``` {") )
                                    if ( $scope.editorStartsWith("``` {") )
                                        $scope.insertTemplate($scope.uploadedFile);
                                    else 
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

                $scope.slideClicked = function ($event) {
                    var buttons = [];
                    buttons.push($scope.createMenuButton("Slide break", "Break text to start a new slide", "wrapFn(ruleClicked)"));
                    buttons.push($scope.createMenuButton("Slide fragment", "Content inside the fragment will be hidden and shown when next is clicked in slide view", "surroundClicked('§§', '§§'); wrapFn()"));
                    buttons.push($scope.createMenuButton("Fragment block", "Content inside will show as a fragment and may contain inner slide fragments", "surroundClicked('<§', '§>'); wrapFn()"));
                    $scope.createMenu($event, buttons);
                }

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
