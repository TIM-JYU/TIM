// TODO: save cursor position when changing editor

import angular, {IPromise} from "angular";
import $ from "jquery";
import rangyinputs from "rangyinputs";
import {timApp} from "tim/app";
import * as draggable from "tim/directives/draggable";
import {setEditorScope} from "tim/editorScope";
import {markAsUsed} from "tim/utils";
import {setsetting} from "tim/utils";
import Editor = AceAjax.Editor;
import VirtualRenderer = AceAjax.VirtualRenderer;
import Ace = AceAjax.Ace;
import * as acemodule from "tim/ace";
import {IAceEditor} from "../ace-types";
import {lazyLoadTS} from "../lazyLoad";
import {$compile, $http, $localStorage, $log, $timeout, $upload, $window} from "../ngimport";
import {ParCompiler} from "../services/parCompiler";
import {getActiveDocument} from "../controllers/view/document";
import {wrapText} from "tim/controllers/view/editing";

markAsUsed(draggable, rangyinputs);

const MENU_BUTTON_CLASS = "menuButtons";
const MENU_BUTTON_CLASS_DOT = "." + MENU_BUTTON_CLASS;

// don't extend from IScope because then type checking is too lousy
interface IParEditorScope {
    textAreaText: string;
    $parent: any;
    ace: Ace;
    data: {original_par: any};
    dataLoaded: boolean;
    deleteUrl: string;
    deleting: boolean;
    duplicates: any[];
    editor: any;
    editorIndex: number;
    element: JQuery;
    extraData: {attrs: {classes: string[]}, docId: number, par: string, access: string, tags: {markread: boolean}, isComment: boolean};
    file: any;
    initialText: string;
    initialTextUrl: string;
    inputs: JQuery[];
    isAce: boolean;
    isIE: boolean;
    lstag: string;
    minSizeSet: boolean;
    newAttr: string;
    newPars: string[];
    oldmeta: HTMLMetaElement;
    wrap: {n: number};

    options: {
        localSaveTag: string,
        texts: {
            initialText: string;
        },
        showDelete: boolean,
        showPlugins: boolean,
        showSettings: boolean,
        destroyAfterSave: boolean,
        touchDevice: boolean,
        metaset: boolean,
    };
    originalPar: any;
    outofdate: boolean;
    parCount: number;
    pluginButtonList: {[tabName: string]: JQuery[]};
    pluginRenameForm: any;
    previewReleased: boolean;
    previewUrl: string;
    proeditor: boolean;
    renameFormShowing: boolean;
    saveUrl: string;
    saving: boolean;
    scrollPos: number;
    settings: {editortab: string};
    snippetManager: {insertSnippet: (editor: Editor, text: string) => void};
    tables: any;
    timer: IPromise<void>;
    unreadUrl: string;
    uploadedFile: string;

    $on(name: string, func: () => void): void;
    $watch(s: string, param2: (newValue, oldValue) => any, b?: boolean): void;
    editorChanged(): void;
    aceLoaded(editor: Editor): void;
    aceReady(): void;
    addAttribute(): void;
    addClass(): void;
    adjustPreview(): void;
    afterCancel(data: any): void;
    afterDelete(data: any): void;
    afterSave(data: any): void;
    bottomClicked(): void;
    cancelClicked(): void;
    cancelPluginRenameClicked(): void;
    changeEditor(mode: string);
    changeMeta(): void;
    changeValue(attributes: string[], text: string): void;
    charClicked(e: Event, char?: string): void;
    checkWrap(): void;
    closeMenu(e: Event, force: boolean): void;
    codeBlockClicked(): void;
    commentClicked(): void;
    createAce(editor: IAceEditor, text?: string);
    createMenu(e: Event, buttons: JQuery[]): void;
    createMenuButton(text: string, title: string, fn: string): JQuery;
    createPluginRenameForm(data: any);
    createTextArea(text: string): void;
    deleteAttribute(key: string): void;
    deleteClass(index: number): void;
    deleteClicked(): void;
    downClicked(): void;
    editorStartsWith(text: string): boolean;
    endClicked(): void;
    endLineClicked(): void;
    forceWrap(force: boolean): void;
    fullscreenSupported(): boolean;
    getAce(): Ace;
    getEditorText(): string;
    getLocalBool(name: string, defaultValue: boolean): boolean;
    getTemplate(plugin: string, template: string, index: number): void;
    goFullScreen(): void;
    gotoCursor(): void;
    headerClicked(header: string): void;
    homeClicked(): void;
    indent(outdent?: boolean): void;
    indentClicked(): void;
    indexClicked(): void;
    insertClicked(): void;
    insertTemplate(text: string): void;
    setPosition(pos: number): void;
    leftClicked(): void;
    linkClicked(descDefault: string, linkDefault: string, isImage: boolean): void;
    listClicked(): void;
    onFileSelect(file: any): void;
    outdentClicked(): void;
    paragraphClicked(): void;
    pluginClicked(e: Event, key: string): void;
    powerClicked(): void;
    redoClicked(): void;
    releaseClicked(): void;
    renameTaskNamesClicked(inputs: JQuery[], duplicates: string[][], renameDuplicates: boolean): void;
    rightClicked(): void;
    ruleClicked(): void;
    saveClicked(): void;
    saveOldMode(mode: string): void;
    savePreviewData(savePreviewPosition: boolean): void;
    scrollToCaret(): void;
    selectLine(select: boolean): {start: number, end: number};
    selectWord(): boolean;
    setAceControllerFunctions(): void;
    setAceFunctions(): void;
    setActiveTab(active: JQuery, area: string): void;
    setEditorMinSize(): void;
    setEditorText(text: string): void;
    isSettings(text: string): boolean;
    setInitialText(): void;
    setLocalValue(name: string, value: string): void;
    setTextAreaControllerFunctions(): void;
    setTextAreaFunctions(): void;
    showUnread(): void;
    slideClicked(e: Event): void;
    squareClicked(): void;
    styleClicked(descDefault: string, styleDefault: string): void;
    surroundClicked(begin: string, end: string, func?: any);
    surroundedBy(before: string, after: string): boolean;
    surroundedByItalic(): boolean;
    tabClicked(e: Event, area: string);
    tableClicked(e: Event): void;
    texBlockClicked(): void;
    texClicked(): void;
    topClicked(): void;
    undoClicked(): void;
    unreadClicked(): void;
    upClicked(): void;
    wrapFn(fn?: () => void): void;
    $apply(): void;
    $evalAsync(fn: () => any): void;
    pageBreakClicked(): void;
}

timApp.directive("pareditor", [
    function() {
        "use strict";
        return {
            templateUrl: "/static/templates/parEditor.html",
            restrict: "E",
            scope: {
                saveUrl: "@",
                deleteUrl: "@",
                previewUrl: "@",
                unreadUrl: "@",
                extraData: "=",
                afterSave: "&",
                afterCancel: "&",
                afterDelete: "&",
                options: "=",
                isAce: "@",
                initialTextUrl: "@",
            },
            controller: ["$scope", function($scope: IParEditorScope) {
                const tag = $scope.options.localSaveTag || "";
                const storage = $window.localStorage;
                $scope.lstag = tag; // par/note/addAbove

                $scope.getLocalBool = function(name, def) {
                    let ret = def;
                    if (!ret) {
                        ret = false;
                    }
                    const val = storage.getItem(name + $scope.lstag);
                    if (!val) {
                        return ret;
                    }
                    return val === "true";
                };

                setEditorScope($scope);
                var sn = storage.getItem("wrap" + $scope.lstag);
                var n;
                n = parseInt(sn);
                if ( isNaN(n) )
                    if ( sn === '' ) n = '';
                    else n = -90;

                $scope.wrap = {n: n};

                $scope.$on("$destroy", () => {setEditorScope(null);});

                const proeditor = $scope.getLocalBool("proeditor", tag === "par");
                $scope.proeditor = proeditor;

                $scope.setLocalValue = function(name, val) {
                    $window.localStorage.setItem(name + $scope.lstag, val);
                };

                $scope.setEditorMinSize = function() {
                    const editor = $("pareditor");
                    $scope.previewReleased = false;

                    const editorOffsetStr = storage.getItem("editorReleasedOffset" + tag);
                    if (editorOffsetStr) {
                        const editorOffset = JSON.parse(editorOffsetStr);
                        editor.css("left", editorOffset.left - editor.offset().left);
                    }

                    if (storage.getItem("previewIsReleased" + tag) === "true") {
                        $scope.releaseClicked();
                    }
                    $scope.minSizeSet = true;
                };

                $scope.forceWrap = function(force: boolean) {
                    var n = $scope.wrap.n;
                    if ( !n  ) return;
                    if ( n < 0 ) n = -n;
                    var text = $scope.getEditorText();
                    if ( !force ) {
                        if (text.indexOf("```") >= 0) return;
                        if (text.indexOf("|") >= 0) return;
                    }
                    var r = wrapText(text, n);
                    if (!r.modified)  return;
                    if ($scope.editorIndex === 0) {
                        var editor = $scope.editor[0];
                        var start = editor.selectionStart;

                        // $scope.setEditorText(r.s);

                        // editor.select();
                        // $timeout(() => {
                            $($scope.editor).val(r.s);
                            $timeout(() => {
                                editor.selectionStart = start;
                                editor.selectionEnd = start;
                            });
                        // });
                    }
                    else if ($scope.editorIndex === 1) { // ACE
                        var editor = $scope.editor;
                        var cursor = editor.selection.getCursor();
                        var index = editor.session.doc.positionToIndex(cursor, 0);
                        var range =  $scope.editor.getSelection().getRange(); // new Range(0,0, 10000,1000);// $scope.editor.session.doc.indexToPosition(100000);
                        range.start.row = 0; // TODO: easier way to find full range
                        range.end.row = 1000;
                        range.start.column = 0;
                        range.end.column = 1000;
                        // $scope.setEditorText(r.s); // not good, undo does not work
                        editor.session.replace(range, r.s);
                        $timeout(() => {
                            cursor = editor.session.doc.indexToPosition(index, 0);
                            editor.selection.moveCursorToPosition(cursor);
                            editor.selection.clearSelection();
                        });
                    }
                }

                $scope.checkWrap = function() {
                    if ( $scope.wrap.n <= 0 ) return;
                    $timeout(() => { // time to let new char happend
                        $scope.forceWrap(false);
                    });
                }

                /*
                $scope.$watch('usercode', function() {
                    if ( $scope.wrap.n > 0 ) $scope.checkWrap();
                }, true);
                */

                $scope.deleteAttribute = function(key) {
                    delete $scope.extraData.attrs[key];
                };

                $scope.deleteClass = function(classIndex) {
                    $scope.extraData.attrs.classes.splice(classIndex, 1);
                };

                $scope.addClass = function() {
                    $scope.extraData.attrs.classes.push("");
                };

                $scope.addAttribute = function() {
                    if ($scope.newAttr === "classes") {
                        $scope.extraData.attrs[$scope.newAttr] = [];
                    } else {
                        $scope.extraData.attrs[$scope.newAttr] = "";
                    }
                    $scope.newAttr = "";
                };

                $scope.settings = $window.sessionsettings;
                let $plugintab;
                $scope.pluginButtonList = {};

                function getPluginsInOrder() {
                    for (const plugin in $window.reqs) {
                        if ($window.reqs.hasOwnProperty(plugin)) {
                            const data = $window.reqs[plugin];
                            if (data.templates) {
                                const tabs = data.text || [plugin];
                                for (let j = 0; j < tabs.length; j++) {
                                    const tab = tabs[j];
                                    if (!$scope.pluginButtonList[tab]) {
                                        $scope.pluginButtonList[tab] = [];
                                    }
                                    for (let k = 0; k < data.templates[j].length; k++) {
                                        const template = data.templates[j][k];
                                        const text = (template.text || template.file);
                                        const clickfn = "getTemplate('" + plugin + "','" + template.file + "', '" + j + "'); wrapFn()";
                                        $scope.pluginButtonList[tab].push($scope.createMenuButton(text, template.expl, clickfn));
                                    }
                                }
                            }
                        }
                    }

                    for (const key in $scope.pluginButtonList) {
                        if ($scope.pluginButtonList.hasOwnProperty(key)) {
                            const clickfunction = "pluginClicked($event, '" + key + "')";
                            const button = $("<button>", {
                                "class": "editorButton",
                                "text": key,
                                "title": key,
                                "ng-click": clickfunction,
                            });
                            $plugintab.append($compile(button)($scope as any));
                        }
                    }
                }

                window.setTimeout(function() {
                    $plugintab = $("#pluginButtons");
                    getPluginsInOrder();
                }, 0);

                if ((navigator.userAgent.match(/Trident/i))) {
                    $scope.isIE = true;
                }

                $scope.dataLoaded = false; // allow load in first time what ever editor

                $scope.isSettings = function(text) {
                    return text.startsWith('``` {settings=""}')
                };

                $scope.setInitialText = function() {
                    if ($scope.dataLoaded) return;
                    if ( !$scope.initialTextUrl ) {
                        var initialText = "";
                        if ( $scope.options.texts ) initialText = $scope.options.texts.initialText;
                        if ( initialText ) {
                            var pos = initialText.indexOf("⁞");
                            if ( pos >= 0 ) initialText = initialText.replace("⁞", ""); // cursor pos
                            $scope.setEditorText(initialText);
                            $scope.initialText = initialText;
                            angular.extend($scope.extraData, {});
                            $scope.editorChanged();
                            $scope.aceReady();
                            $timeout(function() {
                                if ( pos >= 0 ) $scope.setPosition(pos);
                            }, 10);
                        }
                        $scope.dataLoaded = true;
                        return;
                    }
                    $scope.setEditorText("Loading text...");
                    $http.get<{text: string, extraData: any}>($scope.initialTextUrl, {
                        params: $scope.extraData,
                    }).then(function(response) {
                        const data = response.data;
                        $scope.setEditorText(data.text);
                        $scope.initialText = data.text;
                        if ( $scope.isSettings(data.text) ) {
                            $scope.options.showPlugins = false;
                            $scope.options.showSettings = true;
                        }
                        angular.extend($scope.extraData, data.extraData);
                        $scope.editorChanged();
                        $scope.aceReady();
                    }, function(response) {
                        if (response.status === 404) {
                            if ($scope.extraData.isComment) {
                                $window.alert("This comment has been deleted.");
                            } else {
                                $window.alert("This paragraph has been deleted.");
                            }
                        } else {
                            $window.alert("Error occurred: " + response.data.error);
                        }
                        $timeout(function() {
                            $scope.element.remove();
                        }, 1000);
                    });
                    $scope.dataLoaded = true; // prevent data load in future
                };

                $scope.adjustPreview = function() {
                    window.setTimeout(function() {
                        const $editor = $(".editorArea");
                        const $previewContent = $(".previewcontent");
                        const previewDiv = $("#previewDiv");
                        // If preview is released make sure that preview doesn't go out of bounds
                        if ($scope.previewReleased) {
                            const previewOffset = previewDiv.offset();
                            if (previewOffset.top < 0 /*|| previewOffset.top > $window.innerHeight */) {
                                previewDiv.offset({top: 0, left: previewDiv.offset().left});
                            }
                            if (previewOffset.left < 0 || previewOffset.left > $window.innerWidth) {
                                previewDiv.offset({top: previewDiv.offset().top, left: 0});
                            }
                        }
                        // Check that editor doesn't go out of bounds
                        const editorOffset = $editor.offset();
                        if (editorOffset.top < 0) {
                            $editor.offset({top: 0, left: $editor.offset().left});
                        }
                        if (editorOffset.left < 0) {
                            $editor.offset({top: $editor.offset().top, left: 0});
                        }
                        $previewContent.scrollTop($scope.scrollPos);
                    }, 25);

                };

                $scope.createAce = function(editor, text = null) {
                    if (!$scope.minSizeSet) {
                        $scope.setEditorMinSize();
                    }
                    $scope.isAce = true;
                    const line = editor.renderer.lineHeight;
                    const containertop = $(".editorContainer").position().top;
                    const height = $(window).innerHeight() - containertop;
                    const max = Math.floor((height / 2) / line);

                    editor.$blockScrolling = Infinity;
                    editor.renderer.setPadding(10);
                    editor.renderer.setScrollMargin(2, 2, 2, 40);
                    editor.renderer.setVScrollBarAlwaysVisible(true);
                    if ( $scope.lstag === "addAbove") // for chat no extra behaviors to editor
                        editor.getSession().setMode("ace/mode/text");
                    else
                        editor.getSession().setMode("ace/mode/markdown");
                    editor.getSession().setUseWrapMode(false);
                    editor.getSession().setWrapLimitRange(0, 79);
                    editor.setOptions({
                        maxLines: max,
                        minLines: 5,
                        autoScrollEditorIntoView: true,
                        hScrollBarAlwaysVisible: false,
                        vScrollBarAlwaysVisible: false,
                    });

                    editor.commands.addCommand({
                        name: "saveFile",
                        bindKey: {
                            win: "Ctrl-S",
                            mac: "Command-S",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.saveClicked();
                        },
                    });
                    editor.commands.addCommand({
                        name: "bold",
                        bindKey: {
                            win: "Ctrl-B",
                            mac: "Command-B",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.surroundClicked("**", "**");
                        },
                    });
                    editor.commands.addCommand({
                        name: "italic",
                        bindKey: {
                            win: "Ctrl-I",
                            mac: "Command-I",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.surroundClicked("*", "*", $scope.surroundedByItalic);
                        },
                    });
                    editor.commands.addCommand({
                        name: "code",
                        bindKey: {
                            win: "Ctrl-O",
                            mac: "Command-O",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.surroundClicked("`", "`");
                        },
                    });
                    editor.commands.addCommand({
                        name: "codeBlock",
                        bindKey: {
                            win: "Ctrl-Alt-O",
                            mac: "Command-Alt-O",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.codeBlockClicked();
                        },
                    });
                    editor.commands.addCommand({
                        name: "h1",
                        bindKey: {
                            win: "Ctrl-1",
                            mac: "Command-1",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.headerClicked("#");
                        },
                    });
                    editor.commands.addCommand({
                        name: "h2",
                        bindKey: {
                            win: "Ctrl-2",
                            mac: "Command-2",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.headerClicked("##");
                        },
                    });
                    editor.commands.addCommand({
                        name: "h3",
                        bindKey: {
                            win: "Ctrl-3",
                            mac: "Command-3",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.headerClicked("###");
                        },
                    });
                    editor.commands.addCommand({
                        name: "h4",
                        bindKey: {
                            win: "Ctrl-4",
                            mac: "Command-4",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.headerClicked("####");
                        },
                    });
                    editor.commands.addCommand({
                        name: "endLine",
                        bindKey: {
                            win: "Ctrl-Enter",
                            mac: "Command-Enter",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.endLineClicked();
                        },
                    });
                    editor.commands.addCommand({
                        name: "insertParagraph",
                        bindKey: {
                            win: "Shift-Enter",
                            mac: "Shift-Enter",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.paragraphClicked();
                        },
                    });
                    editor.commands.addCommand({
                        name: "commentBlock",
                        bindKey: {
                            win: "Ctrl-Y",
                            mac: "Command-Y",
                            sender: "editor|cli",
                        },
                        exec(env, args, request) {
                            $scope.commentClicked();
                        },
                    });

                    if (text) {
                        editor.getSession().setValue(text);
                    }

                    editor.commands.addCommand({
                        name: 'pageBreak',
                        bindKey: {
                            win: 'Ctrl-M',
                            mac: 'Command-M',
                            sender: 'editor|cli'
                        },
                        exec: function (env, args, request) {
                            $scope.pageBreakClicked();
                        },
                    });
                };

                $scope.setAceControllerFunctions = function() {
                    $scope.getEditorText = function() {
                        return $scope.editor.getSession().getValue();
                    };
                    $scope.setEditorText = function(text) {
                        if ( !text ) return;
                        $scope.editor.getSession().setValue(text);
                    };
                };

                $scope.createTextArea = function(text) {
                    if (!$scope.minSizeSet) {
                        $scope.setEditorMinSize();
                    }
                    $scope.isAce = false;
                    const $textarea = $(`
<textarea rows="10"
          ng-model="textAreaText"
          ng-change="editorChanged()"
          ng-trim="false"
          id="teksti"
          wrap="off">
</textarea>`);
                    $compile($textarea)($scope as any);
                    $(".editorContainer").append($textarea);
                    $scope.editor = $("#teksti");
                    $scope.editorIndex = 0;

                    $scope.editor.keydown(function(e) {
                        if (e.ctrlKey) {
                            if (e.keyCode === 83) {
                                $scope.saveClicked();
                                e.preventDefault();
                            } else if (e.keyCode === 66) {
                                $scope.surroundClicked("**", "**");
                                e.preventDefault();
                            } else if (e.keyCode === 73) {
                                $scope.surroundClicked("*", "*", $scope.surroundedByItalic);
                                e.preventDefault();
                            } else if (e.altKey) {
                                if (e.keyCode === 79) {
                                    $scope.codeBlockClicked();
                                    e.preventDefault();
                                }
                            } else if (e.keyCode === 79) {
                                $scope.surroundClicked("`", "`");
                                e.preventDefault();
                            } else if (e.keyCode === 89) {
                                $scope.commentClicked();
                                e.preventDefault();
                            } else if (e.keyCode === 49) {
                                $scope.headerClicked("#");
                                e.preventDefault();
                            } else if (e.keyCode === 50) {
                                $scope.headerClicked("##");
                                e.preventDefault();
                            } else if (e.keyCode === 51) {
                                $scope.headerClicked("###");
                                e.preventDefault();
                            } else if (e.keyCode === 52) {
                                $scope.headerClicked("####");
                                e.preventDefault();
                            } else if (e.keyCode === 13) {
                                $scope.endLineClicked();
                                e.preventDefault();
                            }
                        } else if (e.keyCode === 9) {
                            const outdent = e.shiftKey;
                            $scope.indent(outdent);
                            e.preventDefault();
                        } else if (e.shiftKey) {
                            if (e.keyCode === 13) {
                                $scope.paragraphClicked();
                                e.preventDefault();
                            }
                        }
                        $scope.checkWrap();
                    });
                    if (text) {
                        $scope.textAreaText = text;
                    }
                };

                $scope.setTextAreaControllerFunctions = function() {
                    $scope.getEditorText = function() {
                        return $scope.textAreaText;
                    };
                    $scope.setEditorText = function(text) {
                        $scope.textAreaText = text;
                    };
                };

                $scope.aceReady = function() {
                    $scope.editor.focus();
                    $scope.bottomClicked();
                };

                $scope.aceLoaded = function(editor) {
                    $scope.editor = editor;
                    $scope.editorIndex = 1;
                    $scope.createAce(editor as IAceEditor);
                    $scope.setInitialText();

                    editor.focus();
                    $scope.aceReady();

                    $scope.editor.keyBinding.addKeyboardHandler(
                        function() {
                            $scope.checkWrap();
                        }
                    );

                    /*iPad does not open the keyboard if not manually focused to editable area
                     var iOS = /(iPad|iPhone|iPod)/g.test($window.navigator.platform);
                     if (!iOS) editor.focus();*/
                };

                $(".editorContainer").on("resize", $scope.adjustPreview);

                $scope.editorChanged = function() {
                    $scope.$evalAsync(() => {
                        $scope.outofdate = true;
                        if ($scope.timer) {
                            $timeout.cancel($scope.timer);
                        }

                        $scope.timer = $timeout(() => {
                            const text = $scope.getEditorText();
                            $scope.scrollPos = $(".previewcontent").scrollTop();
                            $http.post($scope.previewUrl, angular.extend({
                                text,
                            }, $scope.extraData)).then(async (response) => {
                                const data = response.data;
                                const compiled = await ParCompiler.compile(data, $scope);
                                const $previewDiv = angular.element(".previewcontent");
                                $previewDiv.empty().append(compiled);
                                $scope.outofdate = false;
                                $scope.parCount = $previewDiv.children().length;
                                $(".editorContainer").resize();
                                $scope.$apply();
                            }, (response) => {
                                $window.alert("Failed to show preview: " + response.data.error);
                            });
                            $scope.outofdate = true;
                        }, 500);
                    });
                };

                /* Add citation info to help tab */
                document.getElementById("helpCite").setAttribute("value", '#- {rd="' + $scope.extraData.docId + '" rl="no" rp="' + $scope.extraData.par + '"}');

            }],
            link($scope: IParEditorScope, $element, $attrs) {
                $scope.element = $element;

                $scope.tables = {};

                $scope.tables.normal = "Otsikko1 Otsikko2 Otsikko3 Otsikko4\n" +
                    "-------- -------- -------- --------\n" +
                    "1.rivi   x        x        x       \n" +
                    "2.rivi   x        x        x       ";

                $scope.tables.example = "Table:  Otsikko taulukolle\n\n" +
                    "Otsikko    Vasen laita    Keskitetty    Oikea laita\n" +
                    "---------- ------------- ------------ -------------\n" +
                    "1. rivi      2                  3         4\n" +
                    "2. rivi        1000      2000             30000";

                $scope.tables.noheaders = ":  Otsikko taulukolle\n\n" +
                    "---------- ------------- ------------ -------------\n" +
                    "1. rivi      2                  3         4\n" +
                    "2. rivi        1000      2000             30000\n" +
                    "---------- ------------- ------------ -------------\n";

                $scope.tables.multiline = "Table:  Otsikko taulukolle voi\n" +
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
                $scope.tables.strokes = ": Viivoilla tehty taulukko\n\n" +
                    "+---------------+---------------+----------------------+\n" +
                    "| Hedelmä       | Hinta         | Edut                 |\n" +
                    "+===============+===============+======================+\n" +
                    "| Banaani       |  1.34 €       | - valmis kääre       |\n" +
                    "|               |               | - kirkas väri        |\n" +
                    "+---------------+---------------+----------------------+\n" +
                    "| Appelsiini    |  2.10 €       | - auttaa keripukkiin |\n" +
                    "|               |               | - makea              |\n" +
                    "+---------------+---------------+----------------------+\n";

                $scope.tables.pipe = ": Taulukko, jossa tolpat määräävat sarkkeiden paikat.\n\n" +
                    "|Oikea  | Vasen | Oletus | Keskitetty |\n" +
                    "|------:|:-----|---------|:------:|\n" +
                    "|   12  |  12  |    12   |    12  |\n" +
                    "|  123  |  123 |   123   |   123  |\n" +
                    "|    1  |    1 |     1   |     1  |\n";

                $(document).on("webkitfullscreenchange mozfullscreenchange fullscreenchange MSFullscreenChange", function(event) {
                    const editor = $($element).find("#pareditor").get(0);
                    const doc: any = document;
                    if (!doc.fullscreenElement &&    // alternative standard method
                        !doc.mozFullScreenElement && !doc.webkitFullscreenElement && !doc.msFullscreenElement) {
                        editor.removeAttribute("style");
                    }
                });

                $scope.timer = null;
                $scope.outofdate = false;
                $scope.parCount = 0;
                const touchDevice = false;

                $scope.wrapFn = function(func = null) {
                    if (!touchDevice) {
                        // For some reason, on Chrome, re-focusing the editor messes up scroll position
                        // when clicking a tab and moving mouse while clicking, so
                        // we save and restore it manually.
                        const s = $(window).scrollTop();
                        $scope.editor.focus();
                        $(window).scrollTop(s);
                    }
                    if (func !== null) {
                        func();
                    }
                    if ($scope.isIE) {
                        $scope.editorChanged();
                    }
                };

                $scope.changeMeta = function() {
                    $("meta[name='viewport']").remove();
                    const $meta = $($scope.oldmeta);
                    $("head").prepend($meta);
                };

                $scope.deleteClicked = function() {
                    if (!$scope.options.showDelete) {
                        $scope.cancelClicked(); // when empty and save clicked there is no par
                        return;
                    }
                    if ($scope.deleting) {
                        return;
                    }
                    if (!$window.confirm("Delete - are you sure?")) {
                        return;
                    }
                    $scope.deleting = true;

                    $http.post($scope.deleteUrl, $scope.extraData).
                        then(function(response) {
                            const data = response.data;
                            $scope.afterDelete({
                                extraData: $scope.extraData,
                                saveData: data,
                            });
                            if ($scope.options.destroyAfterSave) {
                                $element.remove();
                            }
                            $scope.deleting = false;
                        }, function(response) {
                            $window.alert("Failed to delete: " + response.data.error);
                            $scope.deleting = false;
                        });
                    if ($scope.options.touchDevice) {
                        $scope.changeMeta();
                    }
                };

                $scope.showUnread = function() {
                    return $scope.extraData.par !== "NEW_PAR" && $element.parents(".par").find(".readline.read").length > 0;
                };

                $scope.unreadClicked = function() {
                    if ($scope.options.touchDevice) {
                        $scope.changeMeta();
                    }
                    $http.put($scope.unreadUrl + "/" + $scope.extraData.par, {}).then(function(response) {
                        $element.parents(".par").find(".readline").removeClass("read read-modified");
                        if ($scope.initialText === $scope.getEditorText()) {
                            $element.remove();
                            $scope.afterCancel({
                                extraData: $scope.extraData,
                            });
                        }
                        getActiveDocument().refreshSectionReadMarks();
                    }, function(response) {
                        $log.error("Failed to mark paragraph as unread");
                    });
                };

                $scope.cancelClicked = function() {
                    if ($scope.options.touchDevice) {
                        $scope.changeMeta();
                    }
                    $element.remove();
                    $scope.afterCancel({
                        extraData: $scope.extraData,
                    });
                };

                $scope.releaseClicked = function() {
                    const div = $("#previewDiv");
                    const content = $(".previewcontent");
                    const editor = $(".editorArea");
                    $scope.previewReleased = !($scope.previewReleased);
                    const tag = $scope.options.localSaveTag || "";
                    const storage = $window.localStorage;

                    if (div.css("position") === "absolute") {
                        // If preview has been clicked back in, save the preview position before making it static again
                        if ($scope.minSizeSet) {
                            $scope.savePreviewData(true);
                        }
                        div.css("position", "static");
                        div.find(".draghandle").css("visibility", "hidden");
                        content.css("max-width", "");
                        div.css("display", "default");
                        editor.css("overflow", "hidden");
                        content.css("max-height", "40vh");
                        content.css("overflow-x", "");
                        content.css("width", "");
                        div.css("padding", 0);
                        document.getElementById("releaseButton").innerHTML = "&#8594;";
                    } else {
                        let top = div.offset().top;
                        let left = div.offset().left;
                        // If preview has just been released or it was released last time editor was open
                        if ($scope.minSizeSet || storage.getItem("previewIsReleased" + tag) === "true") {
                            if (storage.getItem("previewReleasedOffset" + tag)) {
                                const savedOffset = (JSON.parse(storage.getItem("previewReleasedOffset" + tag)));
                                left = editor.offset().left + savedOffset.left;
                                top = editor.offset().top + savedOffset.top;
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
                        div.css("width", "100%");
                        div.css("padding", 5);
                        const height = window.innerHeight - 90;
                        content.css("max-height", height);
                        content.css("max-width", window.innerWidth - 90);
                        content.css("overflow-x", "auto");
                        document.getElementById("releaseButton").innerHTML = "&#8592;";
                        div.offset({left, top});
                    }
                    $scope.adjustPreview();
                };

                $scope.savePreviewData = function(savePreviewPosition) {
                    const tag = $scope.options.localSaveTag || "";
                    const storage = $window.localStorage;
                    const editorOffset = $(".editorArea").offset();
                    storage.setItem("editorReleasedOffset" + tag, JSON.stringify(editorOffset));
                    if (savePreviewPosition) {
                        // Calculate distance from editor's top and left
                        const previewOffset = $("#previewDiv").offset();
                        const left = previewOffset.left - editorOffset.left;
                        const top = previewOffset.top - editorOffset.top;
                        storage.setItem("previewReleasedOffset" + tag, JSON.stringify({left, top}));
                    }
                    storage.setItem("previewIsReleased" + tag, $scope.previewReleased.toString());
                };

                /**
                 * Called when user wants to cancel changes after entering duplicate task-ids
                 */
                $scope.cancelPluginRenameClicked = function() {
                    // Cancels recent changes to paragraph/document
                    $http.post("/cancelChanges/", angular.extend({
                        newPars: $scope.newPars,
                        originalPar: $scope.originalPar,
                        docId: $scope.extraData.docId,
                        parId: $scope.extraData.par,
                    }, $scope.extraData)).then(function(response) {
                        // Remove the form and return to editor
                        $element.find("#pluginRenameForm").get(0).remove();
                        $scope.renameFormShowing = false;
                        $scope.saving = false;
                        $scope.deleting = false;
                    }, function(response) {
                        $window.alert("Failed to cancel save: " + response.data.error);
                    });
                };

                /**
                 * Function that handles different cases of user input in plugin rename form
                 * after user has saved multiple plugins with the same taskname
                 * @param inputs - The input fields in plugin rename form
                 * @param duplicates - The duplicate tasks, contains duplicate taskIds and relevant parIds
                 * @param renameDuplicates - Whether user wants to rename task names or not
                 */
                $scope.renameTaskNamesClicked = function(inputs, duplicates, renameDuplicates = false) {
                    // If user wants to ignore duplicates proceed like normal after saving
                    if (!renameDuplicates) {
                        $scope.renameFormShowing = false;
                        if ($scope.options.destroyAfterSave) {
                            $scope.afterSave({
                                extraData: $scope.extraData,
                                saveData: $scope.data,
                            });
                            $element.remove();
                            return;
                        }
                    }
                    const duplicateData = [];
                    let duplicate;

                    // if duplicates are to be renamed automatically (user pressed "rename automatically")
                    if (typeof inputs === "undefined") {
                        if (renameDuplicates) {
                            if (duplicates.length > 0) {
                                for (let i = 0; i < duplicates.length; i++) {
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
                        for (let j = 0; j < duplicates.length; j++) {
                            duplicate = [];
                            duplicate.push(duplicates[j][0]);
                            duplicate.push((inputs[j][0] as HTMLInputElement).value);
                            duplicate.push(duplicates[j][1]);
                            duplicateData.push(duplicate);
                        }
                    }
                    // Save the new task names for duplicates
                    $http.post<{duplicates: string[]}>("/postNewTaskNames/", angular.extend({
                        duplicates: duplicateData,
                        renameDuplicates,
                    }, $scope.extraData)).then(function(response) {
                        const data = response.data;
                        // If no new duplicates were founds
                        if (data.duplicates.length <= 0) {
                            $scope.renameFormShowing = false;
                            $scope.afterSave({
                                extraData: $scope.extraData,
                                saveData: $scope.data,
                            });
                            if ($scope.options.destroyAfterSave) {
                                $element.remove();
                            }
                        }
                        // If there still are duplicates remake the form
                        if (data.duplicates.length > 0) {
                            $element.find("#pluginRenameForm").get(0).remove();
                            $scope.createPluginRenameForm(data);
                        }
                        if (angular.isDefined($scope.extraData.access)) {
                            $localStorage.noteAccess = $scope.extraData.access;
                        }
                    }, function(response) {
                        $window.alert("Failed to save: " + response.data.error);
                    });
                    if ($scope.options.touchDevice) {
                        $scope.changeMeta();
                    }
                };

                /**
                 * Function that creates a form for renaming plugins with duplicate tasknames
                 * @param data - The data received after saving editor text
                 */
                $scope.createPluginRenameForm = function(data) {
                    // Hides other texteditor elements when form is created
                    $scope.renameFormShowing = true;
                    $scope.duplicates = data.duplicates;
                    // Get the editor div
                    const $editorTop = $(".editorArea");
                    // Create a new div
                    let $actionDiv = $("<div>", {class: "pluginRenameForm", id: "pluginRenameForm"});
                    $actionDiv.css("position", "relative");
                    // Add warning and info texts
                    $actionDiv.append($("<strong>", {
                        text: "Warning!",
                    }));
                    $actionDiv.append($("<p>", {
                        text: "There are multiple objects with the same task name in this document.",
                    }));
                    $actionDiv.append($("<p>", {
                        text: "Plugins with duplicate task names might not work properly.",
                    }));
                    $actionDiv.append($("<p>", {
                        text: 'Rename the duplicates by writing new names in the field(s) below and click "Save",',
                    }));
                    $actionDiv.append($("<p>", {
                        text: 'choose "Rename automatically" or "Ignore" to proceed without renaming.',
                    }));
                    $actionDiv.append($("<strong>", {
                        text: "Rename duplicates:",
                    }));

                    // Create the rename form
                    const $form = $("<form>");
                    $scope.inputs = [];
                    let input;
                    let span;

                    // Add inputs and texts for each duplicate
                    for (let i = 0; i < data.duplicates.length; i++) {
                        // Make a span element
                        span = $("<span>");
                        span.css("display", "block");
                        // Add a warning if the plugin has answers related to it
                        const $warningSpan = $("<span>", {
                            class: "pluginRenameExclamation",
                            text: "!",
                            title: "There are answers related to this task. Those answers might be lost upon renaming this task.",
                        });
                        if (data.duplicates[i][2] !== "hasAnswers") {
                            $warningSpan.css("visibility", "hidden");
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
                    const $buttonDiv = $("<div>");
                    // A button for saving with input field values or automatically if no values given
                    $buttonDiv.append($("<button>", {
                        "class": "timButton, pluginRenameObject",
                        "text": "Save",
                        "title": "Rename task names with given names (Ctrl + S)",
                        "ng-click": "renameTaskNamesClicked(inputs, duplicates, true)",
                    }));
                    // Button for renaming duplicates automatically
                    $buttonDiv.append($("<button>", {
                        "class": "timButton, pluginRenameObject",
                        "text": "Rename Automatically",
                        "title": "Rename duplicate task names automatically",
                        "ng-click": "renameTaskNamesClicked(undefined, duplicates, true)",
                    }));
                    // Button for ignoring duplicates
                    $buttonDiv.append($("<button>", {
                        "class": "timButton, pluginRenameObject",
                        "text": "Ignore",
                        "title": "Proceed without renaming",
                        "ng-click": "renameTaskNamesClicked(undefined, undefined, false)",
                    }));
                    // Button that allows user to return to edit and cancel save
                    $buttonDiv.append($("<button>", {
                        "class": "timButton, pluginRenameObject",
                        "text": "Cancel",
                        "title": "Return to editor",
                        "ng-click": "cancelPluginRenameClicked()",
                    }));
                    // Add the new divs to editor container
                    $actionDiv.append($form);
                    $actionDiv.append($buttonDiv);
                    $actionDiv = $compile($actionDiv)($scope as any);
                    $editorTop.append($actionDiv);
                    // Focus the first input element
                    $scope.inputs[0].focus();
                    $scope.pluginRenameForm = $actionDiv;
                    // Add hotkey for quick saving (Ctrl + S)
                    $scope.pluginRenameForm.keydown(function(e) {
                        if (e.ctrlKey) {
                            if (e.keyCode === 83) {
                                $scope.renameTaskNamesClicked($scope.inputs, $scope.duplicates, true);
                                e.preventDefault();
                            }
                        }
                    });
                };

                $scope.saveClicked = function() {
                    if ($scope.saving) {
                        return;
                    }
                    $scope.saving = true;
                    // if ( $scope.wrap.n != -1) //  wrap -1 is not saved
                    $window.localStorage.setItem("wrap"+$scope.lstag, ""+$scope.wrap.n);
                    
                    if ($scope.renameFormShowing) {
                        $scope.renameTaskNamesClicked($scope.inputs, $scope.duplicates, true);
                    }
                    if ($scope.previewReleased) {
                        $scope.savePreviewData(true);
                    } else {
                        $scope.savePreviewData(false);
                    }
                    const text = $scope.getEditorText();
                    if (text.trim() === "") {
                        $scope.deleteClicked();
                        $scope.saving = false;
                        return;
                    }
                    $http.post<{
                        duplicates: string[],
                        original_par: string,
                        new_par_ids: string[],
                    }>($scope.saveUrl, angular.extend({
                        text,
                    }, $scope.extraData)).then(function(response) {
                        const data = response.data;
                        if (data.duplicates.length > 0) {
                            $scope.data = data;
                            $scope.createPluginRenameForm(data);
                            if (data.original_par !== null) {
                                $scope.originalPar = data.original_par;
                            }
                            if (data.new_par_ids !== null) {
                                $scope.newPars = data.new_par_ids;
                            }
                        }
                        if (data.duplicates.length <= 0) {
                            if ($scope.options.destroyAfterSave) {
                                $element.remove();
                            }
                            $scope.afterSave({
                                extraData: $scope.extraData,
                                saveData: data,
                            });
                        }
                        if (angular.isDefined($scope.extraData.access)) {
                            $localStorage.noteAccess = $scope.extraData.access;
                        }
                        if (angular.isDefined($scope.extraData.tags.markread)) { // TODO: tee silmukassa
                            $window.localStorage.setItem("markread", $scope.extraData.tags.markread.toString());
                        }
                        $window.localStorage.setItem("proeditor" + $scope.lstag, $scope.proeditor.toString());

                        if ($scope.isAce) {
                            $scope.setLocalValue("acewrap", $scope.editor.getSession().getUseWrapMode());
                            $scope.setLocalValue("acebehaviours", $scope.editor.getBehavioursEnabled()); // some of these are in editor and some in session?
                        }
                        $scope.saving = false;

                    }, function(response) {
                        $window.alert("Failed to save: " + response.data.error);
                        $scope.saving = false;
                    });
                    if ($scope.options.touchDevice) {
                        $scope.changeMeta();
                    }
                };

                $scope.selectLine = function(select) {
                    const selection = $scope.editor.getSelection();
                    const value = $scope.editor.val();
                    let tempStart = selection.start;
                    while (tempStart > 0) {
                        tempStart--;
                        if (value.charAt(tempStart) === "\n" || tempStart < 0) {
                            tempStart += 1;
                            break;
                        }
                    }
                    let tempEnd = selection.start;
                    while (tempEnd < value.length) {
                        if (value.charAt(tempEnd) === "\n" || tempEnd >= value.length) {
                            break;
                        }
                        tempEnd++;
                    }
                    if (select) {
                        $scope.editor.setSelection(tempStart, tempEnd);
                    }
                    return {start: tempStart, end: tempEnd};
                };

                $scope.indent = function(outdent) {
                    const tab = "    ";
                    const tablength = tab.length;
                    const selection = $scope.editor.getSelection();
                    const pos = selection.start;
                    const value = $scope.editor.val();

                    if (selection.text !== "") {
                        let tempStart = selection.start;
                        while (tempStart--) {
                            if (value.charAt(tempStart) === "\n") {
                                selection.start = tempStart + 1;
                                break;
                            }
                        }

                        let toIndent = value.substring(selection.start, selection.end);
                        const lines = toIndent.split("\n");

                        if (outdent) {
                            for (let i = 0; i < lines.length; i++) {
                                if (lines[i].substring(0, tablength) === tab) {
                                    lines[i] = lines[i].substring(tablength);
                                }
                            }
                            toIndent = lines.join("\n");
                            $scope.editor.setSelection(selection.start, selection.end);
                            $scope.editor.replaceSelectedText(toIndent);
                            $scope.editor.setSelection(selection.start, selection.start + toIndent.length);
                        } else {
                            for (let j = 0; j < lines.length; j++) {
                                lines[j] = tab + lines[j];
                            }
                            toIndent = lines.join("\n");
                            $scope.editor.setSelection(selection.start, selection.end);
                            $scope.editor.replaceSelectedText(toIndent);
                            $scope.editor.setSelection(selection.start, selection.start + toIndent.length);
                        }

                    } else {
                        const left = value.substring(0, pos);
                        const right = value.substring(pos);
                        let edited = left + tab + right;

                        if (outdent) {
                            if (value.substring(pos - tablength, pos) === tab) {
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

                $scope.setTextAreaFunctions = function() {

                    $scope.saveOldMode("text");
                    //Navigation
                    $scope.undoClicked = function() {
                        document.execCommand("undo", false, null);
                    };

                    $scope.redoClicked = function() {
                        document.execCommand("redo", false, null);
                    };

                    $scope.scrollToCaret = function() {
                        const editor = $scope.editor.get(0);
                        const text = $scope.editor.val();
                        const lineHeight = parseInt($scope.editor.css("line-height"));
                        const height = $scope.editor.height();
                        const currentLine = text.substr(0, editor.selectionStart).split("\n").length;
                        const currentScroll = $scope.editor.scrollTop();
                        const currentLineY = currentLine * lineHeight;
                        if (currentLineY > currentScroll + height) {
                            $scope.editor.scrollTop(currentLineY - height);
                        } else if ((currentLineY - lineHeight) < currentScroll) {
                            $scope.editor.scrollTop(currentLineY - lineHeight);
                        }
                    };

                    $scope.leftClicked = function() {
                        const editor = $scope.editor.get(0);
                        editor.selectionStart = editor.selectionEnd -= 1;
                        $scope.scrollToCaret();
                    };

                    $scope.setPosition = function(pos) {
                        const editor = $scope.editor.get(0);
                        editor.selectionStart = editor.selectionEnd = pos;
                        $scope.scrollToCaret();
                    }

                    $scope.rightClicked = function() {
                        const editor = $scope.editor.get(0);
                        editor.selectionStart = editor.selectionEnd += 1;
                        $scope.scrollToCaret();
                    };

                    $scope.upClicked = function() {
                        const editor = $scope.editor.get(0);
                        let pos = editor.selectionEnd;
                        const prevLine = editor.value.lastIndexOf("\n", pos);
                        const TwoBLine = editor.value.lastIndexOf("\n", prevLine - 1);
                        if (prevLine === -1) {
                            return;
                        }
                        pos = pos - prevLine;
                        editor.selectionStart = editor.selectionEnd = TwoBLine + pos;
                        $scope.scrollToCaret();
                    };

                    $scope.downClicked = function() {
                        const editor = $scope.editor.get(0);
                        let pos = editor.selectionEnd;
                        const prevLine = editor.value.lastIndexOf("\n", pos);
                        const nextLine = editor.value.indexOf("\n", pos + 1);
                        if (nextLine === -1) {
                            return;
                        }
                        pos = pos - prevLine;
                        editor.selectionStart = editor.selectionEnd = nextLine + pos;
                        $scope.scrollToCaret();
                    };

                    $scope.homeClicked = function() {
                        const editor = $scope.editor.get(0);
                        editor.selectionEnd = editor.selectionStart =
                            editor.value.lastIndexOf("\n", editor.selectionEnd - 1) + 1;
                        $scope.scrollToCaret();
                    };

                    $scope.endClicked = function() {
                        const editor = $scope.editor.get(0);
                        const pos = editor.selectionEnd;
                        let i = editor.value.indexOf("\n", pos);
                        if (i === -1) {
                            i = editor.value.length;
                        }
                        editor.selectionStart = editor.selectionEnd = i;
                        $scope.scrollToCaret();
                    };

                    $scope.topClicked = function() {
                        const editor = $scope.editor.get(0);
                        editor.selectionStart = editor.selectionEnd = 0;
                        $scope.editor.scrollTop(0);
                    };

                    $scope.bottomClicked = function() {
                        const editor = $scope.editor.get(0);
                        editor.selectionStart = editor.selectionEnd = editor.value.length;
                        $scope.editor.scrollTop($scope.editor.prop("scrollHeight"));
                    };

                    $scope.insertClicked = function() {
                        const input = document.getElementById("teksti") as HTMLInputElement;
                        input.addEventListener("keypress", () => {
                            const s = input.selectionStart;
                            input.value = input.value.substr(0, s) + input.value.substr(s + 1);
                            input.selectionEnd = s;
                        }, false);
                    };
                    //Navigation
                    //Style
                    $scope.indentClicked = function() {
                        $scope.indent();
                    };

                    $scope.outdentClicked = function() {
                        $scope.indent(true);
                    };

                    $scope.surroundClicked = function(before, after, func = null) {
                        if ($scope.editor.getSelection().text === "") {
                            $scope.selectWord();
                        }
                        const surrounded = (func) ? func() : $scope.surroundedBy(before, after);
                        if (surrounded) {
                            const selection = $scope.editor.getSelection();
                            const word = selection.text;
                            const start = selection.start - before.length;
                            const end = selection.end + after.length;
                            $scope.editor.setSelection(start, end);
                            $scope.editor.replaceSelectedText(word, "select");
                        } else {
                            $scope.editor.surroundSelectedText(before, after, "select");
                        }
                    };

                    $scope.codeBlockClicked = function() {
                        $scope.editor.surroundSelectedText("```\n", "\n```", "select");
                    };

                    $scope.headerClicked = function(head) {
                        const selection = $scope.selectLine(true);
                        const tempEnd = selection.end;
                        let line = $scope.editor.getSelection().text;
                        let original = 0;
                        while (line.charAt(0) === "#") {
                            original++;
                            line = line.substr(1);
                        }
                        line = line.substr(1);
                        line = line.trim();
                        $scope.editor.replaceSelectedText(head + " " + line);
                        $scope.editor.setSelection(tempEnd + (head.length - original), tempEnd + (head.length - original));
                        $scope.wrapFn();
                    };

                    $scope.selectWord = function() {
                        const nonASCIISingleCaseWordChar = /[\u00df\u0587\u0590-\u05f4\u0600-\u06ff\u3040-\u309f\u30a0-\u30ff\u3400-\u4db5\u4e00-\u9fcc\uac00-\ud7af]/;
                        const isWordCharBasic = function(ch) {
                            return /\w/.test(ch) || ch > "\x80" &&
                                (ch.toUpperCase() !== ch.toLowerCase() || nonASCIISingleCaseWordChar.test(ch)) && !/\s/.test(ch);
                        };
                        const selection = $scope.editor.getSelection();
                        const doc = $scope.editor.val();
                        const coords = $scope.selectLine(false);
                        const line = doc.substring(coords.start, coords.end);
                        const linestart = coords.start;
                        const lineend = coords.end;
                        if (line) {
                            let tempStart = selection.start;
                            while (tempStart > linestart && isWordCharBasic(doc.charAt(tempStart - 1))) {
                                tempStart--;
                            }
                            let tempEnd = selection.start;
                            while (tempEnd < lineend && isWordCharBasic(doc.charAt(tempEnd))) {
                                tempEnd++;
                            }
                            if (tempStart !== tempEnd) {
                                $scope.editor.setSelection(tempStart, tempEnd);
                                return true;
                            }
                        }
                        return false;
                    };

                    $scope.surroundedBy = function(before, after) {
                        const value = $scope.editor.val();
                        const selection = $scope.editor.getSelection();
                        const word = value.substring(selection.start - before.length, selection.end + after.length);
                        return (word.indexOf(before) === 0 && word.lastIndexOf(after) === (word.length - after.length));
                    };
                    //Style
                    //Insert
                    /**
                     * @param descDefault Placeholder for description
                     * @param linkDefault Placeholder for link address
                     * @param isImage true, if link is an image
                     */
                    $scope.linkClicked = function(descDefault, linkDefault, isImage) {
                        const image = (isImage) ? "!" : "";
                        if ($scope.editor.getSelection().text === "") {
                            if ($scope.selectWord()) {
                                descDefault = $scope.editor.getSelection().text;
                            }
                        } else {
                            descDefault = $scope.editor.getSelection().text;
                        }
                        $scope.editor.replaceSelectedText(image + "[" + descDefault + "](" + linkDefault + ")");
                    };

                    $scope.listClicked = function() {
                        $scope.selectLine(true);
                        $scope.editor.replaceSelectedText("- " + $scope.editor.getSelection().text);
                    };

                    $scope.insertTemplate = function(text) {  // for textArea
                        $scope.closeMenu(null, true);
                        const pluginnamehere = "PLUGINNAMEHERE";
                        const searchEndIndex = $scope.editor.getSelection().start;
                        $scope.editor.replaceSelectedText(text);
                        const searchStartIndex = $scope.editor.getSelection().start;
                        const index = $scope.editor.val().lastIndexOf(pluginnamehere, searchStartIndex);
                        if (index > searchEndIndex) {
                            $scope.editor.setSelection(index, index + pluginnamehere.length);
                        }
                        $scope.wrapFn();
                    };

                    $scope.editorStartsWith = function(text) {
                        return $scope.editor.val().startsWith(text);
                    };

                    $scope.changeValue = function(attributes, text) {
                        const sel = $scope.editor.getSelection();
                        const t = $scope.editor.val();
                        if (t.length === 0) {
                            return;
                        }
                        let st = sel.start;
                        if (t[st] === "\n") {
                            st--;
                        }
                        let b = t.lastIndexOf("\n", st);
                        let e = t.indexOf("\n", st);
                        if (b < 0) {
                            b = 0;
                        } else {
                            b++;
                        }
                        if (e < 0) {
                            e = t.length;
                        }
                        if (e <= b) {
                            return;
                        }

                        let line = t.substring(b, e);
                        for (let i = 0; i < attributes.length; i++) {
                            const ma = line.match(" *" + attributes[i]);
                            if (ma) {
                                line = ma[0] + " " + text;
                                $scope.editor.setSelection(b, e);
                                $scope.editor.replaceSelectedText(line);
                                break;
                            }
                        }
                    };

                    $scope.ruleClicked = function() {
                        $scope.endClicked();
                        $scope.editor.replaceSelectedText("\n#-\n---\n#-\n");
                    };

                    $scope.paragraphClicked = function() {
                        $scope.endClicked();
                        $scope.editor.replaceSelectedText("\n#-\n");
                    };

                    /*
                     * Creates a comment section of selected text, comment block or comments the cursor line
                     */
                    $scope.commentClicked = function() {
                        const editor = $scope.editor.get(0);
                        // If text is selected surround selected text with comment brackets
                        if (editor.selectionEnd - editor.selectionStart === 0) {
                            const pos = editor.selectionEnd;
                            const endOfLastLine = editor.value.lastIndexOf("\n", pos - 1);
                            // If the cursor is in the beginning of a line, make the whole line a comment
                            if (pos - endOfLastLine === 1) {
                                $scope.selectLine(true);
                            }
                        }
                        $scope.surroundClicked("{!!!", "!!!}");
                    };

                    $scope.endLineClicked = function() {
                        const editor = $scope.editor.get(0);
                        const selection = $scope.editor.getSelection();
                        const value = $scope.editor.val();
                        const pos = selection.start;
                        $scope.selectLine(true);
                        const lineToBreak = $scope.editor.getSelection().text;
                        let toKeepInLine;
                        if (lineToBreak.length > 0) {
                            toKeepInLine = value.substring(editor.selectionStart, pos);
                        } else {
                            toKeepInLine = "";
                        }
                        let toNextLine;
                        if ((editor.selectionEnd - pos) > 0) {
                            toNextLine = value.substring(pos, editor.selectionEnd);
                        } else {
                            toNextLine = "";
                        }
                        toNextLine = toNextLine.trim();
                        $scope.editor.replaceSelectedText(toKeepInLine + "\\\n" + toNextLine);
                        $scope.endClicked();
                    };

                    //Insert
                    //Special characters
                    $scope.charClicked = function($event, char) {
                        let character = $($event.target).text();
                        console.log(char);
                        if (typeof char !== "undefined") {
                            character = char;
                        }
                        $scope.editor.replaceSelectedText(character);
                        $scope.wrapFn();
                    };
                    //Special characters
                    //TEX
                    $scope.indexClicked = function() {
                        $scope.editor.surroundSelectedText("_{", "}", "select");
                    };

                    $scope.powerClicked = function() {
                        $scope.editor.surroundSelectedText("^{", "}", "select");
                    };

                    $scope.squareClicked = function() {
                        $scope.editor.surroundSelectedText("\\sqrt {", "}", "select");
                    };
                    //TEX

                    $scope.pageBreakClicked = function () {
                        var editor = $scope.editor.get(0);
                        var selection = $scope.editor.getSelection();
                        var value = $scope.editor.val();
                        var cursor = selection.start;
                        $scope.selectLine(true);

                        var lineToBreak = $scope.editor.getSelection().text;
                        var toKeepInLine;

                        if(lineToBreak.length > 0) {
                            toKeepInLine = value.substring(editor.selectionStart, cursor) + "\n";
                        } else {
                            toKeepInLine = "";
                        }
                        var toNextLine;
                        if ((editor.selectionEnd - cursor) > 0) {
                            toNextLine = value.substring(cursor, editor.selectionEnd);
                        } else {
                            toNextLine = "";
                        }
                        toNextLine = toNextLine.trim();

                        var breakline = '\n#-{print="false"}\n<div id="CSSpagebreak"><p>!================!Page Break!================!</p></div>\n#-\n';

                        $scope.editor.replaceSelectedText(toKeepInLine + breakline +"\n" + toNextLine);
                    };

                    $scope.setTextAreaControllerFunctions();
                };

                $scope.setAceFunctions = function() {
                    $scope.saveOldMode("ace");

                    $scope.snippetManager = $scope.ace.require("ace/snippets").snippetManager;

                    //Navigation
                    $scope.undoClicked = function() {
                        $scope.editor.undo();
                    };

                    $scope.redoClicked = function() {
                        $scope.editor.redo();
                    };

                    $scope.gotoCursor = function() {
                        const firstrow = $scope.editor.renderer.getFirstFullyVisibleRow();
                        const lastrow = $scope.editor.renderer.getLastFullyVisibleRow();
                        const cursor = $scope.editor.getCursorPosition();
                        if (cursor.row < firstrow) {
                            $scope.editor.renderer.scrollToLine(cursor.row, false, true, function() {
                            });
                        } else if (cursor.row > lastrow) {
                            $scope.editor.renderer.scrollToLine(cursor.row - (lastrow - firstrow), false, true, function() {
                            });
                        }
                    };

                    $scope.setPosition = function(pos) {
                        var range = $scope.editor.session.doc.indexToPosition(pos)
                        $scope.editor.moveCursorTo(range.row, range.column); // TODO: find a way to move to postion
                        $scope.gotoCursor();
                    }

                    $scope.leftClicked = function() {
                        $scope.editor.navigateLeft(1);
                        $scope.gotoCursor();
                    };

                    $scope.rightClicked = function() {
                        $scope.editor.navigateRight(1);
                        $scope.gotoCursor();
                    };

                    $scope.upClicked = function() {
                        $scope.editor.navigateUp(1);
                        $scope.gotoCursor();
                    };

                    $scope.downClicked = function() {
                        $scope.editor.navigateDown(1);
                        $scope.gotoCursor();
                    };

                    $scope.homeClicked = function() {
                        $scope.editor.navigateLineStart();
                        $scope.gotoCursor();
                    };

                    $scope.endClicked = function() {
                        $scope.editor.navigateLineEnd();
                        $scope.gotoCursor();
                    };

                    $scope.topClicked = function() {
                        $scope.editor.navigateFileStart();
                        $scope.gotoCursor();
                    };

                    $scope.bottomClicked = function() {
                        $scope.editor.navigateFileEnd();
                        $scope.gotoCursor();
                    };

                    $scope.insertClicked = function() {
                        $scope.editor.setOverwrite(!$scope.editor.getOverwrite());
                    };
                    //Navigation
                    //Style
                    $scope.indentClicked = function() {
                        $scope.editor.indent();
                    };

                    $scope.outdentClicked = function() {
                        $scope.editor.blockOutdent();
                    };

                    $scope.surroundClicked = function(before, after, func) {
                        if (($scope.editor.session.getTextRange($scope.editor.getSelectionRange()) === "")) {
                            $scope.selectWord();
                        }
                        const text = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                        const surrounded = (func) ? func() : $scope.surroundedBy(before, after);
                        if (surrounded) {
                            const range = $scope.editor.getSelectionRange();
                            range.start.column -= before.length;
                            range.end.column += after.length;
                            $scope.editor.selection.setRange(range);
                            $scope.snippetManager.insertSnippet($scope.editor, "${0:" + text + "}");
                        } else {
                            $scope.snippetManager.insertSnippet($scope.editor, before + "${0:$SELECTION}" + after);
                        }
                    };

                    $scope.selectWord = function() {
                        const cursor = $scope.editor.getCursorPosition();
                        const wordrange = $scope.editor.getSession().getAWordRange(cursor.row, cursor.column);
                        const word = ($scope.editor.session.getTextRange(wordrange));
                        if (/^\s*$/.test(word)) {
                            return false;
                        }
                        const wordtrim = word.trim();
                        const difference = word.length - wordtrim.length;
                        wordrange.end.column -= difference;
                        $scope.editor.selection.setRange(wordrange);
                        return true;
                    };

                    $scope.surroundedBy = function(before, after) {
                        const range = $scope.editor.getSelectionRange();
                        range.start.column -= before.length;
                        range.end.column += after.length;
                        const word = ($scope.editor.session.getTextRange(range));
                        return (word.indexOf(before) === 0 && word.lastIndexOf(after) === (word.length - after.length));
                    };

                    $scope.codeBlockClicked = function() {
                        $scope.snippetManager.insertSnippet($scope.editor, "```\n${0:$SELECTION}\n```");
                    };

                    $scope.headerClicked = function(head) {
                        const cursor = $scope.editor.getCursorPosition();
                        let line = $scope.editor.session.getLine(cursor.row);
                        const range = $scope.editor.getSelection().getRange();
                        range.start.column = 0;
                        range.end.column = line.length;
                        while (line.charAt(0) === "#") {
                            line = line.substr(1);
                        }
                        line = line.trim();
                        $scope.editor.selection.setRange(range);
                        $scope.editor.insert(head + " " + line);
                        $scope.wrapFn();
                    };

                    //Style
                    //Insert
                    /**
                     * @param descDefault Placeholder for description
                     * @param styleDefault Placeholder for link address
                     */
                    $scope.styleClicked = function(descDefault, styleDefault) {
                        if (($scope.editor.session.getTextRange($scope.editor.getSelectionRange()) === "")) {
                            if ($scope.selectWord()) {
                                descDefault = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                            }
                        } else {
                            descDefault = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                        }
                        $scope.snippetManager.insertSnippet($scope.editor, "[" + descDefault + "]{.${0:" + styleDefault + "}}");
                    };

                    /**
                     * @param descDefault Placeholder for description
                     * @param linkDefault Placeholder for link address
                     * @param isImage true, if link is an image
                     */
                    $scope.linkClicked = function(descDefault, linkDefault, isImage) {
                        const image = (isImage) ? "!" : "";
                        if (($scope.editor.session.getTextRange($scope.editor.getSelectionRange()) === "")) {
                            if ($scope.selectWord()) {
                                descDefault = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                            }
                        } else {
                            descDefault = $scope.editor.session.getTextRange($scope.editor.getSelectionRange());
                        }
                        $scope.snippetManager.insertSnippet($scope.editor, image + "[" + descDefault + "](${0:" + linkDefault + "})");
                    };

                    $scope.listClicked = function() {
                        $scope.snippetManager.insertSnippet($scope.editor, "- ${0:$SELECTION}");
                    };

                    $scope.paragraphClicked = function() {
                        $scope.editor.navigateLineEnd();
                        $scope.snippetManager.insertSnippet($scope.editor, "\n#-\n");
                    };

                    $scope.endLineClicked = function() {
                        const pos = $scope.editor.getCursorPosition();
                        const line = $scope.editor.session.getLine(pos.row);
                        const range = $scope.editor.getSelection().getRange();
                        range.start.column = 0;
                        range.end.column = line.length;
                        let toKeepInLine;
                        if (line.length > 0) {
                            toKeepInLine = line.substring(0, pos.column);
                        } else {
                            toKeepInLine = "";
                        }
                        let toNextLine;
                        if ((line.length - pos.column) > 0) {
                            toNextLine = line.substring(pos.column, line.end);
                        } else {
                            toNextLine = "";
                        }
                        toNextLine = toNextLine.trim();
                        $scope.editor.selection.setRange(range);
                        $scope.editor.insert(toKeepInLine + "\\" + "\n" + toNextLine);
                        $scope.wrapFn();
                    };

                    $scope.insertTemplate = function(text) { // for ACE-editor
                        $scope.closeMenu(null, true);
                        const range = $scope.editor.getSelectionRange();
                        const start = range.start;
                        $scope.snippetManager.insertSnippet($scope.editor, text);
                        const line = $scope.editor.session.getLine(start.row);
                        const pluginnamehere = "PLUGINNAMEHERE";
                        const index = line.lastIndexOf(pluginnamehere);
                        if (index > -1) {
                            range.start.column = index;
                            range.end.row = start.row;
                            range.end.column = index + pluginnamehere.length;
                            $scope.editor.selection.setRange(range);
                        }
                        $scope.wrapFn();
                    };

                    $scope.editorStartsWith = function(text) {
                        return $scope.editor.session.getLine(0).startsWith(text);
                    };

                    $scope.changeValue = function(attributes, text) {
                        const pos = $scope.editor.getCursorPosition();
                        let line = $scope.editor.session.getLine(pos.row);
                        for (let i = 0; i < attributes.length; i++) {
                            const ma = line.match(" *" + attributes[i]);
                            if (ma) {
                                const len = line.length;
                                line = ma[0] + " " + text;
                                const range = $scope.editor.getSelectionRange();
                                range.start.column = 0;
                                range.end.column = len + 1;
                                $scope.editor.session.replace(range, line);
                                break;
                            }
                        }

                    };

                    $scope.ruleClicked = function() {
                        $scope.editor.navigateLineEnd();
                        $scope.snippetManager.insertSnippet($scope.editor, "\n#-\n---\n#-\n");
                    };

                    /*
                     * Creates a comment section of selected text, comment block or comments the cursor line
                     */
                    $scope.commentClicked = function() {
                        const selection = $scope.editor.getSelection();
                        const range = selection.getRange();
                        const pos = $scope.editor.getCursorPosition();
                        // If cursor is at the start of a line and there is no selection
                        if (pos.column === 0 && (range.start.row === range.end.row && range.start.column === range.end.column)) {
                            $scope.editor.selection.selectLine(true);
                        } else {
                            // If there is nothing but a comment block in line erase it
                            range.start.column -= 4;
                            range.end.column += 4;
                            const text = $scope.editor.session.getTextRange(range);
                            if (text === "{!!!!!!}") {
                                $scope.editor.selection.setRange(range);
                                $scope.snippetManager.insertSnippet($scope.editor, "");
                                return $scope.wrapFn();
                            }
                        }
                        $scope.surroundClicked("{!!!", "!!!}");
                        $scope.wrapFn();
                    };

                    //Insert
                    //Special characters
                    $scope.charClicked = function($event, char) {
                        let character = $($event.target).text();
                        console.log(char);
                        if (typeof char !== "undefined") {
                            character = char;
                        }
                        $scope.editor.insert(character);
                        $scope.wrapFn();
                    };
                    //Special characters
                    //TEX
                    $scope.texClicked = function() {
                        $scope.snippetManager.insertSnippet($scope.editor, "$${0:$SELECTION}$");
                    };

                    $scope.texBlockClicked = function() {
                        $scope.snippetManager.insertSnippet($scope.editor, "$$${0:$SELECTION}$$");
                    };

                    $scope.indexClicked = function() {
                        $scope.snippetManager.insertSnippet($scope.editor, "_{${0:$SELECTION}}");
                    };

                    $scope.powerClicked = function() {
                        $scope.snippetManager.insertSnippet($scope.editor, "^{${0:$SELECTION}}");
                    };

                    $scope.squareClicked = function() {
                        $scope.snippetManager.insertSnippet($scope.editor, "\\sqrt{${0:$SELECTION}}");
                    };
                    //TEX

                    $scope.pageBreakClicked = function () {
                        var cursor = $scope.editor.getCursorPosition();
                        var line = $scope.editor.session.getLine(cursor.row);
                        var range = $scope.editor.getSelection().getRange();
                        range.start.column = 0;
                        range.end.column = line.length;

                        var toKeepInLine;
                        if(line.length > 0) {
                            toKeepInLine = line.substring(0, cursor.column) + "\n";
                        } else {
                            toKeepInLine = "";
                        }
                        var toNextLine;
                        if ((line.length - cursor.column) > 0) {
                            toNextLine = line.substring(cursor.column, line.end);
                        } else {
                            toNextLine = "";
                        }
                        toNextLine = toNextLine.trim();

                        var breakline = '\n#-{print="false"}\n<div id="CSSpagebreak"><p>!================!Page Break!================!</p></div>\n#-\n';

                        $scope.editor.selection.setRange(range);
                        $scope.editor.insert(toKeepInLine + breakline +"\n" + toNextLine);
                        $scope.wrapFn();
                    };

                    $scope.setAceControllerFunctions();
                };

                $scope.saveOldMode = function(oldMode) {
                    $window.localStorage.setItem("oldMode" + $scope.options.localSaveTag, oldMode);
                };

                $scope.surroundedByItalic = function() {
                    return (($scope.surroundedBy("*", "*") && !$scope.surroundedBy("**", "**")) || $scope.surroundedBy("***", "***"));
                };

                $scope.onFileSelect = function(file) {
                    $scope.uploadedFile = "";
                    $scope.editor.focus();
                    $scope.file = file;

                    if (file) {
                        $scope.file.progress = 0;
                        $scope.file.error = null;
                        file.upload = $upload.upload({
                            data: {
                                file,
                            },
                            method: "POST",
                            url: "/upload/",
                        });

                        file.upload.then(function(response) {
                            $timeout(function() {
                                if (response.data.image) {
                                    $scope.uploadedFile = "/images/" + response.data.image;
                                    if ($scope.editorStartsWith("``` {")) {
                                        $scope.insertTemplate($scope.uploadedFile);
                                    }
                                    else {
                                        $scope.insertTemplate("![Image](" + $scope.uploadedFile + ")");
                                    }
                                } else {
                                    $scope.uploadedFile = "/files/" + response.data.file;
                                    $scope.insertTemplate("[File](" + $scope.uploadedFile + ")");
                                }
                            });
                        }, function(response) {
                            if (response.status > 0) {
                                $scope.file.error = response.data.error;
                            }
                        }, function(evt) {
                            $scope.file.progress = Math.min(100, Math.floor(100.0 *
                                evt.loaded / evt.total));
                        });

                        file.upload.finally(function() {
                        });
                    }
                };

                $scope.closeMenu = function(e, force) {
                    const container = $(MENU_BUTTON_CLASS_DOT);
                    if (force || (!container.is(e.target) && container.has(e.target as Element).length === 0)) {
                        container.remove();
                        $(document).off("mouseup.closemenu");
                    }
                };

                $scope.createMenuButton = function(text, title, clickfunction) {
                    const $span = $("<span>", {class: "actionButtonRow"});
                    const button_width = 130;
                    $span.append($("<button>", {
                        "class": "timButton",
                        "text": text,
                        "title": title,
                        "ng-click": clickfunction,
                        "width": button_width,
                    }));
                    return $span;
                };

                $scope.createMenu = function($event, buttons) {
                    $scope.closeMenu(null, true);
                    const $button = $($event.target);
                    const coords = {left: $button.position().left, top: $button.position().top};
                    let $actionDiv = $("<div>", {class: MENU_BUTTON_CLASS});

                    for (let i = 0; i < buttons.length; i++) {
                        $actionDiv.append(buttons[i]);
                    }

                    $actionDiv.append($scope.createMenuButton("Close menu", "", "closeMenu(null, true); wrapFn()"));
                    $actionDiv.offset(coords);
                    $actionDiv.css("position", "absolute"); // IE needs this
                    $actionDiv = $compile($actionDiv)($scope as any);
                    $button.parent().prepend($actionDiv);
                    $(document).on("mouseup.closemenu", $scope.closeMenu);
                };

                $scope.tableClicked = function($event) {
                    const buttons = [];
                    for (const key in $scope.tables) {
                        if ($scope.tables.hasOwnProperty(key)) {
                            const text = key.charAt(0).toUpperCase() + key.substring(1);
                            const clickfn = "insertTemplate(tables['" + key + "']); wrapFn()";
                            buttons.push($scope.createMenuButton(text, "", clickfn));
                        }
                    }
                    $scope.createMenu($event, buttons);
                };

                $scope.slideClicked = function($event) {
                    const buttons = [];
                    buttons.push($scope.createMenuButton("Slide break", "Break text to start a new slide", "wrapFn(ruleClicked)"));
                    buttons.push($scope.createMenuButton("Slide fragment", "Content inside the fragment will be hidden and shown when next is clicked in slide view", "surroundClicked('§§', '§§'); wrapFn()"));
                    buttons.push($scope.createMenuButton("Fragment block", "Content inside will show as a fragment and may contain inner slide fragments", "surroundClicked('<§', '§>'); wrapFn()"));
                    $scope.createMenu($event, buttons);
                };

                $scope.pluginClicked = function($event, key) {
                    $scope.createMenu($event, $scope.pluginButtonList[key]);
                };

                $scope.getTemplate = function(plugin, template, index) {
                    $.ajax({
                        type: "GET",
                        url: "/" + plugin + "/template/" + template + "/" + index,
                        dataType: "text",
                        processData: false,
                        success(data) {
                            data = data.replace(/\\/g, "\\\\");
                            $scope.insertTemplate(data);
                        },
                        error() {
                            $log.error("Error getting template");
                        },
                    });
                    if (!touchDevice) {
                        $scope.editor.focus();
                    }
                };

                $scope.tabClicked = function($event, area) {
                    const active = $($event.target).parent();
                    setsetting("editortab", area);
                    $scope.setActiveTab(active, area);
                    $scope.wrapFn();
                };

                /**
                 * Sets active tab
                 * @param active tab <li> element
                 * @param area area to make visible
                 */
                $scope.setActiveTab = function(active, area) {
                    const naviArea = $("#" + area);
                    const buttons = $(".extraButtonArea");
                    const tabs = $(".tab");
                    for (let i = 0; i < buttons.length; i++) {
                        $(buttons[i]).attr("class", "extraButtonArea hidden");
                    }
                    for (let j = 0; j < tabs.length; j++) {
                        $(tabs[j]).removeClass("active");
                    }
                    $(active).attr("class", "tab active");
                    $(naviArea).attr("class", "extraButtonArea");
                };

                if ($scope.settings.editortab) {
                    //Timeout is used to ensure that ng-ifs are executed before this
                    window.setTimeout(function() {
                        const tab = $scope.settings.editortab.substring(0, $scope.settings.editortab.lastIndexOf("Buttons"));
                        const tabelement = $("#" + tab);
                        if (tabelement.length) {
                            $scope.setActiveTab(tabelement, $scope.settings.editortab);
                        }
                    }, 0);
                }

                /**
                 * @returns {boolean} true if device supports fullscreen, otherwise false
                 */
                $scope.fullscreenSupported = function() {
                    const div: any = $($element).get(0);
                    const requestMethod = div.requestFullScreen ||
                        div.webkitRequestFullscreen ||
                        div.webkitRequestFullScreen ||
                        div.mozRequestFullScreen ||
                        div.msRequestFullscreen;
                    return (typeof (requestMethod) !== "undefined");
                };

                /**
                 * Makes editor div fullscreen
                 */
                $scope.goFullScreen = function() {
                    const div: any = $($element).find("#pareditor").get(0);
                    const doc: any = document;
                    if (!doc.fullscreenElement &&    // alternative standard method
                        !doc.mozFullScreenElement && !doc.webkitFullscreenElement && !doc.msFullscreenElement) {

                        const requestMethod = div.requestFullScreen ||
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
                        if (doc.exitFullscreen) {
                            doc.exitFullscreen();
                        } else if (doc.msExitFullscreen) {
                            doc.msExitFullscreen();
                        } else if (doc.mozCancelFullScreen) {
                            doc.mozCancelFullScreen();
                        } else if (doc.webkitExitFullscreen) {
                            doc.webkitExitFullscreen();
                        }
                    }
                };

                $scope.getEditorText = function() {return "";}; // for first time use this

                /**
                 * Switched editor between ace and textarea
                 */
                $scope.changeEditor = async (newMode) => {
                    const text = $scope.getEditorText();
                    let oldeditor = null;
                    if ($scope.isAce || newMode === "text") {
                        oldeditor = $("#ace_editor");
                        $scope.setTextAreaControllerFunctions();
                        $scope.setTextAreaFunctions();
                        $scope.createTextArea(text);
                        $scope.setInitialText();
                        $scope.editor.focus();
                    } else {
                        oldeditor = $("#teksti");
                        const ace = (await lazyLoadTS<typeof acemodule>("tim/ace", __moduleName)).ace;
                        $scope.ace = ace;
                        $scope.setAceFunctions();
                        const neweditorElem = $("<div>", {
                            class: "editor",
                            id: "ace_editor",
                        });
                        $(".editorContainer").append(neweditorElem);
                        const neweditor = ace.edit("ace_editor");
                        $scope.aceLoaded(neweditor);
                        $scope.editor = neweditor;
                        $scope.editor.getSession().on("change", () => {
                            $scope.editorChanged();
                        });
                        neweditor.setBehavioursEnabled($scope.getLocalBool("acebehaviours", false));
                        neweditor.getSession().setUseWrapMode($scope.getLocalBool("acewrap", false));
                        neweditor.setOptions({maxLines: 28});
                        $scope.setEditorText(text);
                    }
                    if (oldeditor) {
                        oldeditor.remove();
                    }
                    $scope.adjustPreview();
                    if (!$scope.proeditor && $scope.lstag === "note") {
                        const editor = $("pareditor");
                        editor.css("max-width", "40em");
                    }
                };

                const oldMode = $window.localStorage.getItem("oldMode" + $scope.options.localSaveTag) || ($scope.options.touchDevice ? "text" : "ace");
                $scope.changeEditor(oldMode);

                if ($scope.options.touchDevice) {
                    if (!$scope.options.metaset) {
                        const $meta = $("meta[name='viewport']");
                        $scope.oldmeta = $meta[0] as HTMLMetaElement;
                        $meta.remove();
                        $("head").prepend('<meta name="viewport" content="width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=0">');
                    }
                    $scope.options.metaset = true;
                }

                const viewport: any = {};
                viewport.top = $(window).scrollTop();
                viewport.bottom = viewport.top + $(window).height();
                const bounds: any = {};
                bounds.top = $element.offset().top;
                bounds.bottom = bounds.top + $element.outerHeight();
                if (bounds.bottom > viewport.bottom || bounds.top < viewport.top) {
                    $("html, body").scrollTop($element.offset().top);
                }
            },
        };
    }]);
