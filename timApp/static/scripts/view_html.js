var katex, $, angular, modules, version, refererPath, docId, docName, rights, startIndex, users, teacherMode, crumbs, lectureMode;

var timApp = angular.module('timApp', [
    'ngSanitize',
    'angularFileUpload',
    'ui.ace'].concat(modules)).config(['$httpProvider', function ($httpProvider) {
    var interceptor = [
        '$q',
        '$rootScope',
        function ($q, $rootScope) {
            var re = /\/[^/]+\/([^/]+)\/answer\/$/;
            var service = {
                'request': function (config) {
                    if (teacherMode && re.test(config.url)) {
                        var match = re.exec(config.url);
                        var taskId = match[1];
                        var ab = angular.element("answerbrowser[task-id='" + taskId + "']");
                        var browserScope = ab.isolateScope();
                        if (ab.scope().teacherMode) {
                            angular.extend(config.data, {abData: browserScope.getTeacherData()});
                        }
                    }
                    return config;
                },
                'response': function (response) {

                    if (re.test(response.config.url)) {
                        var match = re.exec(response.config.url);
                        var taskId = match[1];
                        $rootScope.$broadcast('answerSaved', {taskId: taskId});
                    }
                    return response;
                }
            };
            return service;
        }
    ];
}]);

timApp.controller("ViewCtrl", [
    '$scope',
    '$http',
    '$q',
    '$upload',
    '$injector',
    '$compile',
    '$window',
    '$document',
    '$rootScope',
    function (sc, http, q, $upload, $injector, $compile, $window, $document, $rootScope) {
        "use strict";
        http.defaults.headers.common.Version = version.hash;
        http.defaults.headers.common.RefererPath = refererPath;
        sc.docId = docId;
        sc.docName = docName;
        sc.crumbs = crumbs;
        sc.rights = rights;
        sc.startIndex = startIndex;
        sc.users = users;
        sc.group = group;
        sc.teacherMode = teacherMode;
        sc.sidebarState = 'autohidden';
        if (sc.users.length > 0) {
            sc.selectedUser = sc.users[0];
        } else {
            sc.selectedUser = null;
        }

        sc.noteClassAttributes = ["difficult", "unclear", "editable", "private"];
        sc.editing = false;
        var NOTE_EDITOR_CLASS = "editorArea";
        var DEFAULT_CHECKBOX_CLASS = "defaultCheckbox";
        var ACTION_BUTTON_ROW_CLASS = "actionButtonRow";
        sc.lectureMode = lectureMode;
        sc.questionShown = false;
        sc.firstTimeQuestions = true;
        var DEFAULT_BUTTON_CLASS = "timButton defaultButton";
        var NOTE_ADD_BUTTON_CLASS = "timButton addNote";
        var NOTE_ADD_BUTTON = "." + NOTE_ADD_BUTTON_CLASS.replace(" ", ".");
        var EDITOR_CLASS = "editorArea";
        var EDITOR_CLASS_DOT = "." + EDITOR_CLASS;
        var PAR_ADD_BUTTON_CLASS = "timButton addPar";
        var PAR_ADD_BUTTON = "." + PAR_ADD_BUTTON_CLASS.replace(" ", ".");
        var PAR_EDIT_BUTTON_CLASS = "timButton editPar";
        var PAR_EDIT_BUTTON = "." + PAR_EDIT_BUTTON_CLASS.replace(" ", ".");
        var QUESTION_ADD_BUTTON_CLASS = "timButton addQuestion";
        var QUESTION_ADD_BUTTON = "." + QUESTION_ADD_BUTTON_CLASS.replace(" ", ".");
        var PAR_CLOSE_BUTTON_CLASS = "timButton menuClose";
        var PAR_CLOSE_BUTTON = "." + PAR_CLOSE_BUTTON_CLASS.replace(" ", ".");

        sc.defaults = [false, false, false, false, false, false];

        sc.updateSelection = function (index) {
            var selected = false;
            for (var i = 0; i < sc.defaults.length; i++) {
                if (sc.defaults[i]) selected = true;
                if (i != index) {
                    sc.defaults[i] = false;
                }
            }

            if (selected) {
                sc.defaultAction = sc.editorFunctions[index];
            } else {
                sc.defaultAction = sc.showOptionsWindow;
            }
        };

        sc.processAllMath = function ($elem) {
            $elem.find('.math').each(function () {
                sc.processMath(this);
            });
        };

        sc.processMath = function (elem) {
            var $this = $(elem);
            var math = $this.text();
            var hasDisplayMode = false;
            if (math[1] === '[') {
                hasDisplayMode = true;
            }
            else if (math[1] !== '(') {
                return;
            }
            katex.render(math.slice(2, -2), elem, {displayMode: hasDisplayMode});
        };


        sc.changeUser = function (user) {
            sc.$broadcast('userChanged', {user: user});
        };

        sc.getParId = function ($par) {
            if ($par.length === 0 || !$par.hasClass('par')) {
                return null;
            }
            return $par.attr("id");
        };

        sc.getElementByParId = function (id) {
            return $("#" + id);
        };

        sc.toggleParEditor = function ($par, options) {
            var caption = 'Add paragraph';
            var touch = typeof('ontouchstart' in window || navigator.msMaxTouchPoints) !== 'undefined';
            var mobile = touch && (window.screen.width < 1200);
            var url;
            var par_id = sc.getParId($par);
            var par_next_id = sc.getParId($par.next());
            if ($par.hasClass("new")) {
                url = '/newParagraph/';
            } else {
                url = '/postParagraph/';
            }

            var attrs = {
                "save-url": url,
                "extra-data": JSON.stringify({
                    docId: sc.docId,
                    par: par_id,
                    par_next: par_next_id,
                    attrs: JSON.parse($par.attr('attrs'))
                }),
                "options": JSON.stringify({
                    showDelete: options.showDelete,
                    showImageUpload: true,
                    showPlugins: true,
                    destroyAfterSave: true,
                    touchDevice: mobile,
                    tags: [
                        {name: 'markread', desc: 'Mark as read'}
                    ]
                }),
                "after-save": 'addSavedParToDom(saveData, extraData)',
                "after-cancel": 'handleCancel(extraData)',
                "after-delete": 'handleDelete(saveData, extraData)',
                "preview-url": '/preview/' + sc.docId,
                "delete-url": '/deleteParagraph/' + sc.docId + "/" + par_id
            };
            if (options.showDelete) {
                caption = 'Edit paragraph';
                attrs["initial-text-url"] = '/getBlock/' + sc.docId + "/" + par_id;
            }
            sc.toggleEditor($par, options, attrs, caption);
        };

        sc.toggleEditor = function ($par, options, attrs, caption) {
            if ($par.children(EDITOR_CLASS_DOT).length) {
                $par.children().remove(EDITOR_CLASS_DOT);
                sc.editing = false;
            } else {
                $(EDITOR_CLASS_DOT).remove();

                var createEditor = function (attrs) {
                    var $div = $("<pareditor>", {class: EDITOR_CLASS}).attr(attrs);
                    $div.attr('tim-draggable-fixed', '');
                    if (caption) $div.attr('caption', caption);
                    $par.append($div);
                    $compile($div[0])(sc);
                    //$div = $compile($div)(sc);
                    sc.editing = true;
                };

                if (options.showDelete) {
                    $(".par.new").remove();
                }
                createEditor(attrs);

            }
        };

        sc.showQuestionById = function (questionId) {
            var question = $("#" + questionId);
            sc.showQuestion(question);

        };

        sc.showQuestion = function (question) {
            sc.json = "No data";
            sc.qId = question[0].getAttribute('id');

            http({
                url: '/getQuestionById',
                method: 'GET',
                params: {'question_id': sc.qId, 'buster': new Date().getTime()}
            })
                .success(function (data) {
                    sc.json = JSON.parse(data.questionJson);
                    $rootScope.$broadcast('changeQuestionTitle', {'title': sc.json.TITLE});
                    $rootScope.$broadcast("setPreviewJson", {
                        questionJson: sc.json,
                        questionId: sc.qId,
                        isLecturer: sc.isLecturer
                    });
                })

                .error(function () {
                    $window.console.log("There was some error creating question to database.");
                });


            sc.lectureId = -1;
            sc.inLecture = false;

            sc.$on('postLectureId', function (event, response) {
                sc.lectureId = response;
            });

            sc.$on('postInLecture', function (event, response) {
                sc.inLecture = response;
            });

            $rootScope.$broadcast('getLectureId');
            $rootScope.$broadcast('getInLecture');
            sc.showQuestionPreview = true;
            //sc.$digest();
        };

        sc.toggleNoteEditor = function ($par, options) {
            var caption = 'Edit comment';
            if (!sc.rights.can_comment) {
                return;
            }
            var url,
                data;
            if (options.isNew) {
                caption = 'Add comment';
                url = '/postNote';
                data = {
                    access: 'everyone',
                    tags: {
                        difficult: false,
                        unclear: false
                    }
                };
            } else {
                url = '/editNote';
                data = options.noteData;
                if (!data.editable) {
                    $window.alert('You cannot edit this note.');
                    return;
                }
            }
            var par_id = sc.getParId($par),
                attrs = {
                    "save-url": url,
                    "extra-data": JSON.stringify(angular.extend({
                        docId: sc.docId,
                        par: par_id
                    }, data)),
                    "options": JSON.stringify({
                        showDelete: !options.isNew,
                        showImageUpload: true,
                        showPlugins: false,
                        tags: [
                            {name: 'difficult', desc: 'The text is difficult to understand'},
                            {name: 'unclear', desc: 'The text is unclear'}
                        ],
                        choices: {
                            desc: [{
                                desc: 'Show note to:',
                                name: 'access',
                                opts: [
                                    {desc: 'Everyone', value: 'everyone'},
                                    {desc: 'Just me', value: 'justme'}
                                ]
                            }]
                        },
                        destroyAfterSave: true
                    }),
                    "after-save": 'handleNoteSave(saveData, extraData)',
                    "after-cancel": 'handleNoteCancel(extraData)',
                    "after-delete": 'handleNoteDelete(saveData, extraData)',
                    "preview-url": '/preview/' + sc.docId,
                    "delete-url": '/deleteNote',
                    "editor-text": data.content
                };
            sc.toggleEditor($par, options, attrs, caption);
        };

        sc.forEachParagraph = function (func) {
            $('.paragraphs .par').each(func);
        };

        // Event handlers

        sc.fixPageCoords = function (e) {
            if (!('pageX' in e) || (e.pageX == 0 && e.pageY == 0)) {
                e.pageX = e.originalEvent.touches[0].pageX;
                e.pageY = e.originalEvent.touches[0].pageY;
            }
            return e;
        };

        sc.onClick = function (className, func) {
            var downEvent = null;
            var downCoords = null;

            $document.on('mousedown touchstart', className, function (e) {
                downEvent = sc.fixPageCoords(e);
                downCoords = {left: downEvent.pageX, top: downEvent.pageY};
            });
            $document.on('mousemove touchmove', className, function (e) {
                if (downEvent == null)
                    return;

                var e2 = sc.fixPageCoords(e);
                if (sc.dist(downCoords, {left: e2.pageX, top: e2.pageY}) > 10) {
                    // Moved too far away, cancel the event
                    downEvent = null;
                }
            });
            $document.on('touchcancel', className, function (e) {
                console.log("cancel");
                downEvent = null;
            });
            $document.on('mouseup touchend', className, function (e) {
                console.log("tock");
                if (downEvent != null) {
                    console.log("event!");
                    if (func($(this), downEvent)) {
                        e.preventDefault();
                    }
                    downEvent = null;
                }
            });
        };

        sc.showEditWindow = function (e, $par) {
            sc.toggleParEditor($par, {showDelete: true});
        };

        sc.onClick(PAR_EDIT_BUTTON, function ($this, e) {
            var $par = $(e.target).parent().parent().parent();
            $(".par.new").remove();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.showEditWindow(e, $par, null);
            return true;
        });

        sc.onClick("#defaultEdit", function ($this, e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.defaultAction = sc.showEditWindow;
            return true;
        });

        sc.createNewPar = function () {
            return $("<div>", {class: "par new", id: 'NEW_PAR', attrs: '{}'})
                .append($("<div>", {class: "parContent"}).html('New paragraph'));
        };

        sc.showAddParagraphAbove = function (e, $par, coords) {
            var $newpar = sc.createNewPar();
            $par.before($newpar);
            sc.toggleParEditor($newpar, {showDelete: false});
        };

        sc.showAddParagraphBelow = function (e, $par, coords) {
            var $newpar = sc.createNewPar();
            $par.after($newpar);
            sc.toggleParEditor($newpar, {showDelete: false});
        };

        sc.onClick(PAR_ADD_BUTTON, function ($this, e) {
            var $par = $(e.target).parent().parent().parent();
            $(".par.new").remove();
            sc.toggleActionButtons(e, $par, false, false, null);
            var $newpar = sc.createNewPar();

            if ($(e.target).hasClass("above")) {
                $par.before($newpar);
            } else if ($(e.target).hasClass("below")) {
                $par.after($newpar);
            }

            sc.toggleParEditor($newpar, {showDelete: false});
            return true;
        });

        // Event handler for "Add question below"
        // Opens pop-up window to create question.
        sc.onClick(QUESTION_ADD_BUTTON, function ($this, e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleQuestion();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.par = $par;
            sc.$apply();
        });

        // Shows question window
        sc.toggleQuestion = function () {
            sc.questionShown = !sc.questionShown;
        };

        $.fn.slideFadeToggle = function (easing, callback) {
            return this.animate({opacity: 'toggle', height: 'toggle'}, 'fast', easing, callback);
        };

        sc.onClick("#defaultPrepend", function ($this, e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.defaultAction = sc.showAddParagraphAbove;
            return true;
        });

        sc.onClick("#defaultAppend", function ($this, e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.defaultAction = sc.showAddParagraphBelow;
            return true;
        });

        sc.doNothing = function (e, $par) {
            sc.toggleActionButtons(e, $par, false, false, null);
        };

        sc.onClick(PAR_CLOSE_BUTTON, function ($this, e) {
            var $par = $(e.target).parent().parent().parent();
            $(".par.new").remove();
            sc.toggleActionButtons(e, $par, false, false, null);
            return true;
        });

        sc.onClick("#defaultClose", function ($this, e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.defaultAction = sc.doNothing;
            return true;
        });

        sc.handleCancel = function (extraData) {
            var $par = sc.getElementByParId(extraData.par);
            if ($par.hasClass("new")) {
                $par.remove();
            }
            sc.editing = false;
        };

        sc.handleDelete = function (data, extraData) {
            var $par = sc.getElementByParId(extraData.par);
            http.defaults.headers.common.Version = data.version;
            $par.remove();
            sc.editing = false;
        };

        sc.addSavedParToDom = function (data, extraData) {
            var $par = sc.getElementByParId(extraData.par),
                len = data.texts.length;
            http.defaults.headers.common.Version = data.version;
            for (var i = len - 1; i >= 0; i--) {
                var html = data.texts[i].html;
                if ('taskId' in data.texts[i].attrs) {
                    html = $compile(html)(sc);
                }
                var $mathdiv = $.parseHTML(html);
                if ($mathdiv) sc.processMath($mathdiv[0]);
                var classes = [];
                if ('classes' in data.texts[i].attrs) {
                    classes = data.texts[i].attrs.classes;
                }
                var $newpar = $("<div>", {
                    class: ["par"].concat(classes).join(" "),
                    id: data.texts[i].id,
                    t: data.texts[i].t,
                    attrs: JSON.stringify(data.texts[i].attrs)
                })
                    .append($("<div>", {class: "parContent"}).append($mathdiv || html));
                var readClass = "unread";
                var old_t = $par.find(".readline").attr("t");
                if (i === 0 && !$par.hasClass("new") && old_t !== null && typeof old_t !== 'undefined') {
                    $par.find(".notes").appendTo($newpar);
                    if (old_t !== data.texts[i].t) {
                        readClass = "modified";
                    } else {
                        readClass = "read";
                    }
                }
                if ('taskId' in data.texts[i].attrs) {
                    var ab = $('<answerbrowser>').attr('task-id', sc.docId + '.' + data.texts[i].attrs.taskId);
                    $compile(ab[0])(sc);
                    ab.prependTo($newpar);
                }
                /*
                 var editDiv = "";
                 if (sc.rights.editable)
                 editDiv = $("<div>", {class: "editline", title: "Click to edit this paragraph"});
                 */
                $par.after($newpar.append($("<div>",
                        {class: "readline " + readClass, title: "Click to mark this paragraph as read", t: old_t}),
                    $("<div>", {class: "editline", title: "Click to edit this paragraph"})));

                if (extraData.tags) {
                    if (extraData.tags['markread']) {
                        var $newread = $newpar.find("div.readline");
                        sc.markParRead($newread, data.texts[i].id);
                    }
                }
            }
            $par.remove();
            sc.editing = false;
        };

        sc.markParRead = function ($this, par_id) {
            var oldClass = $this.attr("class");
            $this.attr("class", "readline read");
            http.put('/read/' + sc.docId + '/' + par_id + '?_=' + Date.now())
                .success(function (data, status, headers, config) {
                    // No need to do anything here
                }).error(function () {
                    $window.alert('Could not save the read marking.');
                    $this.attr("class", oldClass);
                });
            return true;
        };

        sc.onClick(".readline", function ($this, e) {
            var par_id = sc.getParId($this.parents('.par'));
            return sc.markParRead($this, par_id);
        });


        sc.onClick(".editline", function ($this, e) {
            $(".actionButtons").remove();
            var $par = $this.parent();
            var coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top};
            return sc.showOptionsWindow(e, $par, coords);
        });

        sc.showNoteWindow = function (e, $par) {
            sc.toggleNoteEditor($par, {isNew: true});
        };

        sc.onClick(NOTE_ADD_BUTTON, function ($this, e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.showNoteWindow(e, $par, null);
            return true;
        });

        sc.onClick("#defaultAdd", function ($this, e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.defaultAction = sc.showNoteWindow;
            return true;
        });

        sc.handleNoteCancel = function () {
            sc.editing = false;
        };

        sc.handleNoteDelete = function () {
            sc.getNotes();
            sc.editing = false;
        };

        sc.handleNoteSave = function () {
            sc.getNotes();
            sc.editing = false;
        };

        sc.onClick('.paragraphs .parContent', function ($this, e) {
            if (sc.editing) {
                return false;
            }

            sc.$apply();

            var $target = $(e.target);
            var tag = $target.prop("tagName");

            // Don't show paragraph menu on these specific tags or class
            var ignoredTags = ['BUTTON', 'INPUT', 'TEXTAREA', 'A'];
            if (ignoredTags.indexOf(tag) > -1 || $target.parents('.no-popup-menu').length > 0) {
                return false;
            }

            var $par = $this.parent();
            var coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top};
            var toggle1 = $par.find(".actionButtons").length === 0;
            var toggle2 = $par.hasClass("lightselect");

            $(".par.selected").removeClass("selected");
            $(".par.lightselect").removeClass("lightselect");
            $(".actionButtons").remove();
            sc.toggleActionButtons(e, $par, toggle1, toggle2, coords);
            return true;
        });

        sc.onClick(".noteContent", function ($this, e) {
            sc.toggleNoteEditor($this.parent().parent().parent(), {isNew: false, noteData: $this.parent().data()});
            return true;
        });

        sc.onClick(".questionAdded", function ($this, e) {
            var question = $this;
            sc.showQuestion(question);
            sc.par = ($(question).parent().parent());
        });

        // Note-related functions

        sc.showOptionsWindow = function (e, $par, coords) {
            //var default_width = $par.outerWidth() / 16;
            var button_width = 130;
            //var button_width = $par.outerWidth() / 4 - 1.7 * default_width;
            var $actionDiv = $("<div>", {class: 'actionButtons'});
            if (sc.rights.can_comment) {
                var $span = $("<span>", {class: ACTION_BUTTON_ROW_CLASS});
                $span.append($("<button>", {class: NOTE_ADD_BUTTON_CLASS, text: 'Comment/note', width: button_width}));
                $span.append($("<input>", {
                    class: DEFAULT_CHECKBOX_CLASS,
                    type: 'checkbox',
                    'ng-click': 'updateSelection(0)',
                    'ng-model': 'defaults[0]'
                }));
                $actionDiv.append($span);
            }
            if (sc.rights.editable) {
                var $span = $("<span>", {class: ACTION_BUTTON_ROW_CLASS});
                $span.append($("<button>", {class: PAR_EDIT_BUTTON_CLASS, text: 'Edit', width: button_width}));
                $span.append($("<input>", {
                    class: DEFAULT_CHECKBOX_CLASS,
                    type: 'checkbox',
                    'ng-click': 'updateSelection(1)',
                    'ng-model': 'defaults[1]'
                }));
                $actionDiv.append($span);

                var $span = $("<span>", {class: ACTION_BUTTON_ROW_CLASS});
                $span.append($("<button>", {
                    class: PAR_ADD_BUTTON_CLASS + ' above',
                    text: 'Add paragraph above',
                    width: button_width
                }));
                $span.append($("<input>", {
                    class: DEFAULT_CHECKBOX_CLASS,
                    type: 'checkbox',
                    'ng-click': 'updateSelection(2)',
                    'ng-model': 'defaults[2]'
                }));
                $actionDiv.append($span);

                var $span = $("<span>", {class: ACTION_BUTTON_ROW_CLASS});
                $span.append($("<button>", {
                    class: PAR_ADD_BUTTON_CLASS + ' below',
                    text: 'Add paragraph below',
                    width: button_width
                }));

                $span.append($("<input>", {
                    class: DEFAULT_CHECKBOX_CLASS,
                    type: 'checkbox',
                    'ng-click': 'updateSelection(3)',
                    'ng-model': 'defaults[3]'
                }));
                $actionDiv.append($span);

                if (sc.lectureMode) {

                    var $span = $("<span>", {class: ACTION_BUTTON_ROW_CLASS});
                    $span.append($("<button>", {
                        class: QUESTION_ADD_BUTTON_CLASS,
                        text: 'Create question',
                        width: button_width
                    }));
                    $span.append($("<input>", {
                        class: DEFAULT_CHECKBOX_CLASS,
                        type: 'checkbox',
                        'ng-click': 'updateSelection(4)',
                        'ng-model': 'defaults[4]'
                    }));
                    $actionDiv.append($span);
                }

                var $span = $("<span>", {class: ACTION_BUTTON_ROW_CLASS});
                $span.append($("<button>", {class: PAR_CLOSE_BUTTON_CLASS, text: 'Close menu', width: button_width}));
                $span.append($("<input>", {
                    class: DEFAULT_CHECKBOX_CLASS,
                    type: 'checkbox',
                    'ng-click': 'updateSelection(5)',
                    'ng-model': 'defaults[5]'
                }));
                $actionDiv.append($span);

            }
            /*
             if ('ontouchstart' in window || navigator.msMaxTouchPoints) {
             coords = {left: 0, top: 0};
             }*/
            ;
            $actionDiv.offset(coords);
            $actionDiv.css('position', 'absolute'); // IE needs this
            $actionDiv.attr('tim-draggable-fixed', '');
            $actionDiv = $compile($actionDiv)(sc);
            $par.prepend($actionDiv);


            var element = $('.actionButtons');
            var viewport = {};
            viewport.top = $(window).scrollTop();
            viewport.bottom = viewport.top + $(window).height();
            var bounds = {};
            bounds.top = element.offset().top;
            bounds.bottom = bounds.top + element.outerHeight();
            var y = $(window).scrollTop();
            if (bounds.bottom > viewport.bottom) y += (bounds.bottom - viewport.bottom);
            else if (bounds.top < viewport.top) y += (bounds.top - viewport.top);
            $('html, body').animate({
                scrollTop: y
            }, 500);
        };

        sc.dist = function (coords1, coords2) {
            return Math.sqrt(Math.pow(coords2.left - coords1.left, 2) + Math.pow(coords2.top - coords1.top, 2));
        };

        sc.toggleActionButtons = function (e, $par, toggle1, toggle2, coords) {
            if (!sc.rights.editable && !sc.rights.can_comment) {
                return;
            }
            if (toggle2) {
                // Clicked twice successively
                var clicktime = new Date().getTime() - sc.lastclicktime;
                var clickdelta = sc.dist(coords, sc.lastclickplace);
                $par.addClass("selected");

                if (clickdelta > 10) {
                    // Selecting text
                    $par.removeClass("selected");
                    $par.removeClass("lightselect");
                }
                else if (clicktime < 500) {
                    // Double click
                    sc.defaultAction(e, $par, coords);
                }
                else {
                    // Two clicks
                    sc.showOptionsWindow(e, $par, coords);
                }
            } else if (toggle1) {
                // Clicked once
                $par.addClass("lightselect");
                sc.lastclicktime = new Date().getTime();
                sc.lastclickplace = coords;
            } else {
                $window.console.log("This line is new: " + $par);
                $par.children().remove(".actionButtons");
                $par.removeClass("selected");
                $par.removeClass("lightselect");
            }
        };

        sc.getNoteHtml = function (notes) {
            var $noteDiv = $("<div>", {class: 'notes'});
            for (var i = 0; i < notes.length; i++) {
                var classes = ["note"];
                for (var j = 0; j < sc.noteClassAttributes.length; j++) {
                    if (notes[i][sc.noteClassAttributes[j]] || notes[i].tags[sc.noteClassAttributes[j]]) {
                        classes.push(sc.noteClassAttributes[j]);
                    }
                }
                $noteDiv.append($("<div>", {class: classes.join(" ")})
                    .data(notes[i])
                    .append($("<div>", {class: 'noteContent', html: notes[i].html})));
            }
            return $noteDiv;
        };

        sc.getSpeakerNoteHtml = function (notes) {
            $(notes).parent().append($(notes).clone());
            $(notes).removeClass('speaker').addClass('notes');
            var $noteDiv = $("<div>", {class: 'notes'});
            for (var i = 0; i < notes.length; i++) {
                var classes = ["note"];
                for (var j = 0; j < sc.noteClassAttributes.length; j++) {
                    if (notes[i][sc.noteClassAttributes[j]] || notes[i].tags[sc.noteClassAttributes[j]]) {
                        classes.push(sc.noteClassAttributes[j]);
                    }
                }
                $noteDiv.append($("<div>", {class: classes.join(" ")})
                    .data(notes[i])
                    .append($("<div>", {class: 'noteContent', html: notes[i].content})));
            }

            return $noteDiv;
            /*
             var $noteDiv = $("<div>", {class: 'notes'});
             $(notes).removeClass('speaker').addClass('notes');
             return notes;
             */
        };

        sc.getQuestionHtml = function (questions) {
            var questionImage = '../../../static/images/show-question-icon.png';
            var $questionsDiv = $("<div>", {class: 'questions'});

            // TODO: Think better way to get the ID of question.
            for (var i = 0; i < questions.length; i++) {
                var img = new Image(30, 30);
                img.src = questionImage;
                img.title = questions[i].question_title;
                var $questionDiv = $("<span>", {
                    class: 'questionAdded', html: img, id: questions[i].question_id
                });
                $questionsDiv.append($questionDiv);
            }
            return $questionsDiv;
        };


        sc.getQuestions = function () {
            var rn = "?_=" + Date.now();

            http.get('/questions/' + sc.docId + rn)
                .success(function (data) {
                    var pars = {};
                    var questionCount = data.length;
                    for (var i = 0; i < questionCount; i++) {
                        var pi = data[i].par_id;
                        if (!(pi in pars)) {
                            pars[pi] = {questions: []};
                        }

                        pars[pi].questions.push(data[i]);
                    }

                    Object.keys(pars).forEach(function (par_id, index) {
                        var $par = sc.getElementByParId(par_id);
                        $par.find(".questions").remove();
                        var $questionsDiv = sc.getQuestionHtml(pars[par_id].questions);
                        $par.append($questionsDiv);
                    });
                });
        };


        sc.getNotes = function () {
            var rn = "?_=" + Date.now();

            http.get('/notes/' + sc.docId + rn).success(function (data) {
                $('.notes').remove();
                var pars = {};

                var noteCount = data.length;
                for (var i = 0; i < noteCount; i++) {
                    var pi = data[i].par_id;
                    if (!(pi in pars)) {
                        pars[pi] = {notes: []};

                    }
                    if (!('notes' in pars[pi])) {
                        pars[pi].notes = [];
                    }
                    pars[pi].notes.push(data[i]);
                }
                Object.keys(pars).forEach(function (par_id, index) {
                    var $par = sc.getElementByParId(par_id);
                    var $notediv = sc.getNoteHtml(pars[par_id].notes);
                    $par.append($notediv);
                    sc.processAllMath($par);
                });
            }).error(function (data, status, headers, config) {
                $window.alert("Could not fetch notes.");
            });
        };

        sc.getReadPars = function () {
            if (!sc.rights.can_mark_as_read) {
                return;
            }
            var rn = "?_=" + Date.now();
            http.get('/read/' + sc.docId + rn).success(function (data) {
                var readCount = data.length;
                $('.readline').remove();
                var pars = {};
                for (var i = 0; i < readCount; i++) {
                    var readPar = data[i];
                    var pi = data[i].par_id;
                    if (!(pi in pars)) {
                        pars[pi] = {par_hash: data[i].par_hash};
                    }
                }
                sc.forEachParagraph(function (index, elem) {
                    var $par = $(elem);
                    var hash = $par.attr('t');
                    var par_id = $par.attr('id');
                    var classes = ["readline"];
                    var curr_hash = null;
                    if (par_id in pars) {
                        var status = 'read';
                        if (hash !== pars[par_id].par_hash) {
                            status = 'modified';
                        }
                        classes.push(status);
                        curr_hash = pars[par_id].par_hash;
                    } else {
                        classes.push("unread");
                    }
                    var $div = $("<div>", {
                        class: classes.join(" "),
                        title: "Click to mark this paragraph as read",
                        t: curr_hash
                    });
                    $(this).append($div);
                });
            }).error(function () {
                $window.alert("Could not fetch reading info.");
            });
        };

        sc.getEditPars = function () {
            sc.forEachParagraph(function (index, elem) {
                var $div = $("<div>", {class: "editline", title: "Click to edit this paragraph"});
                $(this).append($div);
            });
        };

        sc.markAllAsRead = function () {
            http.put('/read/' + sc.docId + '?_=' + Date.now())
                .success(function (data, status, headers, config) {
                    $('.readline').attr("class", "readline read");
                }).error(function (data, status, headers, config) {
                    $window.alert('Could not mark the document as read.');
                });
        };

        sc.setHeaderLinks = function () {
            $(".par h1, .par h2, .par h3, .par h4, .par h5, .par h6").each(function () {
                var $par = $(this).parent();
                $par.append($("<a>", {
                    text: '#',
                    href: '#' + $(this).attr('id'),
                    class: 'headerlink',
                    title: 'Permanent link'
                }));
            });
        };

        // Index-related functions

        sc.totext = function (str) {
            if (str.indexOf('{') > 0) {
                return str.substring(0, str.indexOf('{')).trim();
            }
            return str;
        };

        sc.tolink = function (str) {
            if (str.indexOf('{') >= 0 && str.indexOf('}') > 0) {
                var ob = str.indexOf('{');
                var cb = str.indexOf('}');
                return str.substring(ob + 1, cb);
            }
            return "#" + str.replace(/^(\d)+(\.\d+)*\.? /, "").replace(/[^\d\wÃ¥Ã¤Ã¶\.\- ]/gi, "").trim().replace(/ +/g, '-').toLowerCase();
        };

        sc.findIndexLevel = function (str) {
            for (var i = 0; i < str.length; i++) {
                if (str.charAt(i) !== '#') {
                    return i;
                }
            }

            return 0;
        };

        sc.getIndex = function () {
            http.get('/index/' + sc.docId).success(function (data) {
                var parentEntry = null;
                sc.indexTable = [];

                for (var i = 0; i < data.length; i++) {
                    var lvl = sc.findIndexLevel(data[i]);
                    if (lvl < 1 || lvl > 3) {
                        continue;
                    }

                    var astyle = "a" + lvl;
                    var txt = data[i].substr(lvl);
                    txt = txt.trim().replace(/\\#/g, "#");
                    var entry = {
                        text: sc.totext(txt),
                        target: sc.tolink(txt),
                        style: astyle,
                        level: lvl,
                        items: [],
                        state: ""
                    };

                    if (lvl === 1) {
                        if (parentEntry !== null) {
                            if ("items" in parentEntry && parentEntry.items.length > 0) {
                                parentEntry.state = 'col';
                            }
                            sc.indexTable.push(parentEntry);
                        }

                        parentEntry = entry;
                    }
                    else if (parentEntry !== null) {
                        if (!("items" in parentEntry)) {
                            // For IE
                            parentEntry.items = [];
                        }
                        parentEntry.items.push(entry);
                    }

                }

                if (parentEntry !== null) {
                    if (parentEntry.items.length > 0) {
                        parentEntry.state = 'col';
                    }
                    sc.indexTable.push(parentEntry);
                }
            }).error(function () {
                $window.alert("Could not fetch index entries.");
            });
        };

        sc.invertState = function (state) {
            if (state === 'exp') {
                return 'col';
            }
            if (state === 'col') {
                return 'exp';
            }
            return state;
        };

        sc.clearSelection = function () {
            if ($document.selection) {
                $document.selection.empty();
            }
            else if ($window.getSelection) {
                $window.getSelection().removeAllRanges();
            }
        };

        sc.invertStateClearSelection = function (event, state) {
            if (event.which !== 1) {
                // Listen only to the left mouse button
                return state;
            }
            if (event.target.className === 'a2' || event.target.className === 'a3') {
                // Do not collapse/expand if a subentry is clicked
                return state;
            }

            var newState = sc.invertState(state);
            if (newState !== state) {
                sc.clearSelection();
            }
            return newState;
        };

        if (sc.lectureMode) {
            sc.$on("getQuestions", function () {
                if (sc.firstTimeQuestions) {
                    sc.getQuestions();
                    sc.firstTimeQuestions = false;
                }
            });

            sc.$on("closeQuestionPreview", function () {
                sc.showQuestionPreview = false;
                //sc.clearQuestion();
            });
        }

        sc.editorFunctions = [sc.showNoteWindow, sc.showEditWindow, sc.showAddParagraphAbove,
            sc.showAddParagraphBelow, sc.toggleQuestion, sc.doNothing];

        // Load index, notes and read markings
        sc.setHeaderLinks();
        sc.indexTable = [];
        sc.getIndex();
        sc.getNotes();
        sc.getReadPars();


        // Tässä jos lisää bindiin 'mousedown', scrollaus menua avattaessa ei toimi Androidilla
        $('body,html').bind('scroll wheel DOMMouseScroll mousewheel', function (e) {
            if (e.which > 0 || e.type == "mousedown" || e.type == "mousewheel") {
                $("html,body").stop();
            }
        });

        if (sc.rights.editable) {
            sc.onClick(".addBottom", function ($this, e) {
                $(".actionButtons").remove();
                var $par = $('#pars').children().last();
                var coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top - 1000};
                return sc.showAddParagraphBelow(e, $par, coords);
            });
            //sc.getEditPars();
        }
        sc.processAllMath($('body'));
        sc.defaultAction = sc.showOptionsWindow;
        sc.onClick(".showContent", function ($this, e) {

            var $pars = $('#pars');
            if ($pars.length > 0) {
                if ($pars.css('display') == 'none') {
                    $pars.css('display', '');
                    $('.showContent').text('Hide content');
                } else {
                    $pars.css('display', 'none');
                    $('.showContent').text('Show content');
                }

                return;
            }

            var $loading = $('<div>', {class: 'par', id: 'loading'});
            $loading.append($('<img>', {src: "/static/images/loading.gif"}));
            $('.paragraphs').append($loading);
            $.ajax({
                type: 'GET', url: '/view_content/' + docName,
                dataType: "html",
                processData: false,
                success: function (data) {
                    var $loading = $('#loading');
                    $loading.remove();
                    $('.paragraphs').append($compile(data)(sc));
                    sc.getIndex();
                    sc.getNotes();
                    sc.getReadPars();
                    sc.processAllMath($('body'));
                    /*
                    if (sc.rights.editable) {
                        sc.getEditPars();
                    }*/
                    if (sc.lectureMode) {
                        sc.getQuestions();
                    }
                    $('.showContent').text('Hide content');
                },
                error: function () {
                    var $loading = $('#loading');
                    $loading.remove();
                    console.log("Virhe");
                }
            });
        });
    }
])
;

/**
 * Controller for creating and editing questions
 * @module questionController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

timApp.controller("QuestionController", ['$scope', '$http', '$window', '$rootScope', function (scope, http, $window, $rootScope) {
    "use strict";
    $(function () {
        $('#calendarStart').datepicker({dateFormat: 'dd.m.yy'});
    });

    scope.putBackQuotations = function (x) {
        return x.replace(/&quot;/g, '"');
    };

    scope.settings = $window.settings;

    scope.setTime = function () {
        console.log(scope.settings);
        scope.question.timeLimit = {hours: 0, minutes: 0, seconds: 30};
        if (scope.settings['timelimit'] && scope.settings['timelimit'] > 0) {
            var time = scope.settings['timelimit'];
            if (time > 3600) {
                scope.question.timeLimit.hours = Math.floor(time / 3600);
            } else {
                scope.question.timeLimit.hours = 0;
            }
            if (time > 60) {
                scope.question.timeLimit.minutes = Math.floor(time / 60);
                time = time % 60;
            } else {
                scope.question.timeLimit.minutes = 0;
            }
            if (time > 0) {
                scope.question.timeLimit.seconds = time;
            } else {
                scope.question.timeLimit.seconds = 0;
            }
        }
    };

    scope.$on("editQuestion", function (event, data) {
            var id = data.question_id;
            var json = data.json;

            if (id) scope.question.question_id = id;
            if (json["TITLE"]) scope.question.title = scope.putBackQuotations(json["TITLE"]);
            if (json["QUESTION"]) scope.question.question = scope.putBackQuotations(json["QUESTION"]);
            if (json["TYPE"]) scope.question.type = json["TYPE"];
            if (json["MATRIXTYPE"]) scope.question.matrixType = json["MATRIXTYPE"];
            if (json["ANSWERFIELDTYPE"]) scope.question.answerFieldType = (json["ANSWERFIELDTYPE"]);


            var jsonData = json["DATA"];
            var jsonHeaders = jsonData["HEADERS"];
            var jsonRows = jsonData["ROWS"];

            var columnHeaders = [];
            for (var i = 0; i < jsonHeaders.length; i++) {
                columnHeaders[i] = {
                    id: i,
                    type: jsonHeaders[i].type,
                    text: scope.putBackQuotations(jsonHeaders[i].text)
                };
            }
            scope.columnHeaders = columnHeaders;

            var rows = [];
            for (var i = 0; i < jsonRows.length; i++) {
                rows[i] = {
                    id: jsonRows[i].id,
                    text: scope.putBackQuotations(jsonRows[i].text),
                    type: jsonRows[i].type,
                    value: jsonRows[i].value
                };


                var jsonColumns = jsonRows[i]["COLUMNS"];
                var columns = [];
                for (var j = 0; j < jsonColumns.length; j++) {
                    columns[j] = {
                        id: j,
                        rowId: i,
                        text: jsonColumns[j].text,
                        //points: jsonColumns[j].points,
                        type: jsonColumns[j].type,
                        answerFiledType: jsonColumns[j].answerFieldType
                    };
                    if (jsonColumns[j].points) {
                        columns[j].points = jsonColumns[j].points;
                    } else {
                        columns[j].points = "";
                    }

                }

                rows[i].columns = columns;

            }
            scope.rows = rows;

            if (json["TIMELIMIT"] && json["TIMELIMIT"] > 0) {
                var time = json["TIMELIMIT"];
                scope.question.endTimeSelected = true;
                if (time > 3600) {
                    scope.question.timeLimit.hours = Math.floor(time / 3600);
                    time = time % 3600;
                } else {
                    scope.question.timeLimit.hours = 0;
                }

                if (time > 60) {
                    scope.question.timeLimit.minutes = Math.floor(time / 60);
                    time = time % 60;
                } else {
                    scope.question.timeLimit.minutes = 0;
                }

                if (time > 0) {
                    scope.question.timeLimit.seconds = time;
                } else {
                    scope.question.timeLimit.seconds = 0;
                }

            } else {
                scope.question.endTimeSelected = false;
            }

            scope.toggleQuestion();

        }
    );

    scope.question = {
        title: "",
        question: "",
        matrixType: "",
        answerFieldType: "",
        timeLimit: {hours: 0, minutes: 0, seconds: 30},
        endTimeSelected: true
    };


    scope.rows = [];
    scope.columns = [];
    scope.columnHeaders = [];
    scope.answerDirection = "horizontal";
    scope.setTime();
    scope.error_message = "";
    scope.answerFieldTypes = [

        {label: "Text area", value: "textArea"},
        {label: "Radio Button horizontal", value: "radiobutton-horizontal"},
        {label: "Checkbox", value: "checkbox"}
    ];

    /**
     * A function for creating a matrix.
     * @memberof module:questionController
     * @param rowsCount The number of rows to create for the matrix.
     * @param columnsCount The number of columns to create for new matrix.
     * @param type The answer type of the matrix.
     */
    scope.createMatrix = function (type) {
        var rowsCount = 0;
        var columnsCount = 0;
        if (type === 'matrix' || type === 'true-false') {
            rowsCount = 2;
            columnsCount = 2;
        } else {
            rowsCount = 2;
            columnsCount = 1;
        }

        if (scope.rows.length < 1) {
            for (var i = 0; i < rowsCount; i++) {
                scope.addRow(i);
            }
        }


        if (type === 'radio-vertical' || 'true-false') scope.question.answerFieldType = 'radio';
        if (type === 'checkbox-vertical') scope.question.answerFieldType = 'checkbox';
        if (type === 'matrix') {
            scope.question.answerFieldType = 'matrix';
        }

        for (var i = 0; i < scope.rows.length; i++) {
            if (scope.rows[i].columns.length > columnsCount) scope.rows[i].columns.splice(columnsCount, scope.rows[i].columns.length);
            if (scope.rows[i].columns.length < columnsCount) scope.addCol(scope.rows[0].columns.length);
            for (var j = 0; j < scope.rows[i].columns.length; j++) {
                scope.rows[j].columns.answerFieldType = scope.question.answerFieldType;
            }
        }

        scope.columnHeaders = [];
        if (type === 'matrix') {
            for (var i = 0; i < scope.rows[0].columns.length; i++) {
                scope.columnHeaders[i] = {
                    id: i,
                    text: "",
                    type: 'header'
                };
            }
        }


        /*        if (scope.question.type != type || scope.rows.length <= 0) {
         scope.question.type = type;

         if (type === 'radio-vertical' || 'true-false') scope.question.answerFieldType = 'radio';
         else if (type === 'checkbox-vertical') scope.question.answerFieldType = 'checkbox';
         else if (type === 'matrix') scope.question.answerFieldType = 'matrix';



         var i;
         if (scope.rows.length > 0) {
         scope.columnHeaders.splice(0, scope.columnHeaders.length);
         for (i = 0; i < scope.rows.length; i++) {
         scope.rows[i].columns.splice(0, scope.rows[i].columns.length);
         }

         for (i = 0; i < columnsCount; i++) {
         scope.addCol(i);
         }

         } else {

         var columnHeaders = [];
         for (i = 0; i < rowsCount; i++) {
         var columns = [];
         columnHeaders = [];
         for (var j = 0; j < columnsCount; j++) {
         columnHeaders.push({type: "header", id: j, text: ""});
         columns[j] = {
         id: j,
         rowId: i,
         text: '',
         points: '',
         type: "answer",
         answerFiledType: scope.question.answerFieldType
         };
         }
         scope.rows[i] = {
         id: i,
         text: '',
         type: 'question',
         value: '',
         columns: columns
         };

         }
         scope.columnHeaders = columnHeaders;

         }
         scope.columnHeaders.splice(columnsCount, scope.columnHeaders.length);
         }*/
    };

    /*    */
    /**
     * A function handling rowClick
     * @memberof module:questionController
     * @param index FILL WITH SUITABLE TEXT
     */
    /*
     scope.rowClick = function (index) {
     scope.addRow(index);
     };*/

    /**
     * A function to add a column to an existing matrix.
     * @memberof module:questionController
     * @param loc The index in the matrix where to add the new column.
     */
    scope.addCol = function (loc) {
        var location = loc;
        if (loc === -1) {
            location = scope.rows[0].columns.length;
            loc = scope.rows[0].columns.length;
        }
        scope.columnHeaders.splice(loc, 0, {type: "header", id: loc, text: ""});
        //add new column to columns
        for (var i = 0; i < scope.rows.length; i++) {

            scope.rows[i].columns.splice(loc, 0, {
                id: location,
                rowId: i,
                text: '',
                points: '',
                type: "answer",
                answerFiledType: scope.question.answerFieldType
            });
        }


    };

    /**
     * The function adds a row to an existing matrix
     * @memberof module:questionController
     * @param loc The index in the matrix where to add the new row.
     */
    scope.addRow = function (loc) {

        scope.CreateColumnsForRow = function (location) {
            var columns = [];
            if (scope.rows.length > 0) {
                for (var j = 0; j < scope.rows[0].columns.length; j++) {
                    columns[j] = {
                        id: j,
                        rowId: location,
                        type: "answer",
                        value: '',
                        answerFiledType: scope.question.answerFieldType,
                        points: ""
                    };

                }
            }
            return columns;
        };

        var location = loc;
        if (loc === -1) {
            location = scope.rows.length;
            loc = scope.rows.length;
        }

        var columns = scope.CreateColumnsForRow(location);
        scope.rows.splice(loc, 0,
            {
                id: location,
                text: "",
                type: "question",
                value: "",
                columns: columns
            });

        for (var i = 0; i < scope.rows.length; i++) {
            scope.rows[i].id = i;
        }


    };

    /**
     * A function to delete a row from a matrix.
     * @memberof module:questionController
     * @param indexToBeDeleted The index of the row to be deleted.
     */
    scope.delRow = function (indexToBeDeleted) {
        scope.error_message = "";
        if (scope.rows.length > 1) {
            if (indexToBeDeleted === -1) {
                scope.rows.splice(-1, 1);
            }
            else {
                scope.rows.splice(indexToBeDeleted, 1);
            }
        } else {
            scope.errorize("", "You cannot have an empty table.");
        }

    };

    /**
     * A function to delete a column from a matrix.
     * @memberof module:questionController
     * @param indexToBeDeleted Index of the column to be deleted.
     */
    scope.delCol = function (indexToBeDeleted) {
        for (var i = 0; i < scope.rows.length; i++) {
            if (indexToBeDeleted === -1) {
                scope.rows[i].columns.splice(-1, 1);
            }
            else {
                scope.rows[i].columns.splice(indexToBeDeleted, 1);
            }
        }
        if (indexToBeDeleted === -1) {
            scope.columnHeaders.splice(-1, 1);
        }
        else {
            scope.columnHeaders.splice(indexToBeDeleted, 1);
        }

    };

    /**
     * A function to reset the question values.
     * @memberof module:questionController
     */
    scope.clearQuestion = function () {
        scope.question = {
            title: "",
            question: "",
            matrixType: "",
            answerFieldType: "",
            endTimeSelected: true
        };
        scope.setTime();

        scope.rows = [];
        scope.answer = "";
        scope.columnHeaders = [];
        //scope.toggleQuestion();
    };

    /**
     * A function to close question edition form.
     * @memberof module:questionController
     */
    scope.close = function () {
        scope.removeErrors();
        scope.clearQuestion();
        if (scope.questionShown) scope.toggleQuestion();
    };

    /**
     * The function replaces linebreaks with HTML code.
     * @memberof module:questionController
     * @param val The input string
     * @returns {*} The reformatted line.
     */
    scope.replaceLinebreaksWithHTML = function (val) {
        var output = val.replace(/(?:\r\n|\r|\n)/g, '<br />');
        output = output.replace(/"/g, '&quot;');
        return output.replace(/\\/g, "\\\\");
    };

    /**
     * The function to highlight the source of the errors for a given ID.
     * @memberof module:questionController
     * @param div_val ID of the element to be errorized.
     * @param error_text Description of the occured error.
     */
    scope.errorize = function (div_val, error_text) {
        angular.element("#" + div_val).css('border', "1px solid red");
        if (error_text.length > 0) {
            scope.error_message += error_text + "<br />";
        }
    };

    /**
     * The function to highlight the source of the errors for a given class.
     * @memberof module:questionController
     * @param div_val Class of the element to be errorized.
     * @param error_text Description of the occured error.
     */
    scope.errorizeClass = function (div_val, error_text) {
        angular.element("." + div_val).css('border', "1px solid red");
        if (error_text.length > 0) {
            scope.error_message += error_text + "<br />";
        }
    };

    /**
     * Removes border of a given element.
     * @memberof module:questionController
     * @param element ID of the field whose border will be removed.
     */
    scope.defInputStyle = function (element) {
        if (element !== null || !element.isDefined) {
            angular.element("#" + element).css("border", "");
        }
    };

    /**
     * Calls defInputStyle for all the form elements.
     * @memberof module:questionController
     */
    scope.removeErrors = function () {
        scope.error_message = "";
        var elementsToRemoveErrorsFrom = [
            "questionName",
            "questionTiming",
            "questionStart",
            "questionTimer",
            "qType",
            "matrix",
            "durationSec",
            "durationHour",
            "durationMin",
            "durationDiv"
        ];
        for (var i = 0; i < elementsToRemoveErrorsFrom.length; i++) {
            if (elementsToRemoveErrorsFrom[i] !== undefined) {
                scope.defInputStyle(elementsToRemoveErrorsFrom[i]);
            }
        }
        angular.element(".rowHeading").css("border", "");
    };

    /**
     * Function for checking if the row headings are empty.
     * @memberof module:questionController
     * @param rows The array of rows to be checked.
     * @returns {boolean} Whether or not the row headings are empty.
     */
    scope.rowHeadingsEmpty = function (rows) {
        for (var i = 0; i < rows.length; i++) {
            if (rows[i].text === "" || rows[i].text === null) {
                return true;
            }
        }
        return false;
    };

    /**
     * Checks if a value is a positive number and makes the appropriate errors if this is not the case.
     * @memberof module:questionController
     * @param element The value to be checked.
     * @param val The id of the value, which is used in case the number is not positive.
     */
    scope.isPositiveNumber = function (element, val) {
        if (element === "" || isNaN(element) || element < 0) {
            scope.errorize(val, "Number has to be positive.");
        }
    };

    /**
     * Validates and saves the question into the database.
     * @memberof module:questionController
     */
    scope.createQuestion = function (question, type, ask) {

        scope.removeErrors();
        if (scope.question.question === undefined || scope.question.question.trim().length === 0 || scope.question.title === undefined || scope.question.title.trim().length === 0) {
            scope.errorize("questionName", "Both title and question are required for a question.");
        }
        if (scope.question.type === undefined) {
            scope.errorize("qType", "Question type must be selected.");
        } else if (scope.question.type === "matrix" && (scope.question.matrixType === undefined || scope.question.matrixType === "")) {
            scope.errorize("check", "Answer type must be selected.");
        } else if ((scope.question.type === "radio-vertical" ||
            scope.question.type === "checkbox-vertical" ||
            scope.question.type === "true-false") &&
            scope.rowHeadingsEmpty(scope.rows)) {
            scope.errorizeClass("rowHeading", "All rows must be filled in.");
        }
        if (scope.rows.length > 0) {
            if ((scope.question.type === "radio-vertical" || scope.question.type === "checkbox-vertical") && scope.rows.length < 2) {
                scope.errorize("matrix", "You must have at least two choices.");
            }
        } else if (scope.question.type !== undefined) {
            scope.errorize("matrix", "You must have at least one row.");
        }
        var timeLimit = "";
        if (scope.question.endTimeSelected) {
            if (scope.question.timeLimit.hours === "") {
                scope.question.timeLimit.hours = 0;
            }
            if (scope.question.timeLimit.minutes === "") {
                scope.question.timeLimit.minutes = 0;
            }
            if (scope.question.timeLimit.seconds === "") {
                scope.question.timeLimit.seconds = 0;
            }
            scope.isPositiveNumber(scope.question.timeLimit.hours, "durationHour");
            scope.isPositiveNumber(scope.question.timeLimit.minutes, "durationMin");
            scope.isPositiveNumber(scope.question.timeLimit.seconds, "durationSec");
            timeLimit = 0;
            timeLimit = parseInt(timeLimit) + parseInt(scope.question.timeLimit.seconds);
            if (scope.question.timeLimit.hours) {
                timeLimit = parseInt(timeLimit) + (scope.question.timeLimit.hours * 60 * 60);
            }
            if (scope.question.timeLimit.minutes) {
                timeLimit = parseInt(timeLimit) + (scope.question.timeLimit.minutes * 60);
            }
            if (timeLimit <= 0) {
                scope.errorize("durationDiv", "Please enter a duration greater then zero or for unending question uncheck the duration box.");
            } else {
                $window.settings['timelimit'] = timeLimit;
                setsetting('timelimit', timeLimit);
            }
        } else {
            timeLimit = "";
        }

        if (scope.error_message !== "") {
            return;
        }

        if (scope.question.type === 'matrix') {

            if (scope.question.matrixType === "radiobutton-horizontal" || scope.question.matrixType === "radiobutton-vertical") {
                scope.question.answerFieldType = "radio";
            }

            if (scope.question.matrixType === "textArea") {
                scope.question.answerFieldType = "text";
            }
            if (scope.question.matrixType === "checkbox") {
                scope.question.answerFieldType = "checkbox";
            }
        }
        var doc_id = scope.docId;
        var $par = scope.par;
        var par_id = scope.getParId($par);

        //TODO use  JSON.stringify


        scope.question.question = scope.replaceLinebreaksWithHTML(scope.question.question);
        scope.question.title = scope.replaceLinebreaksWithHTML(scope.question.title);

        var questionJson = '{"QUESTION": "' + scope.question.question + '", "TITLE": "' + scope.question.title + '", "TYPE": "' + scope.question.type + '", "ANSWERFIELDTYPE": "' + scope.question.answerFieldType + '", "MATRIXTYPE": "' + scope.question.matrixType + '", "TIMELIMIT": "' + timeLimit + '", "DATA": {';

        var testJson = JSON.stringify(scope.question);
        testJson += JSON.stringify(scope.columnHeaders);

        questionJson += '"HEADERS" : [';
        var i;
        if (scope.question.type === "matrix") {
            for (i = 0; i < scope.columnHeaders.length; i++) {
                questionJson += '{';
                questionJson += '"type":"' + scope.columnHeaders[i].type + '",';
                questionJson += '"id":"' + scope.columnHeaders[i].id + '",';
                questionJson += '"text":"' + scope.replaceLinebreaksWithHTML(scope.columnHeaders[i].text) + '"';
                questionJson += '},';
            }
            if (i > 0) {
                questionJson = questionJson.substring(0, questionJson.length - 1);
            }
            questionJson += ']';
            questionJson += ',';
        } else {
            questionJson += "],";
        }

        questionJson += '"ROWS": [';
        for (i = 0; i < scope.rows.length; i++) {
            questionJson += '{';
            questionJson += '"id":"' + scope.rows[i].id + '",';
            questionJson += '"type":"' + scope.rows[i].type + '",';
            questionJson += '"text":"' + scope.replaceLinebreaksWithHTML(scope.rows[i].text) + '",';
            questionJson += '"COLUMNS": [';
            for (var j = 0; j < scope.rows[i].columns.length; j++) {
                questionJson += '{';
                questionJson += '"id":"' + scope.rows[i].columns[j].id + '",';
                questionJson += '"rowId":"' + scope.rows[i].columns[j].rowId + '",';
                questionJson += '"type":"' + scope.rows[i].columns[j].type + '",';
                if (scope.question.answerFieldType !== "text") {
                    questionJson += '"points":"' + scope.rows[i].columns[j].points + '",';
                } else {
                    questionJson += '"points":"' + "" + '",';
                }
                questionJson += '"answerFieldType":"' + scope.question.answerFieldType + '"';
                questionJson += '},';
            }

            if (j > 0) {
                questionJson = questionJson.substring(0, questionJson.length - 1);
            }
            questionJson += ']';
            questionJson += '},';

        }
        if (i > 0) {
            questionJson = questionJson.substring(0, questionJson.length - 1);
        }
        questionJson += ']';
        questionJson += '}}';

        var rn = "?_=" + Date.now();

        http({
            method: 'POST',
            url: '/addQuestion/' + rn,
            params: {
                'question_id': scope.question.question_id,
                'question_title': scope.question.title,
                'answer': "test", //answerVal,
                'par_id': par_id,
                'doc_id': doc_id,
                'questionJson': questionJson
            }
        })
            .success(function (data) {
                $window.console.log("The question was successfully added to database");
                scope.removeErrors();
                //TODO: This can be optimized to get only the new one.
                scope.$parent.getQuestions();
                if (ask) {
                    scope.json = JSON.parse(data.questionJson);
                    scope.qId = data.question_id;
                    http({
                        url: '/askQuestion',
                        method: 'POST',
                        params: {
                            lecture_id: scope.lectureId,
                            question_id: scope.qId,
                            doc_id: scope.docId,
                            buster: new Date().getTime()
                        }
                    }).success(function () {
                        $rootScope.$broadcast('askQuestion', {"json": scope.json, "questionId": scope.qId});
                    }).error(function (error) {
                        $window.console.log(error);
                    });
                }
            }).error(function () {
                $window.console.log("There was some error creating question to database.");
            });

        scope.close();
    };

    scope.deleteQuestion = function () {
        var confirmDi = $window.confirm("Are you sure you want to delete this question?");
        if (confirmDi) {
            http({
                url: '/deleteQuestion',
                method: 'POST',
                params: {question_id: scope.qId, doc_id: scope.docId}
            })
                .success(function () {
                    $window.console.log("Deleted question done!");
                    //location.reload();
                    scope.close();
                    //scope.clearQuestion();
                    scope.getQuestions();
                })
                .error(function (error) {

                    $window.console.log(error);
                    scope.getQuestions();

                });

        }
    };
}]);
