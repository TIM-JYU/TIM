var katex, $, angular, modules;

var timApp = angular.module('timApp', [
    'ngSanitize',
    'angularFileUpload',
    'ui.ace',
    'ngStorage'].concat(modules)).config(['$httpProvider', function ($httpProvider) {
    var interceptor = [
        '$q',
        '$rootScope',
        '$window',
        function ($q, $rootScope, $window) {
            var re = /\/[^/]+\/([^/]+)\/answer\/$/;
            var service = {
                'request': function (config) {
                    if ($window.teacherMode && re.test(config.url)) {
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
    $httpProvider.interceptors.push(interceptor);
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
    '$localStorage',
    '$filter',
    function (sc, http, q, $upload, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter) {
        "use strict";
        http.defaults.headers.common.Version = $window.version.hash;
        http.defaults.headers.common.RefererPath = $window.refererPath;
        sc.docId = $window.docId;
        sc.docName = $window.docName;
        sc.crumbs = $window.crumbs;
        sc.rights = $window.rights;
        sc.startIndex = $window.startIndex;
        sc.users = $window.users;
        sc.group = $window.group;
        sc.teacherMode = $window.teacherMode;
        sc.sidebarState = 'autohidden';
        sc.lectureMode = $window.lectureMode;
        if (sc.users.length > 0) {
            sc.selectedUser = sc.users[0];
        } else {
            sc.selectedUser = null;
        }

        sc.noteClassAttributes = ["difficult", "unclear", "editable", "private"];
        sc.editing = false;

        sc.questionShown = false;
        sc.firstTimeQuestions = true;
        var EDITOR_CLASS = "editorArea";
        var EDITOR_CLASS_DOT = "." + EDITOR_CLASS;

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

            // TODO: Use same route (postParagraph) for both cases, determine logic based on given parameters
            if ($par.hasClass("new")) {
                url = '/newParagraph/';
            } else {
                url = '/postParagraph/';
            }

            var area_start;
            var area_end;

            if (options.area) {
                if (sc.selection.reversed) {
                    area_start = sc.selection.end;
                    area_end = sc.selection.start;
                } else {
                    area_end = sc.selection.end;
                    area_start = sc.selection.start;
                }
            } else {
                area_start = null;
                area_end = null;
            }

            var attrs = {
                "save-url": url,
                "extra-data": JSON.stringify({
                    docId: sc.docId, // current document id
                    par: par_id, // the id of paragraph on which the editor was opened
                    par_next: par_next_id, // the id of the paragraph that follows par or null if par is the last one
                    area_start: area_start,
                    area_end: area_end,
                    attrs: JSON.parse($par.attr('attrs')) // TODO: Take attrs away; not needed
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
                "delete-url": '/deleteParagraph/' + sc.docId
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
                    if (caption) {
                        $div.attr('caption', caption);
                    }
                    $par.append($div);
                    $compile($div[0])(sc);
                    sc.editing = true;
                };

                if (options.showDelete) {
                    $(".par.new").remove();
                }
                createEditor(attrs);

            }
        };

        sc.showQuestion = function (questionId) {
            sc.json = "No data";
            sc.qId = questionId;

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
                        points: data.points,
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
            if (!('pageX' in e) || (e.pageX === 0 && e.pageY === 0)) {
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
                if (downEvent === null) {
                    return;
                }

                var e2 = sc.fixPageCoords(e);
                if (sc.dist(downCoords, {left: e2.pageX, top: e2.pageY}) > 10) {
                    // Moved too far away, cancel the event
                    downEvent = null;
                }
            });
            $document.on('touchcancel', className, function (e) {
                $window.console.log("cancel");
                downEvent = null;
            });
            $document.on('mouseup touchend', className, function (e) {
                $window.console.log("tock");
                if (downEvent !== null) {
                    $window.console.log("event!");
                    if (func($(this), downEvent)) {
                        e.preventDefault();
                    }
                    downEvent = null;
                }
            });
        };

        sc.showEditWindow = function (e, $par) {
            $(".par.new").remove();
            sc.toggleParEditor($par, {showDelete: true, area: false});
        };

        sc.beginAreaEditing = function (e, $par) {
            $(".par.new").remove();
            sc.toggleParEditor($par, {showDelete: true, area: true});
        };

        sc.createNewPar = function () {
            return $("<div>", {class: "par new", id: 'NEW_PAR', attrs: '{}'})
                .append($("<div>", {class: "parContent"}).html('New paragraph'));
        };

        sc.showAddParagraphAbove = function (e, $par) {
            var $newpar = sc.createNewPar();
            $par.before($newpar);
            sc.toggleParEditor($newpar, {showDelete: false, area: false});
        };

        sc.showAddParagraphBelow = function (e, $par) {
            var $newpar = sc.createNewPar();
            $par.after($newpar);
            sc.toggleParEditor($newpar, {showDelete: false, area: false});
        };

        // Event handler for "Add question below"
        // Opens pop-up window to create question.
        sc.addQuestion = function (e, $par) {
            sc.toggleQuestion();
            sc.par = $par;
        };

        // Shows question window
        sc.toggleQuestion = function () {
            sc.questionShown = !sc.questionShown;
        };

        $.fn.slideFadeToggle = function (easing, callback) {
            return this.animate({opacity: 'toggle', height: 'toggle'}, 'fast', easing, callback);
        };

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
            if (extraData.area_start !== null && extraData.area_end !== null) {
                $par = sc.getElementByParId(extraData.area_start);
                var $endpar = sc.getElementByParId(extraData.area_end);
                if (extraData.area_start !== extraData.area_end) {
                    $par.nextUntil($endpar).add($endpar).remove();
                }
            }
            $par.remove();
            sc.editing = false;
            sc.cancelArea();
        };

        sc.addSavedParToDom = function (data, extraData) {
            var $par = sc.getElementByParId(extraData.par),
                len = data.texts.length;

            // check if we were editing an area
            if (extraData.area_start !== null && extraData.area_end !== null) {
                $par = sc.getElementByParId(extraData.area_start);

                // remove all but the first element of the area because it'll be removed later
                // and is required to insert the new ones in the document
                var $endpar = sc.getElementByParId(extraData.area_end);
                $par.nextUntil($endpar).add($endpar).remove();
            }
            http.defaults.headers.common.Version = data.version;
            for (var i = len - 1; i >= 0; i--) {
                var html = data.texts[i].html;
                if ('taskId' in data.texts[i].attrs) {
                    html = $compile(html)(sc);
                }
                var $mathdiv = $.parseHTML(html);
                if ($mathdiv) {
                    sc.processMath($mathdiv[0]);
                }
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
                if (data.texts[i].id in data.read_statuses) {
                    readClass = data.read_statuses[data.texts[i].id];
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
                        {class: "readline " + readClass, title: "Click to mark this paragraph as read"}),
                    $("<div>", {class: "editline", title: "Click to edit this paragraph"})));
            }
            $par.remove();
            sc.editing = false;
            sc.cancelArea();
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

        sc.isParWithinArea = function ($par) {
            return sc.selection.pars.filter($par).length > 0;
        };

        sc.onClick(".editline", function ($this, e) {
            $(".actionButtons").remove();
            var $par = $this.parent();
            if (sc.selection.start !== null && (sc.selection.end === null || !sc.isParWithinArea($par))) {
                sc.selection.end = sc.getParId($par);
            }
            var coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top};
            return sc.showOptionsWindow(e, $par, coords);
        });

        sc.showNoteWindow = function (e, $par) {
            sc.toggleNoteEditor($par, {isNew: true});
        };

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

            var $target = $(e.target);
            var tag = $target.prop("tagName");

            // Don't show paragraph menu on these specific tags or class
            var ignoredTags = ['BUTTON', 'INPUT', 'TEXTAREA', 'A'];
            if (ignoredTags.indexOf(tag) > -1 || $target.parents('.no-popup-menu').length > 0) {
                return false;
            }

            var $par = $this.parent();
            if (sc.selection.start !== null) {
                sc.selection.end = sc.getParId($par);
            }
            else
            {
                var coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top};
                var toggle1 = $par.find(".actionButtons").length === 0;
                var toggle2 = $par.hasClass("lightselect");

                $(".par.selected").removeClass("selected");
                $(".par.lightselect").removeClass("lightselect");
                $(".actionButtons").remove();
                sc.toggleActionButtons(e, $par, toggle1, toggle2, coords);
            }
            sc.$apply();
            return true;
        });

        sc.onClick(".noteContent", function ($this, e) {
            sc.toggleNoteEditor($this.parent().parent().parent(), {isNew: false, noteData: $this.parent().data()});
            return true;
        });

        sc.onClick(".questionAdded", function ($this, e) {
            var question = $this;
            var questionId = question[0].getAttribute('id');
            sc.showQuestion(questionId);
            sc.par = ($(question).parent().parent());
        });

        // Note-related functions

        sc.showOptionsWindow = function (e, $par, coords) {
            var $popup = $('<popup-menu>');
            $popup.attr('tim-draggable-fixed', '');
            $par.prepend($popup); // need to prepend to DOM before compiling
            $compile($popup[0])(sc);
            // TODO: Set offset for the popup
            var element = $popup;
            var viewport = {};
            viewport.top = $(window).scrollTop();
            viewport.bottom = viewport.top + $(window).height();
            var bounds = {};
            bounds.top = element.offset().top;
            bounds.bottom = bounds.top + element.outerHeight();
            var y = $(window).scrollTop();
            if (bounds.bottom > viewport.bottom) {
                y += (bounds.bottom - viewport.bottom);
            }
            else if (bounds.top < viewport.top) {
                y += (bounds.top - viewport.top);
            }
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
                else if (clicktime < 500 && sc.defaultAction !== null) {
                    // Double click
                    sc.defaultAction.func(e, $par, coords);
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
            return "#" + str.replace(/^(\d)+(\.\d+)*\.? /, "").trim().replace(/ +/g, '-').toLowerCase();
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
            sc.indexTable = [];
            var parentEntry = null;
            $(".par h1, .par h2, .par h3").each(function () {
                var id = '#' + $(this).attr('id');
                var header = $(this).prop('tagName');
                var lvl = parseInt(header.substring(1));

                var astyle = "a" + lvl;
                var txt = $(this).text();
                txt = txt.trim().replace(/\\#/g, "#");
                var entry = {
                    text: sc.totext(txt),
                    target: id,
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
            });

            if (parentEntry !== null) {
                if (parentEntry.items.length > 0) {
                    parentEntry.state = 'col';
                }
                sc.indexTable.push(parentEntry);
            }
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

            var newState = sc.invertState(state);
            if (newState !== state) {
                sc.clearSelection();
            }
            return newState;
        };




        sc.onClick('.showContent', function($this, e) {
            sc.contentShown = !sc.contentShown;
            var $pars = $('#pars');
            if (sc.contentLoaded) {
                if (sc.contentShown) {
                    $pars.css('display', '');
                    $('.showContent').text('Hide content');
                } else {
                    $pars.css('display', 'none');
                    $('.showContent').text('Show content');
                }
                return true;
            }
            var $loading = $('<div>', {class: 'par', id: 'loading'});
            $loading.append($('<img>', {src: "/static/images/loading.gif"}));
            $('.paragraphs').append($loading);
            http.get('/view_content/' + sc.docName)
                .success(function (data) {
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
                }).error(function (data) {
                    var $loading = $('#loading');
                    $loading.remove();
                    $window.console.log("Error occurred when fetching view_content");
                });
            sc.contentLoaded = true;
            return true;
        });

        if (sc.lectureMode) {
            sc.$on("getQuestions", function () {
                if (sc.firstTimeQuestions) {
                    sc.getQuestions();
                    sc.firstTimeQuestions = false;
                }
            });

            sc.$on("closeQuestionPreview", function () {
                sc.showQuestionPreview = false;
            });
        }

        // Load index, notes and read markings
        sc.setHeaderLinks();
        sc.indexTable = [];
        sc.getIndex();
        sc.getNotes();
        sc.getReadPars();


        // If you add 'mousedown' to bind, scrolling upon opening the menu doesn't work on Android
        $('body,html').bind('scroll wheel DOMMouseScroll mousewheel', function (e) {
            if (e.which > 0 || e.type === "mousedown" || e.type === "mousewheel") {
                $("html,body").stop();
            }
        });

        if (sc.rights.editable) {
            sc.onClick(".addBottom", function ($this, e) {
                $(".actionButtons").remove();
                var $par = $('#pars').children().last();
                return sc.showAddParagraphBelow(e, $par);
            });
        }
        sc.processAllMath($('body'));

        sc.defaultAction = {func: sc.showOptionsWindow, desc: 'Show options window'};
        sc.selection = {start: null, end: null};
        sc.$watchGroup(['lectureMode', 'selection.start', 'selection.end'], function (newValues, oldValues, scope) {
            sc.editorFunctions = sc.getEditorFunctions();
        });

        sc.$watchGroup(['selection.start', 'selection.end'], function (newValues, oldValues, scope) {
            $('.par.selected').removeClass('selected');
            if (sc.selection.start !== null) {
                var $start = sc.getElementByParId(sc.selection.start);
                if (sc.selection.end !== null && sc.selection.end !== sc.selection.start) {
                    var $end = sc.getElementByParId(sc.selection.end);
                    if ($end.prevAll().filter($start).length !== 0) {
                        sc.selection.reversed = false;
                        sc.selection.pars = $start.nextUntil($end);

                    } else {
                        sc.selection.reversed = true;
                        sc.selection.pars = $start.prevUntil($end);
                    }
                    sc.selection.pars = sc.selection.pars.add($start).add($end);
                } else {
                    sc.selection.pars = $start;
                }
                sc.selection.pars.addClass('selected');
            }
        });

        sc.startArea = function (e, $par) {
            sc.selection.start = sc.getParId($par);
        };

        sc.cancelArea = function (e, $par) {
            sc.selection.start = null;
            sc.selection.end = null;
        };

        sc.getEditorFunctions = function () {
            return [
                {func: sc.showNoteWindow, desc: 'Comment/note', show: sc.rights.can_comment},
                {func: sc.showEditWindow, desc: 'Edit', show: sc.rights.editable},
                {func: sc.showAddParagraphAbove, desc: 'Add paragraph above', show: sc.rights.editable},
                {func: sc.showAddParagraphBelow, desc: 'Add paragraph below', show: sc.rights.editable},
                {func: sc.addQuestion, desc: 'Create question', show: sc.lectureMode},
                {func: sc.startArea, desc: 'Start selecting area', show: sc.selection.start === null},
                {func: sc.beginAreaEditing, desc: 'Edit area', show: sc.selection.start !== null},
                {func: sc.cancelArea, desc: 'Cancel area', show: sc.selection.start !== null}
            ];
        };

        sc.nothing = function(){};

        sc.editorFunctions = sc.getEditorFunctions();

        sc.$storage = $localStorage.$default({
            defaultAction: null
        });

        try {
            var found = $filter('filter')(sc.editorFunctions,
                {desc: sc.$storage.defaultAction}, true);
            if (found.length) {
                sc.defaultAction = found[0];
            }
        } catch (e) {
        }
    }
])
;
