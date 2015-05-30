var katex, $, angular, modules, version, refererPath, docId, docName, rights, startIndex, users, teacherMode, lectureMode;

/*global $:false */

var timApp = angular.module('timApp');
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
        sc.rights = rights;
        sc.startIndex = startIndex;
        sc.users = users;
        sc.teacherMode = teacherMode;
        sc.lectureMode = lectureMode;
        sc.selectedUser = sc.users[0];
        sc.noteClassAttributes = ["difficult", "unclear", "editable", "private"];
        sc.editing = false;
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

        sc.getParIndex = function ($par) {
            return $par.index() + sc.startIndex;
        };

        sc.getElementByParIndex = function (index) {
            return $("#pars").children().eq(index - sc.startIndex);
        };

        sc.toggleParEditor = function ($par, options) {
            var url;
            if ($par.hasClass("new")) {
                url = '/newParagraph/';
            } else {
                url = '/postParagraph/';
            }
            var par_id = sc.getParIndex($par);
            var attrs = {
                "save-url": url,
                "extra-data": JSON.stringify({
                    docId: sc.docId,
                    par: par_id
                }),
                "options": JSON.stringify({
                    showDelete: options.showDelete,
                    showImageUpload: true,
                    destroyAfterSave: true
                }),
                "after-save": 'addSavedParToDom(saveData, extraData)',
                "after-cancel": 'handleCancel(extraData)',
                "after-delete": 'handleDelete(saveData, extraData)',
                "preview-url": '/preview/' + sc.docId,
                "delete-url": '/deleteParagraph/' + sc.docId + "/" + par_id
            };
            if (options.showDelete) {
                attrs["initial-text-url"] = '/getBlock/' + sc.docId + "/" + par_id;
            }
            sc.toggleEditor($par, options, attrs);
        };

        sc.toggleEditor = function ($par, options, attrs) {
            if ($par.children(EDITOR_CLASS_DOT).length) {
                $par.children().remove(EDITOR_CLASS_DOT);
                sc.editing = false;
            } else {
                $(EDITOR_CLASS_DOT).remove();

                var createEditor = function (attrs) {
                    var $div = $("<pareditor>", {class: EDITOR_CLASS}).attr(attrs);
                    $compile($div[0])(sc);
                    $par.append($div);
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
            sc.qId = -1;
            if (question[0].hasAttribute('json')) {
                sc.json = JSON.parse(question[0].getAttribute('json'));
                sc.qId = question[0].getAttribute('id');

            }
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
            sc.$digest();
        };

        sc.toggleNoteEditor = function ($par, options) {
            if (!sc.rights.can_comment) {
                return;
            }
            var url,
                data;
            if (options.isNew) {
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

            var par_id = sc.getParIndex($par),
                attrs = {
                    "save-url": url,
                    "extra-data": JSON.stringify(angular.extend({
                        docId: sc.docId,
                        par: par_id
                    }, data)),
                    "options": JSON.stringify({
                        showDelete: !options.isNew,
                        showImageUpload: false,
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
            sc.toggleEditor($par, options, attrs);
        };

        sc.forEachParagraph = function (func) {
            $('.paragraphs .par').each(func);
        };

        // Event handlers

        var ua = $window.navigator.userAgent,
            eventName = (ua.match(/iPad/i)) ? "touchstart" : "click";

        sc.addEvent = function (className, func) {
            $document.on(eventName, className, func);
        };

        sc.showEditWindow = function (e, $par) {
            sc.toggleParEditor($par, {showDelete: true});
        };

        sc.addEvent(PAR_EDIT_BUTTON, function (e) {
            var $par = $(e.target).parent().parent().parent();
            $(".par.new").remove();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.showEditWindow(e, $par, null);
        });

        sc.addEvent("#defaultEdit", function (e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.defaultAction = sc.showEditWindow;
        });

        sc.showAddParagraphAbove = function (e, $par) {
            var $newpar = $("<div>", {class: "par new"})
                .append($("<div>", {class: "parContent"}).html('New paragraph'));
            $par.before($newpar);
            sc.toggleParEditor($newpar, {showDelete: false});
        };

        sc.showAddParagraphBelow = function (e, $par) {
            var $newpar = $("<div>", {class: "par new"})
                .append($("<div>", {class: "parContent"}).html('New paragraph'));
            $par.after($newpar);
            sc.toggleParEditor($newpar, {showDelete: false});
        };

        sc.addEvent(PAR_ADD_BUTTON, function (e) {
            var $par = $(e.target).parent().parent().parent();
            $(".par.new").remove();
            sc.toggleActionButtons(e, $par, false, false, null);
            var $newpar = $("<div>", {class: "par new"})
                .append($("<div>", {class: "parContent"}).html('New paragraph'));

            if ($(e.target).hasClass("above")) {
                $par.before($newpar);
            } else if ($(e.target).hasClass("below")) {
                $par.after($newpar);
            }

            sc.toggleParEditor($newpar, {showDelete: false});
        });

        // Event handler for "Add question below"
        // Opens pop-up window to create question.
        sc.addEvent(QUESTION_ADD_BUTTON, function (e) {
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

        sc.addEvent("#defaultPrepend", function (e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.defaultAction = sc.showAddParagraphAbove;
        });

        sc.addEvent("#defaultAppend", function (e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.defaultAction = sc.showAddParagraphBelow;
        });

        sc.doNothing = function (e, $par) {
            sc.toggleActionButtons(e, $par, false, false, null);
        };

        sc.addEvent(PAR_CLOSE_BUTTON, function (e) {
            var $par = $(e.target).parent().parent().parent();
            $(".par.new").remove();
            sc.toggleActionButtons(e, $par, false, false, null);
        });

        sc.addEvent("#defaultClose", function (e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.defaultAction = sc.doNothing;
        });

        sc.handleCancel = function (extraData) {
            var $par = sc.getElementByParIndex(extraData.par);
            if ($par.hasClass("new")) {
                $par.remove();
            }
            sc.editing = false;
        };

        sc.handleDelete = function (data, extraData) {
            var $par = sc.getElementByParIndex(extraData.par);
            http.defaults.headers.common.Version = data.version;
            $par.remove();
            sc.editing = false;
        };

        sc.addSavedParToDom = function (data, extraData) {
            var $par = sc.getElementByParIndex(extraData.par),
                len = data.texts.length;
            http.defaults.headers.common.Version = data.version;
            for (var i = len - 1; i >= 0; i--) {
                var $newpar = $("<div>", {class: "par"})
                    .append($("<div>", {class: "parContent"}).html($compile(data.texts[i].html)(sc)));
                var readClass = "unread";
                if (i === 0 && !$par.hasClass("new")) {
                    $par.find(".notes").appendTo($newpar);
                    if ($par.find(".read, .modified").length > 0) {
                        readClass = "modified";
                    }
                }
                $par.after($newpar.append($("<div>",
                    {class: "readline " + readClass, title: "Click to mark this paragraph as read"})));
                sc.processMath($newpar[0]);
            }
            $par.remove();
            sc.editing = false;
        };

        sc.addEvent(".readline", function () {
            var par_id = sc.getParIndex($(this).parents('.par'));
            var oldClass = $(this).attr("class");
            $(this).attr("class", "readline read");
            http.put('/read/' + sc.docId + '/' + par_id + '?_=' + Date.now())
                .success(function (data, status, headers, config) {
                    // No need to do anything here
                }).error(function () {
                    $window.alert('Could not save the read marking.');
                    $(this).attr("class", oldClass);
                });
        });

        sc.showNoteWindow = function (e, $par) {
            sc.toggleNoteEditor($par, {isNew: true});
        };

        sc.addEvent(NOTE_ADD_BUTTON, function (e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.showNoteWindow(e, $par, null);
        });

        sc.addEvent("#defaultAdd", function (e) {
            var $par = $(e.target).parent().parent().parent();
            sc.toggleActionButtons(e, $par, false, false, null);
            sc.defaultAction = sc.showNoteWindow;
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

        sc.addEvent('.paragraphs .parContent', function (e) {
            if (sc.editing) {
                return;
            }

            sc.$apply();

            var $target = $(e.target);
            var tag = $target.prop("tagName");

            // Don't show paragraph menu on these specific tags or class
            var ignoredTags = ['BUTTON', 'INPUT', 'TEXTAREA', 'A'];
            if (ignoredTags.indexOf(tag) > -1 || $target.parents('.no-popup-menu').length > 0) {
                return;
            }

            var $par = $(this).parent();
            var coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top};
            var toggle1 = $par.find(".actionButtons").length === 0;
            var toggle2 = $par.hasClass("lightselect");

            $(".par.selected").removeClass("selected");
            $(".par.lightselect").removeClass("lightselect");
            $(".actionButtons").remove();
            sc.toggleActionButtons(e, $par, toggle1, toggle2, coords);
        });

        sc.addEvent(".noteContent", function () {
            sc.toggleNoteEditor($(this).parent().parent().parent(), {isNew: false, noteData: $(this).parent().data()});
        });

        sc.addEvent(".questionAdded", function () {
            sc.showQuestion($(this));
        });

        // Note-related functions

        sc.showOptionsWindow = function (e, $par, coords) {
            var default_width = $par.outerWidth() / 16;
            var button_width = $par.outerWidth() / 4 - 1.7 * default_width;
            var $actionDiv = $("<div>", {class: 'actionButtons'});
            var $span;
            if (sc.rights.can_comment) {
                $span = $("<span>");
                $span.append($("<button>", {class: NOTE_ADD_BUTTON_CLASS, text: 'Comment/note', width: button_width}));
                $span.append($("<button>", {
                    id: 'defaultAdd',
                    class: DEFAULT_BUTTON_CLASS,
                    text: 'Default',
                    width: default_width
                }));
                $actionDiv.append($span);
            }
            if (sc.rights.editable) {
                $span = $("<span>");
                $span.append($("<button>", {class: PAR_EDIT_BUTTON_CLASS, text: 'Edit', width: button_width}));
                $span.append($("<button>", {
                    id: 'defaultEdit',
                    class: DEFAULT_BUTTON_CLASS,
                    text: 'Default',
                    width: default_width
                }));
                $actionDiv.append($span);

                $span = $("<span>");
                $span.append($("<button>", {
                    class: PAR_ADD_BUTTON_CLASS + ' above',
                    text: 'Add paragraph above',
                    width: button_width
                }));
                $span.append($("<button>", {
                    id: 'defaultPrepend',
                    class: DEFAULT_BUTTON_CLASS,
                    text: 'Default',
                    width: default_width
                }));
                $actionDiv.append($span);

                $span = $("<span>");
                $span.append($("<button>", {
                    class: PAR_ADD_BUTTON_CLASS + ' below',
                    text: 'Add paragraph below',
                    width: button_width
                }));
                $span.append($("<button>", {
                    id: 'defaultAppend',
                    class: DEFAULT_BUTTON_CLASS,
                    text: 'Default',
                    width: default_width
                }));
                $actionDiv.append($span);

                if (sc.lectureMode) {
                    $span = $("<span>");
                    $span.append($("<button>", {
                        class: QUESTION_ADD_BUTTON_CLASS,
                        text: 'Create question',
                        width: button_width
                    }));
                    $span.append($("<button>", {
                        id: 'createQuestion',
                        class: DEFAULT_BUTTON_CLASS,
                        text: 'Default',
                        width: default_width
                    }));
                    $actionDiv.append($span);
                }

                $span = $("<span>");
                $span.append($("<button>", {class: PAR_CLOSE_BUTTON_CLASS, text: 'Close menu', width: button_width}));
                $span.append($("<button>", {
                    id: 'defaultClose',
                    class: DEFAULT_BUTTON_CLASS,
                    text: 'Default',
                    width: default_width
                }));
                $actionDiv.append($span);

            }
            $actionDiv.offset(coords);
            $actionDiv.css('position', 'absolute'); // IE needs this
            $par.prepend($actionDiv);
        };

        sc.toggleActionButtons = function (e, $par, toggle1, toggle2, coords) {
            if (!sc.rights.editable && !sc.rights.can_comment) {
                return;
            }
            if (toggle2) {
                // Clicked twice successively
                var clicktime = new Date().getTime() - sc.lastclick;
                //console.log(clicktime);
                $par.addClass("selected");

                if (clicktime < 500) {
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
                sc.lastclick = new Date().getTime();
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
                    .append($("<div>", {class: 'noteContent', html: notes[i].htmlContent})));
            }
            return $noteDiv;
        };

        sc.getQuestionHtml = function (questions) {
            var questionImage = '../../../static/images/show-question-icon.png';
            var $questionsDiv = $("<div>", {class: 'questions'});

            // TODO: Think better way to get the ID of question.
            for (var i = 0; i < questions.length; i++) {
                var img = new Image(30, 30);
                img.src = questionImage;
                var $questionDiv = $("<div>", {
                    class: 'questionAdded', html: img, json: questions[i].questionJson, id: questions[i].question_id
                });
                $questionsDiv.append($questionDiv);
            }
            return $questionsDiv;
        };


        sc.getQuestions = function () {
            http.get('/questions/' + sc.docId)
                .success(function (data) {
                var pars = {};
                var questionCount = data.length;
                for (var i = 0; i < questionCount; i++) {
                    var pi = data[i].par_index;
                    if (!(pi in pars)) {
                        pars[pi] = {questions: []};
                    }

                    pars[pi].questions.push(data[i]);
                }

                sc.forEachParagraph(function (index) {
                    var parIndex = index + sc.startIndex;
                    if (parIndex in pars) {
                        var $questionsDiv = sc.getQuestionHtml(pars[parIndex].questions);
                        $(this).append($questionsDiv);

                    }
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
                    var pi = data[i].par_index;
                    if (!(pi in pars)) {
                        pars[pi] = {notes: []};

                    }
                    if (!('notes' in pars[pi])) {
                        pars[pi].notes = [];
                    }
                    pars[pi].notes.push(data[i]);
                }
                sc.forEachParagraph(function (index) {
                    var parIndex = index + sc.startIndex;
                    if (parIndex in pars) {
                        var $notediv = sc.getNoteHtml(pars[parIndex].notes);
                        var $this = $(this);
                        $this.append($notediv);
                        sc.processAllMath($this);
                    }
                });

            }).error(function () {
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
                    var pi = data[i].par_index;
                    if (!(pi in pars)) {
                        pars[pi] = {};
                    }
                    pars[pi].readStatus = readPar.status;
                }
                sc.forEachParagraph(function (index) {
                    var parIndex = index + sc.startIndex;
                    var classes = ["readline"];
                    if (parIndex in pars && 'readStatus' in pars[parIndex]) {
                        classes.push(pars[parIndex].readStatus);
                    } else {
                        classes.push("unread");
                    }
                    var $div = $("<div>", {class: classes.join(" "), title: "Click to mark this paragraph as read"});
                    $(this).append($div);
                });
            }).error(function () {
                $window.alert("Could not fetch reading info.");
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
            });
        }

        // Load index, notes and read markings
        sc.setHeaderLinks();
        sc.indexTable = [];
        sc.getIndex();
        sc.getNotes();
        sc.getReadPars();
        sc.processAllMath($('body'));
        sc.defaultAction = sc.showOptionsWindow;
    }]);

timApp.controller("QuestionController", ['$scope', '$http', '$window', function (scope, http, $window) {
    "use strict";
    $(function () {
        $('#calendarStart').datepicker({dateFormat: 'dd.m.yy'});
    });
    scope.question = {
        title: "",
        question: "",
        matrixType: "",
        answerFieldType: "",
        timeLimit: {hours: "", minutes: "", seconds: ""}
    };


    scope.questionType = "";
    scope.rows = [];
    scope.columns = [];
    scope.columnHeaders = [];
    scope.answerDirection = "horizontal";
    scope.question.timeLimit.seconds = 10;
    scope.error_message = "";
    scope.answerFieldTypes = [

        {label: "Text area", value: "textArea"},
        {label: "Radio button vertical", value: "radiobutton-vertical"},
        {label: "Radio Button horizontal", value: "radiobutton-horizontal"},
        {label: "Checkbox", value: "checkbox"}
    ];

    scope.createMatrix = function (rowsCount, columnsCount, type) {
        if (type === 'radio' || type === 'checkbox') {
            scope.question.answerFieldType = type;
        }
        if (type === 'true-false') {
            scope.question.answerFieldType = 'radio';
        }
        if (type === 'matriisi') {
            scope.question.answerFieldType = 'matriisi';
        }

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
    };


    scope.rowClick = function (index) {
        scope.addRow(index);
    };

    scope.addCol = function (loc) {
        var location = loc;
        if (loc === -1) {
            location = scope.rows[0].columns.length;
            loc = scope.rows[0].columns.length;
        }
        scope.columnHeaders.splice(loc, 0, {type: "header", text: ""});
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
    scope.addRow = function (loc) {

        scope.CreateColumnsForRow = function (location) {
            var columns = [];
            for (var j = 0; j < scope.rows[0].columns.length; j++) {
                columns[j] = {
                    id: j,
                    rowId: location,
                    type: "answer",
                    value: '',
                    answerFiledType: scope.question.answerFieldType
                };

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

    scope.delRow = function (indexToBeDeleted) {
        if (indexToBeDeleted === -1) {
            scope.rows.splice(-1, 1);
        }
        else {
            scope.rows.splice(indexToBeDeleted, 1);
        }
    };

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

    scope.clearQuestion = function () {
        scope.question = {
            question: ""
        };

        scope.rows.splice(0, scope.rows.length - 1);
        scope.answer = "";
        scope.toggleQuestion();
    };

    scope.close = function () {
        scope.removeErrors();
        scope.clearQuestion();
    };

    scope.replaceLinebreaksWithHTML = function (val){
        var output = val.replace(/(?:\r\n|\r|\n)/g, '<br />');
        return output.replace(/\\/g, "\\\\");
    };

    scope.errorize = function (div_val, error_text) {
        angular.element("#" + div_val).css('border', "1px solid red");
        if (error_text.length > 0) {
            scope.error_message += error_text + "<br />";
        }
    };

    scope.errorizeClass = function (div_val, error_text) {
        angular.element("." + div_val).css('border', "1px solid red");
        if (error_text.length > 0) {
            scope.error_message += error_text + "<br />";
        }
    };

    scope.defInputStyle = function (element) {
        if (element !== null || !element.isDefined) {
            angular.element("#"+element).css("border", "");
        }
    };

    scope.removeErrors = function () {
        scope.error_message = "";
        var elementsToRemoveErrorsFrom = [
            "questionName",
            "questionTiming",
            "questionStart",
            "questionTimer",
            "qType",
            "matrix"
        ];
        for (var i = 0; i < elementsToRemoveErrorsFrom.length; i++) {
            if (elementsToRemoveErrorsFrom[i] !== undefined) {
                scope.defInputStyle(elementsToRemoveErrorsFrom[i]);
            }
        }
        angular.element(".rowHeading").css("border", "");
    };

    scope.rowHeadingsEmpty = function(rows) {
        for(var i=0; i<rows.length; i++){
            if(rows[i].text === "" || rows[i].text === null){
                return true;
            }
        }
        return false;
    };

    scope.createQuestion = function () {
        scope.removeErrors();
        if (scope.question.question === undefined || scope.question.question.trim().length === 0 || scope.question.title === undefined || scope.question.title.trim().length === 0) {
            scope.errorize("questionName","Both title and question are required for a question.");
        }
        if (scope.question.type === undefined) {
            scope.errorize("qType", "Question type must be selected.");
        } else if (scope.question.type === "matrix" && scope.question.MatrixType === undefined) {
            scope.errorize("check", "Answer type must be selected.");
        } else if ((scope.question.type === "radio-vertical" ||
                    scope.question.type === "checkbox-vertical" ||
                    scope.question.type === "true-false") &&
                    scope.rowHeadingsEmpty(scope.rows)) {
                        scope.errorizeClass("rowHeading", "All rows must be filled in.");
        }
        if ((scope.question.type === "radio-vertical" || scope.question.type === "checkbox-vertical") && scope.rows.length < 2) {
            scope.errorize("matrix", "You must have at least two choices.");
        }
        if (scope.error_message !== ""){
            return;
        }
        if (scope.question.answerFieldType === 'matriisi') {
            if (scope.question.matrixType === "radiobutton-horizontal" || scope.question.matrixType === "radiobutton-vertical") {
                scope.question.answerFieldType = "radio";
            }
        }
        if (scope.question.matrixType === "textArea") {
            scope.question.answerFieldType = "text";
        }
        if (scope.question.matrixType === "checkbox") {
            scope.question.answerFieldType = "checkbox";
        }

        var doc_id = scope.docId;
        var $par = scope.par;
        var par_index = scope.getParIndex($par);
        var timeLimit = "";
        if (scope.question.endTimeSelected) {
            timeLimit = 0;
            timeLimit = parseInt(timeLimit) + parseInt(scope.question.timeLimit.seconds);
            if (scope.question.timeLimit.hours) {
            timeLimit = parseInt(timeLimit) + (scope.question.timeLimit.hours * 60 * 60);
            }
            if (scope.question.timeLimit.minutes) {
                timeLimit = parseInt(timeLimit) + (scope.question.timeLimit.minutes * 60);
            }
        }
        //TODO use  JSON.stringify

        scope.question.question = scope.replaceLinebreaksWithHTML(scope.question.question);
        scope.question.title = scope.replaceLinebreaksWithHTML(scope.question.title);
        var questionJson = '{"QUESTION": "' + scope.question.question + '", "TITLE": "' + scope.question.title + '", "TYPE": "' + scope.question.type + '", "TIMELIMIT": "' + timeLimit + '", "DATA": {';

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
                questionJson += '"points":"' + scope.rows[i].columns[j].points + '",';
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

        http({
            method: 'POST',
            url: '/addQuestion',
            params: {
                'question': scope.question.question,
                'answer': "test", //answerVal,
                'par_index': par_index,
                'doc_id': doc_id,
                'questionJson': questionJson
            }
        })
            .success(function () {
                $window.console.log("The question was successfully added to database");
                scope.removeErrors();
                scope.clearQuestion();
                //TODO: This can be optimized to get only the new one.
                scope.$parent.getQuestions();
            })
            .error(function () {
                $window.console.log("There was some error creating question to database.");
            });
    };
}]);
