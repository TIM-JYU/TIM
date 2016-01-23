var katex, $, angular, modules, MathJax;

var timApp = angular.module('timApp', [
    'ngSanitize',
    'angularFileUpload',
    'ui.ace',
    'ngStorage'].concat(modules)).config(['$httpProvider', function ($httpProvider) {
    timLogTime("timApp config","view");
    var interceptor = [
        '$q',
        '$rootScope',
        '$window',
        function ($q, $rootScope, $window) {
            var re = /\/[^/]+\/([^/]+)\/answer\/$/;
            var service = {
                'request': function (config) {
                    if (re.test(config.url)) {
                        var match = re.exec(config.url);
                        var taskId = match[1];
                        var ab = angular.element("answerbrowser[task-id='" + taskId + "']");
                        if (ab.isolateScope() ) {
                            var browserScope = ab.isolateScope();
                            angular.extend(config.data, {abData: browserScope.getBrowserData()});
                        }
                        var $par = ab.parents('.par');
                        if ( ab.scope() ) angular.extend(config.data, {ref_from: {docId: ab.scope().docId, par: $par.attr('id')}});
                    }
                    return config;
                },
                'response': function (response) {
                    if (re.test(response.config.url)) {
                        var match = re.exec(response.config.url);
                        var taskId = match[1];
                        $rootScope.$broadcast('answerSaved', {taskId: taskId, savedNew: response.data.savedNew});
                        if (response.data.error) {
                            $window.alert(response.data.error);
                        }
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
    '$timeout',
    function (sc, http, q, $upload, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout) {
        "use strict";
        timLogTime("VieCtrl start","view");
        http.defaults.headers.common.Version = $window.version.hash;
        http.defaults.headers.common.RefererPath = $window.refererPath;
        sc.docId = $window.docId;
        sc.docName = $window.docName;
        sc.showIndex = $window.showIndex;
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
        sc.mathJaxLoaded = false;
        sc.mathJaxLoadDefer = null;
        sc.hideRefresh = false;
        sc.hidePending = false;
        sc.pendingUpdates = {};
        var EDITOR_CLASS = "editorArea";
        var EDITOR_CLASS_DOT = "." + EDITOR_CLASS;

        sc.reload = function() {
            sc.markPageNotDirty();
            $window.location.reload();
        };

        sc.closeRefreshDlg = function() {
            sc.hideRefresh = true;
        };

        sc.markPageDirty = function() {
            var e = angular.element('#page_is_dirty');
            e.val('1');
            sc.hideRefresh = true;
        };

        sc.markPageNotDirty = function() {
            var e = angular.element('#page_is_dirty');
            e.val('0');
        };

        sc.isPageDirty = function() {
            var e = angular.element('#page_is_dirty');
            return e.val() === '1';
        };

        sc.processAllMathDelayed = function ($elem) {
            $timeout(function () {
                sc.processAllMath($elem);
            }, 500);
        };

        sc.processAllMath = function ($elem) {
            timLogTime("processAllMath start","view");
            $elem.find('.math').each(function () {
                sc.processMath(this);
            });
            timLogTime("processAllMath end","view");
        };

        sc.processMath = function (elem) {
            try {
                $window.renderMathInElement(elem);
            }
            catch (e) {
                if (sc.mathJaxLoaded) {
                    MathJax.Hub.Queue(["Typeset", MathJax.Hub, elem]);
                } else {
                    if (sc.mathJaxLoadDefer === null) {
                        sc.mathJaxLoadDefer = $.ajax({
                            dataType: "script",
                            cache: true,
                            url: "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
                        });
                    }
                    sc.mathJaxLoadDefer.done(function () {
                        sc.mathJaxLoaded = true;
                        MathJax.Hub.Queue(["Typeset", MathJax.Hub, elem]);
                    });
                }
            }
        };

        sc.showDialog = function (message) {
            $('<div id="dialog"><p>' + message + '</div>').dialog({
                dialogClass: "no-close", modal: true,
                close: function (event, ui) {
                    $(this).dialog("close");
                    $(this).remove();
                },
                buttons: [
                    {
                        text: "OK",
                        click: function () {
                            $(this).dialog("close");
                        }
                    }
                ]
            });
        };

        sc.$on('showDialog', function (event, message) {
            sc.showDialog(message);
        });

        sc.changeUser = function (user) {
            sc.selectedUser = user;
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

        sc.getElementByParHash = function(t) {
            return $("[t='" + t + "']");
        };

        sc.toggleParEditor = function ($par, options) {
            var caption = 'Add paragraph';
            var touch = typeof('ontouchstart' in window || navigator.msMaxTouchPoints) !== 'undefined';
            var mobile = touch && (window.screen.width < 1200);
            var url;
            var par_id = sc.getParId($par);
            var par_next_id = sc.getParId($par.next());
            if (par_next_id == "null")
                par_next_id = null;

            // TODO: Use same route (postParagraph) for both cases, determine logic based on given parameters
            if (par_id == "null" || $par.hasClass("new")) {
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
                "extra-data": {
                    docId: sc.docId, // current document id
                    par: par_id, // the id of paragraph on which the editor was opened
                    par_next: par_next_id, // the id of the paragraph that follows par or null if par is the last one
                    area_start: area_start,
                    area_end: area_end
                },
                "options": {
                    showDelete: options.showDelete,
                    showImageUpload: true,
                    showPlugins: true,
                    destroyAfterSave: true,
                    touchDevice: mobile,
                    tags: [
                        {name: 'markread', desc: 'Mark as read'}
                    ]
                },
                "after-save": 'addSavedParToDom(saveData, extraData)',
                "after-cancel": 'handleCancel(extraData)',
                "after-delete": 'handleDelete(saveData, extraData)',
                "preview-url": '/preview/' + sc.docId,
                "delete-url": '/deleteParagraph/' + sc.docId
            };
            if (options.showDelete) {
                caption = 'Edit paragraph';
                if (par_id != "null")
                    attrs["initial-text-url"] = '/getBlock/' + sc.docId + "/" + par_id;
            }
            sc.toggleEditor($par, options, attrs, caption);
        };

        sc.getRefAttrs = function ($par) {
            return {
                'ref-id': $par.attr('ref-id'),
                'ref-t': $par.attr('ref-t'),
                'ref-doc-id': $par.attr('ref-doc-id')
            };
        };

        sc.toggleEditor = function ($par, options, attrs, caption) {
            if (sc.isReference($par)) {
                angular.extend(attrs['extra-data'], sc.getRefAttrs($par));
            }
            Object.keys(attrs).forEach(function (key, index) {
                if (typeof attrs[key] === 'object' && attrs[key] !== null) {
                    //console.log('converting ' + key + " to string");
                    attrs[key] = JSON.stringify(attrs[key]);
                }
            });
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
                        expl: JSON.parse(data.expl),
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
            var touch = typeof('ontouchstart' in window || navigator.msMaxTouchPoints) !== 'undefined';
            var mobile = touch && (window.screen.width < 1200);
            if (!sc.rights.can_comment) {
                return;
            }
            var url,
                data, initUrl;
            if (options.isNew) {
                caption = 'Add comment';
                url = '/postNote';
                data = {
                    access: sc.$storage.noteAccess,
                    tags: {
                        difficult: false,
                        unclear: false
                    }
                };
                initUrl = null;
            } else {
                url = '/editNote';
                data = {};
                initUrl = '/note/' + options.noteData.id;
            }
            var par_id = sc.getParId($par),
                attrs = {
                    "save-url": url,
                    "extra-data": angular.extend({
                        docId: sc.docId,
                        par: par_id,
                        isComment: true
                    }, data),
                    "options": {
                        showDelete: !options.isNew,
                        showImageUpload: true,
                        showPlugins: false,
                        touchDevice: mobile,
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
                    },
                    "after-save": 'handleNoteSave(saveData, extraData)',
                    "after-cancel": 'handleNoteCancel(extraData)',
                    "after-delete": 'handleNoteDelete(saveData, extraData)',
                    "preview-url": '/preview/' + sc.docId,
                    "delete-url": '/deleteNote',
                    "initial-text-url": initUrl
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
        
        sc.oldWidth = $($window).width();
        $($window).resize(function (e) {
            if (e.target === $window) {
                var newWidth = $($window).width();
                if (newWidth !== sc.oldWidth) {
                    sc.oldWidth = newWidth;
                    var selected = $('.par.lightselect, .par.selected');
                    if (selected.length > 0) {
                        selected[0].scrollIntoView();
                    }
                }
            }
        });

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
                if (downEvent !== null) {
                    if (func($(this), downEvent)) {
                        e.preventDefault();
                    }
                    downEvent = null;
                }
            });
        };

        sc.onMouseOver = function (className, func) {
            $document.on('mouseover', className, function (e) {
                if (func($(this), sc.fixPageCoords(e))) {
                    e.preventDefault();
                }
            });
        };

        sc.onMouseOut = function (className, func) {
            $document.on('mouseout', className, function (e) {
                if (func($(this), sc.fixPageCoords(e))) {
                    e.preventDefault();
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
            $rootScope.$broadcast('toggleQuestion');
            sc.par = $par;
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
            sc.beginUpdate();
        };

        sc.beginUpdate = function () {
            http.get('/getUpdatedPars/' + sc.docId)
                .success(function (data, status, headers, config) {
                    sc.updatePendingPars(data.changed_pars);
                })
                .error(function () {
                    $window.alert('Error occurred when getting updated paragraphs.')
                });
        };

        sc.getElementByRefId = function (ref) {
            return $(".par[ref-id='" + ref +  "']");
        };

        sc.removeDefaultPars = function () {
            sc.getElementByParId("null").remove();
        };

        sc.addSavedParToDom = function (data, extraData) {
            var $par;
            if (angular.isDefined(extraData['ref-id'])) {
                $par = sc.getElementByRefId(extraData['ref-id']);
            } else {
                $par = sc.getElementByParId(extraData.par);
            }

            // check if we were editing an area
            if (angular.isDefined(extraData.area_start) &&
                angular.isDefined(extraData.area_start) &&
                extraData.area_start !== null &&
                extraData.area_end !== null) {
                $par = sc.getElementByParId(extraData.area_start);

                // remove all but the first element of the area because it'll be used
                // when replacing
                var $endpar = sc.getElementByParId(extraData.area_end);
                $par.nextUntil($endpar).add($endpar).remove();
            }
            var newPars = $($compile(data.texts)(sc));
            $par.replaceWith(newPars);
            sc.processAllMathDelayed(newPars);
            http.defaults.headers.common.Version = data.version;
            sc.editing = false;
            sc.cancelArea();
            sc.removeDefaultPars();
            sc.markPageDirty();
            sc.beginUpdate();
        };

        sc.pendingUpdatesCount = function () {
            return Object.keys(sc.pendingUpdates).length;
        };

        sc.showUpdateDialog = function () {
            return !sc.hidePending && sc.pendingUpdatesCount() > 0;
        };

        sc.updatePendingPars = function (pars) {
            angular.extend(sc.pendingUpdates, pars);
            sc.hidePending = false;
            if (sc.pendingUpdatesCount() < 10) {
                sc.updatePending();
            }
        };

        sc.updatePending = function () {
            for (var key in sc.pendingUpdates) {
                if (sc.pendingUpdates.hasOwnProperty(key)) {
                    var $par = sc.getElementByParId(key);
                    var newPar = $($compile(sc.pendingUpdates[key])(sc));
                    $par.replaceWith(newPar);
                    sc.processAllMathDelayed(newPar);
                }
            }
            sc.pendingUpdates = {};
        };

        sc.isReference = function ($par) {
            return angular.isDefined($par.attr('ref-id'));
        };

        sc.markParRead = function ($this, $par) {
            var oldClass = $this.attr("class");
            $this.attr("class", "readline read");
            var par_id = sc.getParId($par);
            var data = {};
            if (sc.isReference($par)) {
                data = sc.getRefAttrs($par);
            }
            if ( !sc.selectedUser ) return true;
            if ( sc.selectedUser.name.indexOf("Anonymous") == 0 ) return true;  
            http.put('/read/' + sc.docId + '/' + par_id + '?_=' + Date.now(), data)
                .success(function (data, status, headers, config) {
                    sc.markPageDirty();
                }).error(function () {
                    $window.alert('Could not save the read marking.');
                    $this.attr("class", oldClass);
                });
            return true;
        };

        sc.onClick(".readline", function ($this, e) {
            return sc.markParRead($this, $this.parents('.par'));
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

        sc.setAreaAttr = function(area, attr, value) {
            var area_selector = "[data-area=" + area + "]";
            $(area_selector).css(attr, value);
        };

        sc.onClick(".areacollapse", function ($this, e) {
            $this.removeClass("areacollapse");
            var area_name = $this.parent().attr('data-area-start');
            console.log("Collapse " + area_name);
            sc.setAreaAttr(area_name, "display", "none");
            $this.addClass("areaexpand");
        });

        sc.onClick(".areaexpand", function ($this, e) {
            $this.removeClass("areaexpand");
            var area_name = $this.parent().attr('data-area-start');
            console.log("Expand " + area_name);
            sc.setAreaAttr(area_name, "display", "");
            $this.addClass("areacollapse");
        });

        sc.showNoteWindow = function (e, $par) {
            sc.toggleNoteEditor($par, {isNew: true});
        };

        sc.handleNoteCancel = function () {
            sc.editing = false;
        };

        sc.handleNoteDelete = function (saveData, extraData) {
            sc.addSavedParToDom(saveData, extraData);
        };

        sc.handleNoteSave = function (saveData, extraData) {
            sc.addSavedParToDom(saveData, extraData);
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
            else {
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

        sc.onClick(".note", function ($this, e) {
            if (!$this.hasClass('editable')) {
                sc.showDialog('You cannot edit this note.');
                return true;
            }
            sc.toggleNoteEditor($this.parents('.par'), {isNew: false, noteData: {id: $this.attr('note-id')}});
            return true;
        });

        sc.onClick(".questionAdded", function ($this, e) {
            var question = $this;
            var questionId = question[0].getAttribute('id');
            sc.showQuestion(questionId);
            sc.par = ($(question).parent().parent());
        });

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

        sc.getQuestionHtml = function (questions) {
            var questionImage = '/static/images/show-question-icon.png';
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
            $(".parContent").each(function () {
                var $p = $(this);
                $p.find('h1, h2, h3, h4, h5, h6').each(function () {
                    var $h = $(this);
                    var id = $h.attr('id');
                    if (angular.isDefined(id)) {
                        $h.append($("<a>", {
                            text: '#',
                            href: '#' + id,
                            class: 'headerlink',
                            title: 'Permanent link'
                        }));
                    }
                });
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
            timLogTime("getindex","view");
            http.get('/index/' + sc.docId)
                .success(function (data) {
                    timLogTime("getindex succ","view");
                    if (data.empty) {
                        sc.showIndex = false;
                    } else {
                        var indexElement = $(".index-sidebar .sideBarContainer");
                        $(indexElement).html(data);
                        sc.showIndex = true;
                    }
                    timLogTime("getindex done","view");
                }).error(function () {
                    console.log("Could not get index");
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

            var newState = sc.invertState(state);
            if (newState !== state) {
                sc.clearSelection();
            }
            return newState;
        };


        sc.onClick('.showContent', function ($this, e) {
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

        sc.$on("getQuestions", function () {
            if (sc.firstTimeQuestions) {
                sc.getQuestions();
                sc.firstTimeQuestions = false;
            }
        });

        sc.$on("closeQuestionPreview", function () {
            sc.showQuestionPreview = false;
        });

        // Load index, notes and read markings
        timLogTime("getList start","view");
        sc.setHeaderLinks();
        timLogTime("getList end","view");


        // If you add 'mousedown' to bind, scrolling upon opening the menu doesn't work on Android
        $('body,html').bind('scroll wheel DOMMouseScroll mousewheel', function (e) {
            if (e.which > 0 || e.type === "mousedown" || e.type === "mousewheel") {
                $("html,body").stop();
            }
        });

        if (sc.rights.editable) {
            sc.onClick(".addBottom", function ($this, e) {
                $(".actionButtons").remove();
                //var $par = $('.par').last();
                //return sc.showAddParagraphBelow(e, $par);
                return sc.showAddParagraphAbove(e, $(".addBottomContainer"));
            });
        }
        sc.processAllMath($('body'));

        sc.defaultAction = {func: sc.showOptionsWindow, desc: 'Show options window'};
        timLogTime("VieCtrl end","view");
        sc.selection = {start: null, end: null};
        sc.$watchGroup(['lectureMode', 'selection.start', 'selection.end', 'editing'], function (newValues, oldValues, scope) {
            sc.editorFunctions = sc.getEditorFunctions();
            if (sc.editing) {
                sc.notification = "Editor is already open.";
            } else {
                sc.notification = "";
            }
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

        sc.onMouseOver('.parlink', function ($this, e) {
            sc.over_reflink = true;

            var $par = $this.parent();
            var coords = {left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top};
            var params;

            try {
                params = {
                    docid: e.target.attributes['data-docid'].value,
                    parid: e.target.attributes['data-parid'].value
                };
            } catch (TypeError) {
                // The element was modified
                return;
            }

            sc.showRefPopup(e, $this, coords, params);
        });

        sc.onMouseOver('.ref-popup', function ($this, e) {
            sc.over_popup = true;
        });

        sc.onMouseOut('.ref-popup', function ($this, e) {
            sc.over_popup = false;
            sc.hideRefPopup();
        });

        sc.onMouseOut('.parlink', function ($this, e) {
            sc.over_reflink = false;
            sc.hideRefPopup();
        });

        sc.showRefPopup = function (e, $ref, coords, attrs) {
            var $popup = $('<ref-popup>');
            $popup.offset(coords);

            for (var attr in attrs) {
                if (attrs.hasOwnProperty(attr)) {
                    $popup.attr(attr, attrs[attr]);
                }
            }

            $ref.prepend($popup); // need to prepend to DOM before compiling
            $compile($popup[0])(sc);
            return $popup;
        };

        sc.hideRefPopup = function() {
            if (sc.over_reflink || sc.over_popup)
                return;

            $(".refPopup").remove();
        };

        sc.startArea = function (e, $par) {
            sc.selection.start = sc.getParId($par);
        };

        sc.cancelArea = function (e, $par) {
            sc.selection.start = null;
            sc.selection.end = null;
        };

        sc.nothing = function () {
        };

        sc.goToEditor = function (e, $par) {
            $('pareditor')[0].scrollIntoView();
        };

        sc.closeAndSave = function (e, $par) {
            $('pareditor').isolateScope().saveClicked();
            sc.showOptionsWindow(e, $par);
        };

        sc.closeWithoutSaving = function (e, $par) {
            $('pareditor').isolateScope().cancelClicked();
            sc.showOptionsWindow(e, $par);
        };

        sc.getEditorFunctions = function () {
            if (sc.editing) {
                return [
                    {func: sc.goToEditor, desc: 'Go to editor', show: true},
                    {func: sc.closeAndSave, desc: 'Close editor and save', show: true},
                    {func: sc.closeWithoutSaving, desc: 'Close editor and cancel', show: true},
                    {func: sc.nothing, desc: 'Close menu', show: true}
                ];
            } else {
                return [
                    {func: sc.showNoteWindow, desc: 'Comment/note', show: sc.rights.can_comment},
                    {func: sc.showEditWindow, desc: 'Edit', show: sc.rights.editable},
                    {func: sc.showAddParagraphAbove, desc: 'Add paragraph above', show: sc.rights.editable},
                    {func: sc.showAddParagraphBelow, desc: 'Add paragraph below', show: sc.rights.editable},
                    {func: sc.addQuestion, desc: 'Create question', show: sc.lectureMode && sc.rights.editable},
                    {
                        func: sc.startArea,
                        desc: 'Start selecting area',
                        show: sc.rights.editable && sc.selection.start === null
                    },
                    {
                        func: sc.beginAreaEditing,
                        desc: 'Edit area',
                        show: sc.selection.start !== null && sc.rights.editable
                    },
                    {func: sc.cancelArea, desc: 'Cancel area', show: sc.selection.start !== null},
                    {func: sc.nothing, desc: 'Close menu', show: true}
                ];
            }
        };



        sc.editorFunctions = sc.getEditorFunctions();

        sc.$storage = $localStorage.$default({
            defaultAction: null,
            noteAccess: 'everyone'
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
