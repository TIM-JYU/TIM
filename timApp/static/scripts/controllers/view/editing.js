/* globals angular, $ */

var timApp = angular.module('timApp');

timApp.defineEditing = function (sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
    "use strict";
    sc.EDITOR_CLASS = "editorArea";
    sc.EDITOR_CLASS_DOT = "." + sc.EDITOR_CLASS;
    sc.editing = false;

    sc.toggleParEditor = function ($pars, options) {
        var $par = sc.getFirstPar($pars);
        var area_start, area_end;
        var caption = 'Add paragraph';
        var touch = typeof('ontouchstart' in window || navigator.msMaxTouchPoints) !== 'undefined';
        var mobile = touch && (window.screen.width < 1200);
        var url;
        var par_id = sc.getParId($par);
        var par_next_id = sc.getParId(sc.getNextPar($par));
        if (par_next_id === "HELP_PAR")
            par_next_id = null;

        if ($pars.length > 1) {
            area_start = sc.getParId($par);
            area_end = sc.getLastParId($pars);
            url = '/postParagraph/';
            options.showDelete = true;
        } else {
            // TODO: Use same route (postParagraph) for both cases, determine logic based on given parameters
            if (par_id === "HELP_PAR" || $pars.hasClass("new")) {
                url = '/newParagraph/';
                options.showDelete = false;
            } else {
                url = '/postParagraph/';
                options.showDelete = true;
            }

            area_start = options.area ? sc.getParId(sc.selection.start) : null;
            area_end = options.area ? sc.getParId(sc.selection.end) : null;
        }


        if (options.area) {
            area_end = sc.getParId(sc.selection.end);
            area_start = sc.getParId(sc.selection.start);
        } else {
            area_start = null;
            area_end = null;
        }

        var tags = {};
        var markread = $window.localStorage.getItem("markread") || false;
        tags["markread"] = markread === "true";


        var attrs = {
            "save-url": url,
            "extra-data": {
                docId: sc.docId, // current document id
                par: par_id, // the id of paragraph on which the editor was opened
                par_next: par_next_id, // the id of the paragraph that follows par or null if par is the last one
                area_start: area_start,
                area_end: area_end,
                tags: tags
            },
            "options": {
                localSaveTag: "par",
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
            "delete-url": '/deleteParagraph/' + sc.docId,
            "unread-url": '/unread/' + sc.docId
        };
        if (options.showDelete) {
            caption = 'Edit paragraph';
            if (par_id !== "HELP_PAR")
                attrs["initial-text-url"] = '/getBlock/' + sc.docId + "/" + par_id;
        }
        sc.toggleEditor($par, options, attrs, caption, "pareditor");
    };

    sc.toggleEditor = function ($par, options, attrs, caption, directive) {
        if (sc.isReference($par)) {
            angular.extend(attrs['extra-data'], sc.getRefAttrs($par));
        }
        Object.keys(attrs).forEach(function (key, index) {
            if (typeof attrs[key] === 'object' && attrs[key] !== null) {
                attrs[key] = JSON.stringify(attrs[key]);
            }
        });
        if ($par.children(sc.EDITOR_CLASS_DOT).length) {
            $par.children().remove(sc.EDITOR_CLASS_DOT);
            sc.editing = false;
        } else {
            $(sc.EDITOR_CLASS_DOT).remove();

            var createEditor = function (attrs) {
                var $div = $("<" + directive + ">", {class: sc.EDITOR_CLASS}).attr(attrs);
                $div.attr('tim-draggable-fixed', '');
                if (caption) {
                    $div.attr('caption', caption);
                }
                $par.append($div);
                $compile($div[0])(sc);
                sc.editing = true;
                $timeout(function () {
                    sc.goToEditor();
                }, 0);
            };

            if (options.showDelete) {
                $(".par.new").remove();
            }
            createEditor(attrs);
        }
    };

    sc.editSettingsPars = function (recursiveCall) {
        var pars = [];
        $(".par").each(function () {
            if (sc.getParAttributes($(this)).hasOwnProperty("settings")) {
                pars.push(this);
            }
        });
        if (pars.length === 0) {
            if (recursiveCall) {
                throw 'Faulty recursion stopped, there should be a settings paragraph already';
            }
            var $first = $(".par:first");
            var par_next = sc.getParId($first);
            if (par_next === "HELP_PAR") {
                par_next = null;
            }
            $first.before(sc.createNewPar());
            var parToReplace = "NEW_PAR";
            http.post('/newParagraph/', {
                "text": '``` {settings=""}\nexample:\n```',
                "docId": sc.docId,
                "par_next": par_next
            }).success(function (data, status, headers, config) {
                sc.addSavedParToDom(data, {par: parToReplace});
                sc.editSettingsPars(true);
            }).error(function (data, status, headers, config) {
                $window.alert(data.error);
            });
        }
        else if (pars.length === 1) {
            sc.toggleParEditor($(pars[0]), {area: false});
        }
        else {
            var start = pars[0];
            var end = pars[pars.length - 1];
            sc.selection.start = $(start);
            sc.selection.end = $(end);
            $(pars).addClass('selected');
            sc.toggleParEditor($(pars), {area: true});
            $(pars).removeClass('selected');
            sc.cancelArea();
        }
    };

    sc.showEditWindow = function (e, $par) {
        $(".par.new").remove();
        sc.toggleParEditor($par, {area: false});
    };

    sc.beginAreaEditing = function (e, $par) {
        $(".par.new").remove();
        sc.toggleParEditor($par, {area: true});
    };

    sc.createNewPar = function () {
        return $("<div>", {class: "par new", id: 'NEW_PAR', attrs: '{}'})
            .append($("<div>", {class: "parContent"}).html('New paragraph'));
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

    sc.showAddParagraphAbove = function (e, $par) {
        var $newpar = sc.createNewPar();
        $par.before($newpar);
        sc.toggleParEditor($newpar, {area: false});
    };

    sc.showAddParagraphBelow = function (e, $par) {
        var $newpar = sc.createNewPar();
        $par.after($newpar);
        sc.toggleParEditor($newpar, {area: false});
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

        var $newPars = $($compile(data.texts)(sc));

        if ($window.editMode === 'area')
            $newPars.find('.editline').removeClass('editline').addClass('editline-disabled');

        $par.replaceWith($newPars);
        sc.processAllMathDelayed($newPars);
        sc.docVersion = data.version;
        sc.editing = false;
        sc.cancelArea();
        sc.removeDefaultPars();
        sc.markPageDirty();
        sc.beginUpdate();
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
        } else if (sc.selection.start !== null && $window.editMode) {
            return [
                {
                    func: sc.beginAreaEditing,
                    desc: 'Edit area',
                    show: true
                },
                {func: sc.nameArea, desc: 'Name area', show: true},
                {func: sc.cutArea, desc: 'Cut area', show: true},
                {func: sc.copyArea, desc: 'Copy area', show: true},
                {func: sc.cancelArea, desc: 'Cancel area', show: true},
                {func: sc.nothing, desc: 'Close menu', show: true}
            ];
        } else {
            return [
                {func: sc.showNoteWindow, desc: 'Comment/note', show: sc.item.rights.can_comment},
                {func: sc.showEditWindow, desc: 'Edit', show: sc.item.rights.editable},
                {func: sc.cutPar, desc: 'Cut paragraph', show: $window.editMode === 'par'},
                {func: sc.copyPar, desc: 'Copy paragraph', show: $window.editMode !== 'area'},
                //{func: sc.cutArea, desc: 'Cut area', show: $window.editMode === 'area'},
                //{func: sc.copyArea, desc: 'Copy area', show: $window.editMode === 'area'},
                {
                    func: sc.showPasteMenu,
                    desc: 'Paste...',
                    show: $window.editMode && (sc.allowPasteRef || sc.allowPasteContent)
                },
                {func: sc.showMoveMenu, desc: 'Move here...', show: $window.allowMove},
                {func: sc.removeAreaMarking, desc: 'Remove area marking', show: $window.editMode === 'area'},
                {func: sc.showAddParagraphAbove, desc: 'Add paragraph above', show: sc.item.rights.editable},
                {func: sc.addQuestionQst, desc: 'Add question above', show: sc.lectureMode && sc.item.rights.editable},
                {func: sc.editQst, desc: 'Edit question', show: sc.lectureMode && sc.item.rights.editable},
                {func: sc.addQuestion, desc: 'Create lecture question', show: sc.lectureMode && sc.item.rights.editable},
                {
                    func: sc.startArea,
                    desc: 'Start selecting area',
                    show: $window.editMode === 'par' && sc.selection.start === null
                },
                {func: sc.nothing, desc: 'Close menu', show: true}
            ];
        }
    };

    sc.showAddParagraphMenu = function (e, $par_or_area, coords) {
        sc.showPopupMenu(e, $par_or_area, coords, {actions: 'addParagraphFunctions'});
    };

    sc.getAddParagraphFunctions = function () {
        return [
            {func: sc.showAddParagraphAbove, desc: 'Above', show: true},
            {func: sc.showAddParagraphBelow, desc: 'Below', show: true},
            {func: sc.nothing, desc: 'Cancel', show: true}
        ];
    };

    sc.removeDefaultPars = function () {
        sc.getElementByParId("HELP_PAR").remove();
    };

    sc.extendSelection = function ($par, allowShrink) {
        if (sc.selection.start === null) {
            sc.selection.start = $par;
            sc.selection.end = $par;
        } else {
            var n = sc.selection.pars.length;
            var startIndex = sc.getParIndex(sc.selection.pars.eq(0));
            var endIndex = sc.getParIndex(sc.selection.pars.eq(n - 1));
            var areaLength = endIndex - startIndex + 1;
            var newIndex = sc.getParIndex($par);

            if (newIndex < startIndex) {
                sc.selection.start = $par;
            } else if (newIndex > endIndex) {
                sc.selection.end = $par;
            } else if (allowShrink && areaLength > 1 && newIndex === startIndex) {
                sc.selection.start = $(sc.selection.pars[1]);
            } else if (allowShrink && areaLength > 1 && newIndex === endIndex) {
                sc.selection.end = $(sc.selection.pars[n - 2]);
            }
        }
    };

    if (sc.item.rights.editable) {
        sc.onClick(".addBottom", function ($this, e) {
            $(".actionButtons").remove();
            //var $par = $('.par').last();
            //return sc.showAddParagraphBelow(e, $par);
            return sc.showAddParagraphAbove(e, $(".addBottomContainer"));
        });

        sc.onClick(".pasteBottom", function ($this, e) {
            $(".actionButtons").remove();
            sc.pasteAbove(e, $(".addBottomContainer"), false);
        });

        sc.onClick(".pasteRefBottom", function ($this, e) {
            $(".actionButtons").remove();
            sc.pasteAbove(e, $(".addBottomContainer"), true);
        });
    }

    sc.addParagraphFunctions = sc.getAddParagraphFunctions();

    sc.selection = {start: null, end: null};

    sc.$watchGroup(['selection.start', 'selection.end'], function (newValues, oldValues, scope) {
        $('.par.lightselect').removeClass('lightselect');
        $('.par.selected').removeClass('selected');
        $('.par.marked').removeClass('marked');
        if (sc.selection.start !== null) {
            var $start = sc.selection.start;
            if (sc.selection.end !== null && !sc.selection.end.is(sc.selection.start)) {
                var $end = sc.selection.end;
                sc.selection.pars = sc.getPars($start, $end);
            } else {
                sc.selection.pars = $start;
            }
            sc.selection.pars.addClass('marked');
        }
    });
};
