/* globals angular, $, timLogTime */

var timApp = angular.module('timApp');

timApp.defineNotes = function (sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
    "use strict";

    sc.toggleNoteEditor = function ($par_or_area, options) {
        var caption = 'Edit comment';
        var touch = typeof('ontouchstart' in window || navigator.msMaxTouchPoints) !== 'undefined';
        var mobile = touch && (window.screen.width < 1200);
        if (!sc.item.rights.can_comment) {
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
        var $par = sc.getFirstPar($par_or_area);
        var par_id = sc.getFirstParId($par_or_area),
            attrs = {
                "save-url": url,
                "extra-data": angular.extend({
                    docId: sc.docId,
                    par: par_id,
                    isComment: true
                }, data),
                "options": {
                    localSaveTag: "note",
                    showDelete: !options.isNew,
                    showImageUpload: true,
                    showPlugins: false,
                    touchDevice: mobile,
                    /*
                     tags: [
                     {name: 'difficult', desc: 'The text is difficult to understand'},
                     {name: 'unclear', desc: 'The text is unclear'}
                     ],
                     */
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
                "initial-text-url": initUrl,
                "unread-url": '/unread/' + sc.docId
            };

        sc.toggleEditor($par, options, attrs, caption, "pareditor");
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

    sc.showNoteWindow = function (e, $par) {
        sc.toggleNoteEditor($par, {isNew: true});
    };

    sc.onClick(".note", function ($this, e) {
        if (!$this.hasClass('editable')) {
            sc.showDialog('You cannot edit this note.');
            return true;
        }
        sc.toggleNoteEditor($this.parents('.par'), {isNew: false, noteData: {id: $this.attr('note-id')}});
        return true;
    });

    /**
     * Creates the note badge button (the button with letter 'C' on it).
     * @method createNoteBadge
     * @param $par - Element where the badge needs to be attached
     */
    sc.createNoteBadge = function ($par) {
        sc.noteBadgePar = $par;
        if (sc.noteBadge) {
            //var parent = getElementParent(sc.noteBadge);
            //if ( !parent ) $compile(sc.noteBadge)(sc);
            return sc.noteBadge;
        }

        var btn = document.createElement("input");
        btn.type = "button";
        btn.classList.add("note-badge");
        if (window.velpMode)
            btn.classList.add("note-badge-with-velp");
        btn.classList.add("timButton");
        btn.value = "C";
        btn.title = "Add comment/note";
        btn.id = "noteBadge";
        sc.noteBadge = btn;
        // btn.setAttribute("ng-click", "addNote()");
        btn.onclick = function ($event) {
            $event.stopPropagation();
            sc.toggleNoteEditor(sc.noteBadgePar, {isNew: true});
        };
        $compile(btn)(sc);
        return btn;
    };


    sc.addNote = function () {
        // sc.clearNoteBadge(null);
        sc.toggleNoteEditor(sc.noteBadgePar, {isNew: true});
    };


    sc.setNotePadge = function ($event) {
        $event.stopPropagation();
        var $par = $($event.target);
        if (!$par.hasClass("par")) $par = $par.parents('.par');
        sc.updateNoteBadge($par);
    };

    /**
     * Moves the note badge to the correct element.
     * @method updateNoteBadge
     * @param $par - Element where the badge needs to be attached
     */
    sc.updateNoteBadge = function ($par) {
        if (!$par) return null;
        if ($par.parents('.previewcontent').length > 0) {
            return;
        }
        sc.markParRead($par, sc.readingTypes.clickPar);
        var newElement = $par[0];
        if (!newElement) return null;
        sc.addElementToParagraphMargin(newElement, sc.createNoteBadge($par));
    };

    /**
     * Removes the note badge and clears the element selection.
     * @param e - Current click event
     */
    sc.clearNoteBadge = function (e) {
        var btn = sc.noteBadge;
        if (btn) {
            var parent = sc.getElementParent(btn);
            if (parent) parent.removeChild(btn);
        }

        if (e !== null) {
            e.stopPropagation();
        }
    };

    /**
     * Adds an element to the paragraph margin.
     * @method addElementToParagraphMargin
     * @param par - Paragraph where the element will be added
     * @param el - Element to add
     */
    sc.addElementToParagraphMargin = function (par, el) {
        var container = par.getElementsByClassName("notes");
        if (container.length > 0) {
            container[0].appendChild(el);
        } else {
            container = document.createElement("div");
            container.classList.add("notes");
            container.appendChild(el);
            par.appendChild(container);
        }
    };
};
