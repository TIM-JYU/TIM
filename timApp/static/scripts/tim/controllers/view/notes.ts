import angular, {IScope} from "angular";
import $ from "jquery";
import {$compile, $window} from "../../ngimport";
import {addElementToParagraphMargin, getFirstPar, getFirstParId} from "./parhelpers";
import {onClick} from "./eventhandlers";
import {markParRead, readingTypes} from "./readings";
import {getElementParent} from "../../utils";
import {ViewCtrl} from "./ViewCtrl";

export class NotesHandler {
    public sc: IScope;
    public viewctrl: ViewCtrl;
    public noteBadgePar: JQuery;
    public noteBadge: HTMLElement;

    initNotes(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        onClick(".note", ($this, e) => {
            if (!$this.hasClass("editable")) {
                return false;
            }
            this.toggleNoteEditor($this.parents(".par"), {isNew: false, noteData: {id: $this.attr("note-id")}});
            return true;
        });
    }

    toggleNoteEditor($parOrArea, options) {
        let caption = "Edit comment";
        const touch = typeof ("ontouchstart" in window || navigator.msMaxTouchPoints) !== "undefined";
        const mobile = touch && (window.screen.width < 1200);
        if (!this.viewctrl.item.rights.can_comment) {
            return;
        }
        let url,
            data, initUrl;
        if (options.isNew) {
            caption = "Add comment";
            url = "/postNote";
            data = {
                access: this.viewctrl.$storage.noteAccess,
                tags: {
                    difficult: false,
                    unclear: false,
                },
            };
            initUrl = null;
        } else {
            url = "/editNote";
            data = {};
            initUrl = "/note/" + options.noteData.id;
        }
        const $par = getFirstPar($parOrArea);
        const par_id = getFirstParId($parOrArea),
            attrs = {
                "save-url": url,
                "extra-data": angular.extend({
                    docId: this.viewctrl.docId,
                    par: par_id,
                    isComment: true,
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
                            desc: "Show note to:",
                            name: "access",
                            opts: [
                                {desc: "Everyone", value: "everyone"},
                                {desc: "Just me", value: "justme"},
                            ],
                        }],
                    },
                    destroyAfterSave: true,
                },
                "after-save": "$ctrl.handleNoteSave(saveData, extraData)",
                "after-cancel": "$ctrl.handleNoteCancel(extraData)",
                "after-delete": "$ctrl.handleNoteDelete(saveData, extraData)",
                "preview-url": "/preview/" + this.viewctrl.docId,
                "delete-url": "/deleteNote",
                "initial-text-url": initUrl,
                "unread-url": "/unread/" + this.viewctrl.docId,
            };

        this.viewctrl.toggleEditor($par, options, attrs, caption, "pareditor");
    }

    handleNoteCancel() {
        this.viewctrl.editing = false;
    }

    handleNoteDelete(saveData, extraData) {
        this.viewctrl.addSavedParToDom(saveData, extraData);
    }

    handleNoteSave(saveData, extraData) {
        this.viewctrl.addSavedParToDom(saveData, extraData);
    }

    showNoteWindow(e, $par) {
        this.toggleNoteEditor($par, {isNew: true});
    }

    /**
     * Creates the note badge button (the button with letter 'C' on it).
     * @method createNoteBadge
     * @param $par - Element where the badge needs to be attached
     */
    createNoteBadge($par) {
        this.noteBadgePar = $par;
        if (this.noteBadge) {
            //var parent = getElementParent(sc.noteBadge);
            //if ( !parent ) $compile(sc.noteBadge)(sc);
            return this.noteBadge;
        }

        const btn = document.createElement("input");
        btn.type = "button";
        btn.classList.add("note-badge");
        if ($window.velpMode)
            btn.classList.add("note-badge-with-velp");
        btn.classList.add("timButton");
        btn.value = "C";
        btn.title = "Add comment/note";
        btn.id = "noteBadge";
        this.noteBadge = btn;
        // btn.setAttribute("ng-click", "addNote()");
        btn.onclick = ($event) => {
            $event.stopPropagation();
            this.toggleNoteEditor(this.noteBadgePar, {isNew: true});
        };
        $compile(btn)(this.sc);
        return btn;
    }

    addNote() {
        // sc.clearNoteBadge(null);
        this.toggleNoteEditor(this.noteBadgePar, {isNew: true});
    }

    setNotePadge($event) {
        $event.stopPropagation();
        let $par = $($event.target);
        if (!$par.hasClass("par")) $par = $par.parents(".par");
        this.updateNoteBadge($par);
    }

    /**
     * Moves the note badge to the correct element.
     * @method updateNoteBadge
     * @param $par - Element where the badge needs to be attached
     */
    updateNoteBadge($par) {
        if (!$par) return null;
        if ($par.parents(".previewcontent").length > 0) {
            return;
        }
        markParRead($par, readingTypes.clickPar);
        const newElement = $par[0];
        if (!newElement) return null;
        addElementToParagraphMargin(newElement, this.createNoteBadge($par));
    }

    /**
     * Removes the note badge and clears the element selection.
     * @param e - Current click event
     */
    clearNoteBadge(e) {
        const btn = this.noteBadge;
        if (btn) {
            const parent = getElementParent(btn);
            if (parent) parent.removeChild(btn);
        }

        if (e !== null) {
            e.stopPropagation();
        }
    }
}
