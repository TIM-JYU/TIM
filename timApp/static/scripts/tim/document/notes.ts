import type {IScope} from "angular";
import $ from "jquery";
import type {Moment} from "moment";
import {openEditor} from "tim/editor/pareditorOpen";
import {getCurrentEditor} from "tim/editor/editorScope";
import type {IVisibilityVars} from "tim/timRoot";
import {getVisibilityVars} from "tim/timRoot";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import * as t from "io-ts";
import type {ParContext} from "tim/document/structure/parContext";
import {fromParents} from "tim/document/structure/create";
import type {IGroup} from "tim/user/IUser";
import {Users} from "tim/user/userService";
import type {IPluginInfoResponse} from "tim/editor/parCompiler";
import type {PareditorController} from "tim/editor/pareditor";
import type {IModalInstance} from "tim/ui/dialog";
import {documentglobals} from "tim/util/globals";
import {$compile, $http} from "tim/util/ngimport";
import {getViewName, isMobileDevice, TimStorage, to} from "tim/util/utils";
import type {
    EditPosition,
    IExtraData,
    IParResponse,
} from "tim/document/editing/edittypes";
import {EditType, extraDataForServer} from "tim/document/editing/edittypes";
import {onClick} from "tim/document/eventhandlers";
import {addElementToParagraphMargin} from "tim/document/parhelpers";
import {handleUnread, markParRead, ReadingType} from "tim/document/readings";
import type {ViewCtrl} from "tim/document/viewctrl";

export interface INoteEditorOptions {
    noteData?: {id: string};
    showDelete?: boolean;
}

export interface INote {
    id: number;
    doc_id: number;
    par_id: string;
    par_hash: string;
    content: string;
    created: Moment;
    modified?: Moment;
    access: "everyone" | "justme";
    html: string;
    tags: string[];
    usergroup: IGroup;
}

const NOTES_AUTHOR_SELF = "Just me";
const NOTES_AUTHOR_SELF_TITLE = "No one can answer to this note!";

export class NotesHandler {
    private hideVars: IVisibilityVars = getVisibilityVars();
    public sc: IScope;
    public viewctrl: ViewCtrl;
    public noteBadgePar: ParContext | undefined;
    public noteBadge: HTMLElement | undefined;
    private editorInstance?: IModalInstance<PareditorController>;
    public editor?: PareditorController;
    private editorLoad?: Promise<IModalInstance<PareditorController>>;

    constructor(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        onClick(".note", ($this, e) => {
            if (!$this.hasClass("editable")) {
                return false;
            }
            const id = $this.attr("note-id");
            if (!id) {
                showMessageDialog(
                    "Cannot edit this note; missing id attribute."
                );
                return;
            }
            const par = fromParents($this);
            this.toggleNoteEditor(par, {noteData: {id: id}});
            return true;
        });
    }

    async toggleNoteEditor(par: ParContext, options: INoteEditorOptions = {}) {
        if (getCurrentEditor() || this.editorInstance || this.editorLoad) {
            // Double-clicking a comment will trigger this message, but it seems like on Chrome, the message dialog
            // interferes with the editor dialog if it opens first, making the editor invisible.
            // Awaiting for editor render helps.
            await this.editorInstance?.rendered;
            await showMessageDialog("Some editor is already open.");
            return;
        }
        let caption = "Edit comment";
        if (!this.viewctrl.item.rights.can_comment) {
            return;
        }
        let url: string;
        let data;
        let initialText = "";
        let justMeLabel = NOTES_AUTHOR_SELF;
        let justMeTitle = NOTES_AUTHOR_SELF_TITLE;
        if (!options.noteData) {
            caption = "Add comment";
            url = "/postNote";
            data = {
                access:
                    new TimStorage("noteAccess", t.string).get() ?? "everyone",
                tags: {},
            };
        } else {
            url = "/editNote";
            const r = await to(
                $http.get<{text: string; extraData: INote}>(
                    "/note/" + options.noteData.id
                )
            );

            if (r.ok) {
                const notedata = r.result.data;
                initialText = notedata.text;
                const curUser = Users.getCurrent();
                if (curUser.name != notedata.extraData.usergroup.name) {
                    justMeLabel = `Just to user ${notedata.extraData.usergroup.name}`;
                    justMeTitle = `Only ${notedata.extraData.usergroup.name} will see the note!`;
                }
                data = {
                    id: options.noteData.id,
                    access: notedata.extraData.access,
                    tags: {},
                };
            } else {
                await showMessageDialog(
                    `Failed to open comment editor: ${r.result.data.error}`
                );
                return;
            }
        }

        const extraData: IExtraData = {
            docId: this.viewctrl.docId,
            isComment: true,
            par: par,
            ...data,
        };
        const params: EditPosition = {
            type: EditType.CommentAction,
            par: par,
        };
        this.viewctrl.editing = true;
        this.editorLoad = openEditor({
            extraData,
            initialText,
            defaultSize: "md",
            options: {
                caption,
                localSaveTag: "note",
                showDelete: !!options.noteData,
                showUpload: true,
                showImageUpload: true,
                showDocumentImport: false,
                showSettings: false,
                tags: [],
                showPlugins: false,
                touchDevice: isMobileDevice(),
                choices: [
                    {
                        desc: "Show note to:",
                        name: "access",
                        title: "Who can see the note. With everyone only teachers can see the user name",
                        opts: [
                            {
                                desc: "Everyone",
                                value: "everyone",
                                title: "User name is visible only for teachers",
                            },
                            {
                                desc: justMeLabel,
                                value: "justme",
                                title: justMeTitle,
                            },
                        ],
                    },
                ],
            },
            deleteCb: async () => {
                const r = await to(
                    $http.post<IParResponse>(`/deleteNote`, {
                        id: options.noteData?.id,
                        ctx: par.getJsonForServer(),
                        view: getViewName(),
                    })
                );
                if (!r.ok) {
                    return {error: r.result.data.error};
                } else {
                    this.viewctrl.editingHandler.addSavedParToDom(
                        r.result.data,
                        params
                    );
                }
                return {};
            },
            previewCb: async (text, proofread) => {
                const r = await to(
                    $http.post<IPluginInfoResponse>(
                        `/preview/${this.viewctrl.docId}`,
                        {text, proofread, ...extraDataForServer(extraData)}
                    )
                );
                if (!r.ok) {
                    throw Error("preview failed");
                }
                return r.result.data;
            },
            saveCb: async (text, eData) => {
                const r = await to(
                    $http.post<IParResponse>(url, {
                        id: options.noteData?.id,
                        text: text,
                        access: eData.access,
                        tags: eData.tags,
                        ctx: par.getJsonForServer(),
                        view: getViewName(),
                    })
                );
                if (!r.ok) {
                    return {error: r.result.data.error};
                } else {
                    this.viewctrl.editingHandler.addSavedParToDom(
                        r.result.data,
                        params
                    );
                }
                return {};
            },
            unreadCb: async () => {
                await handleUnread(params);
            },
        });
        this.editorInstance = await this.editorLoad;
        this.editor = await this.editorInstance.dialogInstance.promise;
        await to(this.editorInstance.result);
        this.editorInstance = undefined;
        this.editor = undefined;
        this.editorLoad = undefined;
        this.viewctrl.editing = false;
    }

    showNoteWindow(e: MouseEvent, par: ParContext) {
        this.toggleNoteEditor(par);
    }

    /**
     * Creates the note badge button (the button with letter 'C' on it).
     * @param par - Element where the badge needs to be attached
     */
    createNoteBadge(par: ParContext) {
        this.noteBadgePar = par;
        if (this.noteBadge) {
            // var parent = getElementParent(sc.noteBadge);
            // if ( !parent ) $compile(sc.noteBadge)(sc);
            return this.noteBadge;
        }

        const btn = document.createElement("a");
        btn.classList.add("note-badge");
        if (documentglobals().velpMode) {
            btn.classList.add("note-badge-with-velp");
        }
        btn.classList.add("timButton");
        btn.id = "noteBadge";
        this.noteBadge = btn;
        // btn.setAttribute("ng-click", "addNote()");
        btn.onclick = (event) => {
            event.stopPropagation();
            if (!this.viewctrl.editMenuOnLeft) {
                // TODO: Clean up types
                void this.viewctrl.parmenuHandler.openParMenu(
                    $(event.currentTarget as HTMLElement),
                    {
                        originalEvent: event,
                        pageY: event.pageY,
                        pageX: event.pageX,
                        type: "click",
                        target: event.currentTarget as HTMLElement,
                    }
                );
                return;
            }

            this.addNote();
        };
        this.updateBadgeState();
        $compile(btn)(this.sc);
        return btn;
    }

    addNote() {
        if (this.noteBadgePar) {
            this.toggleNoteEditor(this.noteBadgePar);
        } else {
            showMessageDialog("There is no note badge attached.");
        }
    }

    updateBadgeState() {
        if (!this.noteBadge) {
            return;
        }
        const btn = this.noteBadge;
        const editOnLeft = this.viewctrl.editMenuOnLeft;
        btn.innerHTML = editOnLeft
            ? "C"
            : `<i class="glyphicon glyphicon-pencil"></i>`;
        btn.title = editOnLeft
            ? $localize`Add comment/note`
            : $localize`Open edit menu`;
        if (!editOnLeft) {
            btn.classList.add("edit-menu-button");
        } else {
            btn.classList.remove("edit-menu-button");
        }
    }

    /**
     * Moves the note badge to the correct element.
     * @param par - Element where the badge needs to be attached
     * @param badgeY - Badge's position relative to the element where it's attached
     */
    updateNoteBadge(par: ParContext, badgeY: number) {
        // At the moment note badge is not usable for people who can't comment
        // Moreover, can_comment is false is user is not logged in, in which case other misc functions of notes editor
        // (e.g. marking block unread) is not usable either
        if (
            !this.viewctrl.item.rights.can_comment ||
            this.hideVars.noteBadgeButton ||
            this.viewctrl.actionsDisabled
        ) {
            return;
        }
        if (par.isInPreview()) {
            return;
        }
        markParRead(par, ReadingType.ClickPar);
        const btn = this.createNoteBadge(par);
        if (!this.viewctrl.editMenuOnLeft) {
            btn.style.transform = `translateY(${badgeY}px)`;
        } else {
            btn.style.transform = "";
        }
        addElementToParagraphMargin(par, btn);
    }

    /**
     * Removes the note badge and clears the element selection.
     * @param e - Current click event
     */
    clearNoteBadge(e: Event) {
        const btn = this.noteBadge;
        if (btn) {
            $(btn).remove();
        }

        if (e != null) {
            e.stopPropagation();
        }
    }
}
