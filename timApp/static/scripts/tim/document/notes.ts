import {IScope} from "angular";
import $ from "jquery";
import {Moment} from "moment";
import {openEditor} from "tim/editor/pareditorOpen";
import {getCurrentEditor} from "tim/editor/editorScope";
import {getVisibilityVars, IVisibilityVars} from "tim/timRoot";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import * as t from "io-ts";
import {ParContext} from "tim/document/structure/parContext";
import {fromParents} from "tim/document/structure/create";
import {IPluginInfoResponse} from "../editor/parCompiler";
import {PareditorController} from "../editor/pareditor";
import {IModalInstance} from "../ui/dialog";
import {documentglobals} from "../util/globals";
import {$compile, $http} from "../util/ngimport";
import {isMobileDevice, TimStorage, to} from "../util/utils";
import {
    EditPosition,
    EditType,
    extraDataForServer,
    IExtraData,
    IParResponse,
} from "./editing/edittypes";
import {onClick} from "./eventhandlers";
import {addElementToParagraphMargin} from "./parhelpers";
import {handleUnread, markParRead, ReadingType} from "./readings";
import {ViewCtrl} from "./viewctrl";

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
}

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
                showImageUpload: true,
                showSettings: false,
                tags: [],
                showPlugins: false,
                touchDevice: isMobileDevice(),
                choices: [
                    {
                        desc: "Show note to:",
                        name: "access",
                        title:
                            "Who can see the note. With everyone only teachers can see the user name",
                        opts: [
                            {
                                desc: "Everyone",
                                value: "everyone",
                                title: "User name is visible only for teachers",
                            },
                            {
                                desc: "Just me",
                                value: "justme",
                                title: "No one can answer to this note!",
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

        const btn = document.createElement("input");
        btn.type = "button";
        btn.classList.add("note-badge");
        if (documentglobals().velpMode) {
            btn.classList.add("note-badge-with-velp");
        }
        btn.classList.add("timButton");
        btn.value = "C";
        btn.title = "Add comment/note";
        btn.id = "noteBadge";
        this.noteBadge = btn;
        // btn.setAttribute("ng-click", "addNote()");
        btn.onclick = ($event) => {
            $event.stopPropagation();
            this.addNote();
        };
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

    /**
     * Moves the note badge to the correct element.
     * @param par - Element where the badge needs to be attached
     */
    updateNoteBadge(par: ParContext) {
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
        if (!par.isActionable()) {
            return;
        }
        markParRead(par, ReadingType.ClickPar);
        addElementToParagraphMargin(par, this.createNoteBadge(par));
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
