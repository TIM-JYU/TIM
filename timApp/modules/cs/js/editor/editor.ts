/* eslint no-underscore-dangle: ["error", { "allow": ["content_", "minRows_", "maxRows_", "wrap_", "modes_", "fileIndex_", "files_", "modeIndex_", "mayAddFiles_"] }] */
import {
    ChangeDetectorRef,
    Component,
    EventEmitter,
    HostBinding,
    Input,
    Output,
    ViewChild,
} from "@angular/core";

import {TimStorage} from "tim/util/utils";
import * as t from "io-ts";
import type {IFile} from "../util/file-select";
import {getInt} from "../util/util";
import {ICsParsonsOptions} from "../cs-parsons/csparsons";
import {NormalEditorComponent} from "./normal";
import {AceEditorComponent} from "./ace";
import {ParsonsEditorComponent} from "./parsons";

type ModeID = number;

export class Mode {
    static readonly Default: ModeID = -1;
    static readonly Normal: ModeID = 0;
    static readonly ACE: ModeID = 1;
    static readonly Parsons: ModeID = 2;
    static readonly JSParsons: ModeID = 3;

    static readonly defaultTexts = {
        [Mode.Default]: "Default", // TODO: translations
        [Mode.Normal]: "Normal",
        [Mode.ACE]: "Highlight",
        [Mode.Parsons]: "Parsons",
        [Mode.JSParsons]: "JS Parsons",
    };

    static readonly modeClasses = {
        [Mode.Default]: "default",
        [Mode.Normal]: "normal",
        [Mode.ACE]: "ace",
        [Mode.Parsons]: "parsons",
        [Mode.JSParsons]: "jsparsons",
    };

    public id: ModeID;
    public text: string;

    constructor(id: ModeID, text?: string) {
        this.id = id;
        this.text = text ?? Mode.defaultTexts[id];
    }
}

export const CURSOR = "⁞";

export interface IEditor {
    content: string;

    setSelection?(start: number): void;
    doWrap?(wrap: number): void;
    insert?(str: string): void;
    setReadOnly(b: boolean): void;
    focus(): void;
    addFormulaEditorOpenHandler?(cb: () => void): void;
}

export interface IEditorFile {
    path: string;
    content: string;
    source?: string;
}

export interface IMultiEditor extends IEditor {
    activeFile: string | undefined;
    allFiles: IEditorFile[];

    setFiles(files: EditorFile[]): void;
    addFile(file: EditorFile): void;
    addFile(
        path: string,
        base?: string,
        languageMode?: string,
        content?: string
    ): void;
    removeFile(filename: string): void;
    renameFile(path: string, oldPath?: string): void;
    setFileContent(path: string, content: string): void;
}

export class EditorFile {
    path: string = "";
    base: string; // starting content
    content_?: string;
    oldContent?: string;
    languageMode?: string;
    canClose: boolean;
    canRename: boolean; // TODO: implement renaming
    canModify: boolean;
    placeholder?: string;
    source?: string;

    constructor(
        path?: string,
        base?: string,
        languageMode?: string,
        canClose?: boolean,
        canRename?: boolean,
        canModify?: boolean
    ) {
        this.path = path ?? "";
        this.base = base ?? "";
        this.languageMode = languageMode;
        this.canClose = !!canClose;
        this.canRename = !!canRename;
        this.canModify = canModify === undefined || canModify;
    }

    get content() {
        return this.content_ ?? this.oldContent ?? this.base;
    }
    set content(str: string) {
        this.content_ = str;
    }
}

@Component({
    selector: "cs-jsparsons-editor",
    template: `NOT IMPLEMENTED`,
})
export class JSParsonsEditorComponent implements IEditor {
    content: string = "";
    setReadOnly(b: boolean) {}
    focus() {}
}

@Component({
    selector: "cs-editor",
    template: `
        <ng-container *ngIf="files.length">
            <div *ngIf="showTabs" class="tab-list">
                <div *ngFor="let file of files; index as i; trackBy: trackByPath" class="tab-label file-tab" [ngClass]="{'tab-label-active': tabIndex == i}" (click)="tabIndex = i">
                    {{file.path}} {{file.canModify ? "" : "(read-only)"}}
                    <div class="close-wrapper">
                        <tim-close-button *ngIf="file.canClose" (click)="closeFile(i)"></tim-close-button>
                    </div>
                </div>
                <div *ngIf="canAddFile" class="tab-label" [ngClass]="{'tab-label-active': tabIndex == files.length}" (click)="tabIndex = files.length">
                    <span class="file-add-tab glyphicon glyphicon-plus"></span>
                </div>
            </div>
            <ng-container *ngIf="!addTabActive">
            <cs-normal-editor *ngIf="mode == Mode.Normal"
                    [minRows]="minRows_"
                    [maxRows]="maxRows_"
                    [placeholder]="file && file.placeholder ? file.placeholder : ''"
                    [disabled]="isDisabled"
                    [spellcheck]="spellcheck">
            </cs-normal-editor>
            <cs-parsons-editor *ngIf="mode == Mode.Parsons"
                    [base]="base" 
                    [parsonsOptions]="parsonsOptions"            
                    (change)="onEditorContentChanged($event)">
            </cs-parsons-editor>
            <cs-jsparsons-editor *ngIf="mode == Mode.JSParsons"></cs-jsparsons-editor>
            <cs-ace-editor *ngIf="mode == Mode.ACE"
                    [languageMode]="languageMode"
                    [minRows]="minRows_"
                    [maxRows]="maxRows_"
                    [placeholder]="file && file.placeholder ? file.placeholder : ''"
                    [disabled]="isDisabled">
            </cs-ace-editor>
            </ng-container>
            <div *ngIf="addTabActive" class="add-view">
                <file-select class="small" style="height: 4em;"
                        [multiple]="false"
                        [stem]="'Load a file (optional)'"
                        [dragAndDrop]="true"
                        (file)="onFileLoad($event)">
                </file-select>
                Filename:<input type="text" placeholder="Give a name here" [(ngModel)]="filenameInput">
                <br>
                <button class="timButton btn-sm"
                        (click)="clickAddFile()"
                        [attr.title]="addButtonTitle"
                        [disabled]="disableAddButton">Add</button>
            </div>
        </ng-container>`,
})
export class EditorComponent implements IMultiEditor {
    static readonly defaultMode = Mode.Normal;
    Mode = Mode;

    private normalEditor?: NormalEditorComponent;

    private aceEditor?: AceEditorComponent;
    parsonsEditor?: ParsonsEditorComponent;
    editorreadonly: boolean = false;

    @Input() disabled: boolean = false;
    @Input() spellcheck?: boolean;

    allowedPaths?: string[]; // undefined for all allowed
    maxFiles: number = 1;
    mayAddFiles_: boolean = false;

    @Output("content") private contentChange: EventEmitter<string> =
        new EventEmitter(true);
    @Output("close") private fileCloseEmitter: EventEmitter<{
        file: EditorFile;
        index: number;
    }> = new EventEmitter(true);
    minRows_: number = 1;
    maxRows_: number = 100;
    private wrap_?: {n: number; auto: boolean};

    @Input() parsonsOptions?: ICsParsonsOptions;

    private modeIndex_: number = -1;
    private modes_: Mode[] = [];

    // file.content is used for storing content if editor isn't available. Set to undefined after consuming.
    private files_: EditorFile[] = [new EditorFile()];
    private fileIndex_: number = 0;

    addTabActive: boolean = false;
    private loadedFile?: IFile;
    filenameInput: string = "";
    private editorIndexStorage = new TimStorage("editorIndex", t.number);
    formulaFunction = function () {};

    constructor(private cdr: ChangeDetectorRef) {
        this.showOtherEditor(
            this.savedEditorMode ?? EditorComponent.defaultMode
        );
    }

    setReadOnly(b: boolean) {
        this.editor?.setReadOnly(b);
        this.editorreadonly = b;
    }

    ngOnInit() {
        if (this.minRows_ < 1) {
            this.minRows_ = 1;
        }
        if (this.maxRows_ != -1 && this.maxRows_ < this.minRows_) {
            this.maxRows_ = this.minRows_;
        }
    }

    ngDoCheck() {
        // TODO: make editors emit an event instead of ngDoCheck
        if (this.mode == Mode.Parsons) {
            return;
        }
        const content = this.content;
        if (content !== this.oldContent) {
            this.oldContent = content;

            if (this.wrap_?.auto) {
                this.doWrap();
            }

            this.contentChange.emit(content);
        }
    }

    onEditorContentChanged(args: {content: string; init: boolean}) {
        if (!args.init && this.oldContent !== args.content) {
            this.contentChange.emit(args.content);
        }
        this.oldContent = args.content;
    }

    private initEditor(oldContent: string) {
        if (!this.editor) {
            this.content_ = oldContent;
        } else {
            this.editor.setReadOnly(this.editorreadonly);
            this.content = oldContent;
            this.content_ = undefined;
            this.addFormulaEditorOpenHandler(this.formulaFunction);
        }
    }

    // For after ngIf sets the value
    @ViewChild(NormalEditorComponent) private set normalEditorViewSetter(
        component: NormalEditorComponent | undefined
    ) {
        if (component == this.normalEditor) {
            return;
        }
        const oldContent = this.normalEditor?.content ?? this.content;
        this.normalEditor = component;
        this.initEditor(oldContent);
    }
    @ViewChild(AceEditorComponent) private set aceEditorViewSetter(
        component: AceEditorComponent | undefined
    ) {
        if (component == this.aceEditor) {
            return;
        }
        const oldContent = this.aceEditor?.content ?? this.content;
        this.aceEditor = component;
        this.initEditor(oldContent);
    }
    @ViewChild(ParsonsEditorComponent) private set parsonsEditorViewSetter(
        component: ParsonsEditorComponent | undefined
    ) {
        if (component == this.aceEditor) {
            return;
        }
        const oldContent = this.parsonsEditor?.content ?? this.content;
        this.parsonsEditor = component;
        this.initEditor(oldContent);
    }

    @Input()
    set minRows(rows: number | string) {
        this.minRows_ = getInt(rows) ?? 1;
    }
    @Input()
    set maxRows(rows: number | string) {
        this.maxRows_ = getInt(rows) ?? 100;
    }
    @Input()
    set editorIndex(index: number) {
        this.showOtherEditor(index);
    }
    get modes() {
        return this.modes_;
    }
    @Input()
    set modes(modes: Mode[]) {
        const mode = this.mode;
        const index = modes.findIndex((m) => m.id == mode);
        this.modes_ = modes;
        if (index != -1) {
            this.modeIndex_ = index;
        } else {
            this.modeIndex_ = 0;
        }
        this.cdr.detectChanges();
    }

    @Input()
    set wrap(wrap: {n: number; auto: boolean} | undefined) {
        this.wrap_ = wrap;
        if (this.wrap_ && this.wrap_.n <= 0) {
            this.wrap_ = undefined;
        }
        if (this.wrap_?.auto) {
            this.doWrap();
        }
    }

    get tabIndex(): number {
        return this.addTabActive ? this.files.length : this.fileIndex;
    }
    set tabIndex(index: number) {
        this.addTabActive = this.canAddFile && index == this.files.length;
        if (!this.addTabActive) {
            this.setFileIndex(index);
        }
    }

    get fileIndex(): number {
        return this.fileIndex_;
    }
    set fileIndex(index: number) {
        this.setFileIndex(index);
    }
    private setFileIndex(index: number) {
        if (this.file) {
            this.file.content = this.content;
        }
        this.fileIndex_ = this.clampIndex(index);
        this.content = this.file?.content ?? "";
    }
    private clampIndex(index: number) {
        if (index < 0) {
            return 0;
        }
        if (index >= this.files.length) {
            return this.files.length - 1;
        } else {
            return index;
        }
    }

    get files(): EditorFile[] {
        return this.files_;
    }
    set files(files: EditorFile[]) {
        this.files_ = files;
        if (files.length == 0) {
            this.fileIndex = 0;
        } else {
            const clampedIndex = this.clampIndex(this.fileIndex);
            if (files[clampedIndex]) {
                this.content =
                    files[clampedIndex].content ?? files[clampedIndex].base;
            }
        }
        // refresh index in case it is outside the new range
        this.setFileIndex(this.fileIndex);
        this.maxFiles = this.mayAddFiles ? -1 : this.files.length;
        this.cdr.detectChanges();
    }

    get isDisabled(): boolean {
        return this.disabled || !this.file?.canModify;
    }

    get mayAddFiles() {
        return this.mayAddFiles_;
    }
    set mayAddFiles(b: boolean) {
        if (b != this.mayAddFiles_) {
            this.maxFiles = b ? -1 : this.files.length;
        }
        this.mayAddFiles_ = b;
    }

    get allFiles(): IEditorFile[] {
        const out = this.files.map((f) => ({
            source: f.source,
            path: f.path,
            content: f.content,
        }));
        if (out.length > 0) {
            out[this.fileIndex].content = this.content;
        }
        return out;
    }

    get file(): EditorFile | undefined {
        return this.files[this.fileIndex];
    }

    get activeFile(): string | undefined {
        return this.file?.path;
    }
    set activeFile(path: string | undefined) {
        if (path) {
            const index = this.findFile(path);
            if (index != -1) {
                this.fileIndex = index;
                return;
            }
        }
    }

    get modified(): boolean {
        return !this.isDisabled && this.content != this.base;
    }

    get editor(): IEditor | undefined {
        switch (this.mode) {
            case Mode.Normal:
                return this.normalEditor;
            case Mode.ACE:
                return this.aceEditor;
            case Mode.Parsons:
                return this.parsonsEditor;
            case Mode.JSParsons:
                break;
        }
        return undefined;
    }

    get content() {
        return this.editor?.content ?? this.file?.content ?? this.base;
    }
    set content(str: string) {
        if (this.editor) {
            this.editor.content = str;
        } else if (this.file) {
            this.file.content = str;
        }
    }

    get content_(): string | undefined {
        return this.file?.content_;
    }
    set content_(str: string | undefined) {
        if (this.file) {
            this.file.content_ = str;
        }
    }

    get oldContent(): string {
        return this.file?.oldContent ?? "";
    }
    set oldContent(str: string) {
        if (this.file) {
            this.file.oldContent = str;
        }
    }

    get base() {
        return this.file?.base ?? "";
    }
    @Input()
    set base(str: string) {
        if (this.file) {
            this.file.base = str;
        }
    }

    get languageMode(): string {
        return this.file?.languageMode ?? "text";
    }
    set languageMode(str: string) {
        if (this.file) {
            this.file.languageMode = str;
        }
    }

    get nextModeText(): string | undefined {
        if (this.modes.length <= 1) {
            return undefined;
        }
        return this.modes[(this.modeIndex + 1) % this.modes.length].text;
    }

    get modeIndex(): number {
        return this.modeIndex_;
    }
    set modeIndex(index: number) {
        this.modeIndex_ = index;
        this.mode = this.mode; // save and make sure it is in range
    }

    @HostBinding("class") get className() {
        return `editor-${Mode.modeClasses[this.mode]}`;
    }

    get mode(): ModeID {
        if (this.modeIndex == -1 || this.modeIndex >= this.modes.length) {
            return -1;
        }
        const mode = this.modes[this.modeIndex].id;
        if (this.isDisabled) {
            if (mode == Mode.Normal || mode == Mode.ACE) {
                return mode;
            }
            return Mode.Normal;
        }
        return mode;
    }
    set mode(mode: ModeID) {
        if (mode == -1) {
            this.mode = this.savedEditorMode ?? EditorComponent.defaultMode; // TODO: make sure default is in modes
        } else {
            const index = this.modes.findIndex((e) => e.id == mode);
            if (index != -1) {
                this.modeIndex_ = index;
            }
        }

        if (this.mode != Mode.Default && this.mode <= 1) {
            this.savedEditorMode = this.mode; // Why not save parsons?
        }
    }

    get savedEditorMode(): ModeID | null {
        let emode: ModeID | null;
        // TODO: change editorIndex into a list of priority?
        emode = this.editorIndexStorage.get() ?? null; // TODO: change to editorMode?
        if (emode !== null) {
            if (emode == -1) {
                emode = null;
            }
        }
        return emode;
    }
    set savedEditorMode(mode: ModeID | null) {
        if (mode === null) {
            return;
        }
        this.editorIndexStorage.set(mode);
    }

    set placeholder(str: string | undefined) {
        if (this.file) {
            this.file.placeholder = str;
        }
    }

    get showTabs() {
        return this.files.length > 1 || this.canAddFile || this.file?.canClose; // TODO: show when upload is also available. TODO: show always?
    }

    get canAddFile() {
        return this.maxFiles == -1 || this.files.length < this.maxFiles;
    }

    get disableAddButton(): boolean {
        return (
            !this.filenameInput ||
            (!!this.allowedPaths &&
                !this.allowedPaths.includes(this.filenameInput))
        );
    }

    get addButtonTitle(): string | null {
        if (!this.filenameInput) {
            return "Filename cannot be empty!";
        } else if (
            this.allowedPaths &&
            !this.allowedPaths.includes(this.filenameInput)
        ) {
            return `Filename ${this.filenameInput} is not allowed!`;
        }
        return null;
    }

    reset() {
        if (this.parsonsOptions) {
            this.parsonsOptions.shuffle = true;
        }
        this.content = this.base;
    }

    insert(str: string) {
        this.editor?.insert?.(str);
    }

    showOtherEditor(editorMode?: number) {
        if (editorMode == -1) {
            this.mode = this.savedEditorMode ?? EditorComponent.defaultMode;
        } else if (editorMode != undefined) {
            const index = this.modes.findIndex((e) => e.id == editorMode);
            if (index == -1) {
                this.modes.push(new Mode(editorMode));
            }
            this.mode = editorMode;
        } else {
            this.mode = this.modes[(this.modeIndex + 1) % this.modes.length].id;
        }
    }

    doWrap() {
        if (this.wrap_) {
            this.editor?.doWrap?.(this.wrap_.n);
        }
    }

    setFiles(files: EditorFile[]) {
        this.files = files;
    }

    addFile(file: EditorFile): void;
    addFile(
        path: string,
        base?: string,
        languageMode?: string,
        content?: string
    ): void;
    addFile(
        fileorpath: EditorFile | string,
        base?: string,
        languageMode?: string,
        content?: string
    ) {
        let file: EditorFile;
        if (fileorpath instanceof EditorFile) {
            file = fileorpath;
        } else {
            file = new EditorFile(fileorpath);
            if (base) {
                file.base = base;
            }
            if (languageMode) {
                file.languageMode = languageMode;
            }
            if (content) {
                file.content = content;
            }
        }

        const index = this.findFile(file.path);
        if (index == -1) {
            this.files.push(file);
            this.cdr.detectChanges();
        } else {
            this.files[index] = file;
        }
        this.addTabActive = false;
    }

    findFile(path: string): number {
        return this.files.findIndex((f) => f.path == path);
    }

    removeFileByIndex(index: number) {
        if (this.clampIndex(index) != index) {
            return;
        }
        let nindex = this.fileIndex;
        if (nindex > index) {
            nindex--;
        }
        if (this.fileIndex == index) {
            if (this.files.length - 1 == this.fileIndex) {
                nindex = this.clampIndex(nindex - 1);
                this.setFileIndex(nindex);
            } else {
                this.setFileIndex(nindex + 1);
            }
        }
        this.files.splice(index, 1);
        this.fileIndex_ = nindex;
        if (this.files.length == 0) {
            this.addTabActive = this.canAddFile;
        }
    }

    removeFile(path: string) {
        const index = this.findFile(path);
        if (index != -1) {
            this.removeFileByIndex(index);
        }
    }

    closeFile(index: number) {
        const file = this.files[index];
        this.removeFileByIndex(index);
        this.cdr.detectChanges();
        this.fileCloseEmitter.emit({file: file, index: index});
    }

    renameFile(path: string, oldPath?: string) {
        if (!oldPath) {
            if (this.file) {
                this.file.path = path;
            }
            return;
        }

        const index = this.findFile(oldPath);
        if (index != -1) {
            this.files[index].path = path;
        }
    }

    setFileContent(path: string, content: string) {
        const index = this.findFile(path);
        if (index == this.fileIndex) {
            this.content = content;
        } else if (index != -1) {
            this.files[index].content = content;
        } else {
            this.addFile(path);
            this.setFileContent(path, content);
        }
    }

    onFileLoad(file: IFile) {
        this.loadedFile = file;
        if (!this.filenameInput) {
            this.filenameInput = file.realName;
        }
    }

    clickAddFile() {
        const filename = this.filenameInput;
        if (this.allowedPaths && !this.allowedPaths.includes(filename)) {
            return;
        }
        const content = this.loadedFile?.content ?? "";
        const file = new EditorFile(filename, content, undefined, true);
        file.content = content;
        this.addFile(file);
        this.tabIndex = this.files.length - 1;
        this.filenameInput = "";
    }

    trackByPath(index: number, item: EditorFile) {
        return item.path;
    }

    focus() {
        this.editor?.focus();
    }
    addFormulaEditorOpenHandler(cb: () => void): void {
        this.formulaFunction = cb;
        if (this.editor?.addFormulaEditorOpenHandler) {
            this.editor.addFormulaEditorOpenHandler(cb);
        }
    }
}
