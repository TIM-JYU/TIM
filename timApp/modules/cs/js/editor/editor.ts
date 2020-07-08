/* eslint no-underscore-dangle: ["error", { "allow": ["content_", "minRows_", "maxRows_", "wrap_", "modes_", "fileIndex_", "files_", "modeIndex_", "mayAddFiles_"] }] */
/* eslint-disable @typescript-eslint/tslint/config -- decorators cause issues on setters */
import $ from "jquery";
import {Ace} from "ace-builds/src-noconflict/ace";
import {
    ViewChild,
    Component,
    Input,
    Output,
    EventEmitter,
    ChangeDetectorRef,
} from "@angular/core";

import {NormalEditorComponent} from "./normal";
import {AceEditorComponent} from "./ace";
import {ParsonsEditorComponent} from "./parsons";
import {countChars, getInt} from "../util";

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

    public id: ModeID;
    public text: string;

    constructor(id: ModeID, text?: string) {
        this.id = id;
        this.text = text ?? Mode.defaultTexts[id];
    }
}

export interface IEditor {
    content: string;

    setSelection?(): void;
    doWrap?(wrap: number): void;
    insert?(str: string): void;
}

export interface IEditorFile {
    path: string;
    content: string;
}

export interface IMultiEditor extends IEditor {
    activeFile: string;
    allFiles: IEditorFile[];

    setFiles(files: EditorFile[]): void;
    addFile(file: EditorFile): void;
    addFile(path: string, base?: string, languageMode?: string, content?: string): void;
    removeFile(filename: string): void;
    renameFile(path: string, oldPath?: string): void;
}

class EditorFile {
    path: string = "";
    base: string = ""; // starting content
    content?: string;
    oldContent: string = "";


    getContent(): string {
        return this.content ?? this.base;
    }
}

@Component({
    selector: "cs-jsparsons-editor",
    template: `NOT IMPLEMENTED`,
})
export class JSParsonsEditorComponent implements IEditor {
    content: string = "";
}

@Component({
    selector: "cs-editor",
    template: `
        <ng-container *ngIf="!cssPrint">
            <mat-tab-group *ngIf="files.length > 1" [(selectedIndex)]="fileIndex" animationDuration="0ms">
                <mat-tab *ngFor="let file of files; trackBy: trackByPath">
                    <ng-template mat-tab-label>
                        <div (click)="$event.preventDefault()">
                            {{file.path}}
                        </div>
                    </ng-template>
                </mat-tab>
            </mat-tab-group>
            <cs-normal-editor *ngIf="mode == Mode.Normal"
                    [minRows]="minRows_"
                    [maxRows]="maxRows_">
            </cs-normal-editor>
            <cs-parsons-editor *ngIf="mode == Mode.Parsons"
                    [shuffle]="parsonsShuffle"
                    [maxcheck]="parsonsMaxcheck"
                    [base]="base"
                    [words]="parsonsWords"
                    [styleWords]="parsonsStyleWords"
                    [notordermatters]="parsonsNotordermatters">
            </cs-parsons-editor>
            <cs-jsparsons-editor *ngIf="mode == Mode.JSParsons"></cs-jsparsons-editor>
            <cs-ace-editor *ngIf="mode == Mode.ACE"
                    [languageMode]="languageMode"
                    [minRows]="minRows_"
                    [maxRows]="maxRows_">
            </cs-ace-editor>
        </ng-container>
        <pre *ngIf="cssPrint"></pre>`,
})
export class EditorComponent implements IMultiEditor {
    static readonly defaultMode = Mode.ACE;
    Mode = Mode;
    console=console;

    private normalEditor?: NormalEditorComponent;
    private aceEditor?: AceEditorComponent;
    parsonsEditor?: ParsonsEditorComponent;

    @Output("content") private contentChange: EventEmitter<string> = new EventEmitter<string>();
    @Input() cssPrint: boolean = false; // TODO: what is this actually supposed to do
    minRows_: number = 1;
    maxRows_: number = 100;
    private wrap_?: {n: number, auto: boolean};

    parsonsShuffle_: boolean = false;
    @Input() parsonsMaxcheck?: number;
    @Input() parsonsNotordermatters: boolean = false;
    @Input() parsonsStyleWords: string = "";
    @Input() parsonsWords: boolean = false;

    private modeIndex_: number = -1;
    private mode_?: number;
    private modes_: Mode[] = [];

    // file.content is used for storing content if editor isn't available. Set to undefined after consuming.
    private files_: EditorFile[] = [new EditorFile()];
    private fileIndex_: number = 0;

    ngOnInit() {
        if (this.minRows_ < 1) {
            this.minRows_ = 1;
        }
        if (this.maxRows_ != -1 && this.maxRows_ < this.minRows_) {
            this.maxRows_ = this.minRows_;
        }
    }

    ngAfterViewInit() {
        this.mode = this.savedEditorMode ?? EditorComponent.defaultMode;
        this.showOtherEditor(this.mode);
    }

    ngDoCheck() {
        // TODO: make editors emit an event instead of ngDoCheck
        const content = this.content;
        if (content !== this.oldContent) {
            this.oldContent = content;

            if (this.wrap_?.auto) {
                this.doWrap();
            }

            this.contentChange.emit(content);
        }
    }

    private initEditor<T extends IEditor>(oldContent: string) {
        if (!this.editor) {
            this.content_ = oldContent;
        } else {
            this.content = oldContent;
            this.content_ = undefined;
        }
    }

    // For after ngIf sets the value
    @ViewChild(NormalEditorComponent) private set normalEditorViewSetter(component: NormalEditorComponent | undefined) {
        const oldContent = this.content;
        this.normalEditor = component;
        this.initEditor(oldContent);
    }
    @ViewChild(AceEditorComponent) private set aceEditorViewSetter(component: AceEditorComponent | undefined) {
        const oldContent = this.content;
        this.aceEditor = component;
        this.initEditor(oldContent);
    }
    @ViewChild(ParsonsEditorComponent) private set parsonsEditorViewSetter(component: ParsonsEditorComponent | undefined) {
        const oldContent = this.content;
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
    set parsonsShuffle(shuffle: boolean) {
        this.parsonsShuffle_ = shuffle;
    }
    @Input()
    set editorIndex(index: number) {
        this.mode = index;
    }
    get modes() {
        return this.modes_;
    }
    @Input()
    set modes(modes: Mode[]) {
        const mode = this.mode;
        this.modes_ = modes;
        this.mode = mode;
    }

    @Input()
    private set wrap(wrap: {n: number, auto: boolean} | undefined) {
        this.wrap_ = wrap;
        if (this.wrap_ && this.wrap_.n <= 0) {
            this.wrap_ = undefined;
        }
        if(this.wrap_?.auto) {
            this.doWrap();
        }
    }

    get fileIndex(): number {
        return this.fileIndex_;
    }
    set fileIndex(index: number) {
        this.setFileIndex(index);
    }
    private setFileIndex(index: number) {
        this.file.content = this.content;
        if (index < 0) {
            this.fileIndex_ = 0;
        } if (index >= this.files.length) {
            this.fileIndex = this.files.length-1;
        } else {
            this.fileIndex_ = index;
        }
        if (this.editor) {
            this.editor.content = this.file.content ?? this.file.base;
        }

    get files(): EditorFile[] {
        return this.files_;
    }
    set files(files: EditorFile[]) {
        if(files.length == 0) {
            this.files_ = [new EditorFile];
            this.fileIndex = 0;
        } else {
            this.files_ = files;
             // refresh index in case it is outside the new range
            this.setFileIndex(this.fileIndex);
        }
    }

    get allFiles(): IEditorFile[] {
        const out = this.files.map((f) => <IEditorFile>{path: f.path, content: f.content ?? f.base});
        out[this.fileIndex].content = this.content;
        return out;
    }

    get file(): EditorFile {
        return this.files[this.fileIndex];
    }

    get activeFile(): string {
        return this.file.path;
    }
    set activeFile(path: string) {
        const index = this.findFile(path);
        if (index != -1) {
            // TODO: what to do...
        } else {
            this.fileIndex = index;
        }
    }

    get modified(): boolean {
        return this.content != this.base;
    }

    get editor(): IEditor | undefined {
        switch(this.mode) {
            case Mode.Normal:
                return this.normalEditor;
            case Mode.ACE:
                return this.aceEditor;
            case Mode.Parsons:
                return this.parsonsEditor;
            case Mode.JSParsons:
                break;
        }
        return this.normalEditor;
    }

    get content() {
        return this.editor?.content ?? this.content_ ?? this.base;
    }
    set content(str: string) {
        if (this.editor) {
            this.editor.content = str;
            this.cdr.detectChanges();
        } else {
            this.file.content = str;
        }
    }

    get content_(): string | undefined {
        return this.file.content;
    }
    set content_(str: string | undefined) {
        this.file.content = str;
    }

    get oldContent(): string {
        return this.file.oldContent;
    }
    set oldContent(str: string) {
        this.file.oldContent = str;
    }

    get base() {
        return this.file.base;
    }
    @Input()
    set base(str: string) {
        this.file.base = str;
    }

    get languageMode(): string {
        return this.file.languageMode;
    }
    @Input()
    set languageMode(str: string) {
        this.file.languageMode = str;
    }

    get nextModeText(): string | undefined {
        if (this.modes.length <= 1) {
            return undefined;
        }
        return this.modes[(this.modeIndex+1) % this.modes.length].text;
    }

    get modeIndex(): number {
        return this.modeIndex_;
    }
    set modeIndex(index: number) {
        this.modeIndex_ = index;
        this.mode = this.mode; // save and make sure it is in range
    }

    get mode(): ModeID {
        if(this.modeIndex == -1 || this.modeIndex >= this.modes.length) {
            return -1;
        }
        return this.modes[this.modeIndex].id;
    }
    set mode(mode: ModeID) {
        if (mode == -1) {
            this.mode = this.savedEditorMode ?? EditorComponent.defaultMode; // TODO: make sure default is in modes
        } else {
            const index = this.modes.findIndex((e) => e.id == mode);
            if (index == -1) {
                this.modeIndex_ = this.modes.length;
                this.modes.push(new Mode(mode));
            } else {
                this.modeIndex_ = index;
            }
        }

        if (this.mode != Mode.Default && this.mode <= 1) {
            this.savedEditorMode = this.mode; // Why not save parsons?
        }
    }

    get savedEditorMode(): ModeID | null {
        let emode: ModeID | null = null;
         // TODO: change editorIndex into a list of priority?
        const emodestr = localStorage.getItem("editorIndex"); // TODO: change to editorMode?
        if (emodestr !== null) {
            emode = parseInt(emodestr, 10);
            if (emode == -1) {
                emode = null;
            }
        }
        return emode;
    }
    set savedEditorMode(mode: ModeID | null) {
        if (!mode) { return; }
        localStorage.setItem("editorIndex", mode.toString());
    }

    reset() {
        this.parsonsShuffle_ = true;
        this.content = this.base;
    }

    insert(str: string) {
        this.editor?.insert?.(str);
    }

    showOtherEditor(editorMode?: number) {
        if (editorMode != undefined) {
            this.mode = editorMode;
        } else {
            this.mode = this.modes[(this.modeIndex+1) % this.modes.length].id;
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
    addFile(path: string, base?: string, languageMode?: string, content?: string): void;
    addFile(fileorpath: string | EditorFile, base?: string, languageMode?: string, content?: string) {
        let file: EditorFile;
        if (fileorpath as any instanceof EditorFile) {
            file = fileorpath as EditorFile;
        } else {
            file = new EditorFile(fileorpath as string);
            if(base) { file.base = base; }
            if(languageMode) { file.languageMode = languageMode; }
            if(content) { file.content = content; }
        }

        const index = this.findFile(file.path);
        if (index == -1) {
            this.files.push(file);
        } else {
            this.files[index] = file;
        }
    }

    findFile(path: string): number {
        return this.files.findIndex((f) => f.path == path);
    }

    removeFile(path: string) {
        const index = this.findFile(path);
        if (index != -1) {
            this.files.splice(index, 1);
        }
    }

    renameFile(path: string, oldPath?: string) {
        if (!oldPath) {
            this.file.path = path;
            return;
        }

        const index =  this.findFile(oldPath);
        if (index != -1) {
            this.files[index].path = path;
        }
    }

    trackByPath(index: number, item: EditorFile) {
        return item.path;
    }
}
