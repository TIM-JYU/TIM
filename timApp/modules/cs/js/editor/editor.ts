/* es-lint:disable */
import $ from "jquery";
import {Ace} from "ace-builds/src-noconflict/ace";
import {
    ViewChild,
    Component,
    Input,
    Output,
    EventEmitter,
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
    wrap?(wrap: number): void;
    insert?(str: string): void;
}

// TODO: ?
@Component({
    selector: "cs-jsparsons-editor",
    template: `NOT IMPLEMENTED`,
})
export class JSParsonsEditorComponent implements IEditor {
    content: string = "";
}

//`/* && !$ctrl.hide.changed*/+`
@Component({
    selector: "cs-editor",
    template: `
        <ng-container *ngIf="!cssPrint">
            <cs-normal-editor *ngIf="mode == Mode.Normal"
                    [minRows]="minRows_"
                    [maxRows]="maxRows_">
            </cs-normal-editor>
            <cs-ace-editor *ngIf="mode == Mode.ACE" 
                    [languageMode]="languageMode" 
                    [minRows]="minRows_" 
                    [maxRows]="maxRows_">
            </cs-ace-editor>
            <cs-parsons-editor *ngIf="mode == Mode.Parsons" 
                    [shuffle]="parsonsShuffle_"
                    [maxcheck]="parsonsMaxcheck"
                    [base]="base"
                    [words]="parsonsWords"
                    [styleWords]="parsonsStyleWords"
                    [notordermatters]="parsonsNotordermatters">
            </cs-parsons-editor>
            <cs-jsparsons-editor *ngIf="mode == Mode.JSParsons"></cs-jsparsons-editor>
        </ng-container>
        <pre *ngIf="cssPrint"></pre>`,
})
export class EditorComponent {
    static readonly defaultMode = Mode.ACE; 
    Mode = Mode;
    
    private normalEditor?: NormalEditorComponent;
    private aceEditor?: AceEditorComponent;
    parsonsEditor?: ParsonsEditorComponent;
    
    @Output("content") private contentChange: EventEmitter<string> = new EventEmitter<string>();
    @Input() base: string = ""; // starting content
    private oldContent: string = "";
    @Input() cssPrint: boolean = false; // TODO: what is this actually supposed to do
    minRows_: number = 1;
    maxRows_: number = 100;
    private wrap_?: {n: number, auto: boolean};
    @Input() languageMode: string = "text";
    
    parsonsShuffle_: boolean = false;
    @Input() parsonsMaxcheck?: number;
    @Input() parsonsNotordermatters: boolean = false;
    @Input() parsonsStyleWords: string = "";
    @Input() parsonsWords: boolean = false;
    
    //private editorMode!: number;
    private modeIndex_: number = -1;
    private mode_?: number;
    private modes_: Mode[] = [];
    // for storing content if editor isn't available. Set to undefined after comsuming.
    private content_?: string;
    
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
        const content = this.content; // don't want to read it multiple times
        if (content !== this.oldContent) {
            this.oldContent = content;
            
            if (this.wrap_?.auto) { 
                this.doWrap();
            }
            
            this.contentChange.emit(content);
        }
    }
    
    private initEditor<T extends IEditor>(oldContent: string | undefined) {
        if (!this.editor) {
            this.content_ = oldContent;
        } else {
            this.content = oldContent ?? this.base;
            this.content_ = undefined;
        }
    }
    
    // For after ngIf sets the value
    @ViewChild(NormalEditorComponent) private set normalEditorViewSetter(component: NormalEditorComponent | undefined) {
        const oldContent = this.editor?.content ?? this.content_;
        this.normalEditor = component;
        this.initEditor(oldContent);
    }
    @ViewChild(AceEditorComponent) private set aceEditorViewSetter(component: AceEditorComponent | undefined) {
        const oldContent = this.editor?.content ?? this.content_;
        this.aceEditor = component;
        this.initEditor(oldContent);
    }
    @ViewChild(ParsonsEditorComponent) private set parsonsEditorViewSetter(component: ParsonsEditorComponent | undefined) {
        const oldContent = this.editor?.content ?? this.content_;
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
            case Mode.JSParsons: // TODO: ?
                break;
        }
        return this.normalEditor;
    }
    
    get content() {
        return this.editor?.content ?? "";
    }
    set content(str: string) {
        if (this.editor) {
            this.editor.content = str;
        } else {
            this.content_ = str;
        }
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
    
    get savedEditorMode(): ModeID | null { // TODO: change editorIndex to a list of priority?
        let emode: ModeID | null = null;
        const emodestr = localStorage.getItem("editorIndex"); // TODO: change to editorMode?
        if (emodestr !== null) {
            emode = parseInt(emodestr);
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
            this.editor?.wrap?.(this.wrap_.n);
        }
    }
}
