import {IController} from "angular";
import {Component, ChangeDetectorRef, ElementRef} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {DomSanitizer} from "@angular/platform-browser";
import * as t from "io-ts";
import {CsBase, Example, ConsolePWD, languageTypes} from "./csPlugin";

function trackByIndex(index: number, o: unknown) {
    return index;
}

@Component({
    selector: "cs-console",
    template: `
    <div class="web-console no-popup-menu {{currentSize}} " (keydown)="handleKey($event)">
        <code class="console-output">
            <div class="console-output-elem" *ngFor="let item of history; trackBy: trackByIndex">
                <span class="console-oldinput">
                    <span class="console-in">{{item.istem}}</span>
                    <span class="console-userInput">{{item.input}}</span>
                </span>
                <span class="console-oldresponse">
                    <span *ngIf="!isShell">  <br/>
                        <span class="console-out">{{item.ostem}}</span>
                    </span>
                    <span class="console-response" [ngClass]="{error: item.error}">
                        <span [innerHTML]="item.response"></span>
                    </span>
                    <!-- Double span since [textContent] eats the innermost one -->
                </span>
            </div>
            <span class="console-expander-sym" (click)="toggleSize()"></span>
        </code>
        <div class="console-examples-box">
            <span class="examples-title" (click)="examplesVisible=!examplesVisible">    ▼ example expressions ▲</span>
            <div>Click to load:</div>
            <ul>
                <li *ngFor="let example of examples; index as i">
                    <a (click)="loadExample(i)" title="example.expr">{{example.title||example.expr}}</a>
                </li>
            </ul>
        </div>
        <div class="console-curIndex" *ngIf="isShell">{{pwd}}</div>
        <span class="console-curIndex">in_{{cursor}}</span>
        <input type="text" placeholder="type expressions here"
                        class="console-input"
                        [(ngModel)]="currentInput"/>
        &nbsp;
        <div class="console-buttons">
            <button (click)="up()">↑</button>&nbsp;<button (click)="down()">↓</button>&nbsp;<button (click)="handler()">Enter</button>&nbsp;
        </div>
    </div>`,
})
export class CsConsoleComponent extends CsBase implements IController {
    trackByIndex = trackByIndex;
    // isShell: boolean; method
    cursor: number;
    currentSize: string;
    // isHtml: boolean;
    oldpwd!: string;
    currentInput: string;
    pwd!: string;
    // byCode: string; method
    // content: AttrType;
    examples: Array<t.TypeOf<typeof Example>>;
    examplesVisible: boolean = true;
    history: Array<{
        istem: string;
        ostem: string;
        input: string;
        response: string;
        error?: string;
    }>;
    // savestate: string;
    // path: string;
    // type: string;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        public cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer);
        this.examples = [];
        this.history = [];
        this.currentSize = "normal";
        this.currentInput = "";
        this.cursor = this.history.length; // this.history.length means new input is last command.
    }

    $postLink() {
        // nothing to do
    }

    get isShell() {
        return languageTypes.getRunType(this.type, "") === "shell";
    }

    get savestate() {
        return this.markup.savestate;
    }

    ngOnInit() {
        super.ngOnInit();

        // This block could be re-used

        // End of generally re-usable TIM stuff
        if (this.markup.examples) {
            this.examples = this.markup.examples;
        }

        this.pwd = ConsolePWD.getPWD(this);
        this.oldpwd = this.pwd;
        if (this.usercode === "" && this.byCode) {
            this.usercode = this.byCode.split("\n")[0];
        }
        this.currentInput = this.usercode;
        if (this.isShell) {
            ConsolePWD.register(this);
        }
    }

    setPWD(pwd: string) {
        this.pwd = pwd;
    }

    loadExample(i: number) {
        this.currentInput = this.examples[i].expr;
        this.focusOnInput();
    }

    focusOnInput() {
        const el: HTMLInputElement | null = this.getRootElement().querySelector(
            ".console-input"
        );
        if (el) {
            el.focus();
        }
    }

    async handler() {
        const url = this.pluginMeta.getAnswerUrl();
        const ty = languageTypes.getRunType(this.type, "shell");
        const ucode = this.currentInput;
        const isInput = false;
        const uargs = "";
        const uinput = "";

        const r = await this.httpPut<{
            web: {pwd?: string; error?: string; console?: string};
        }>(url, {
            input: {
                usercode: ucode,
                userinput: uinput,
                isInput: isInput,
                userargs: uargs,
                type: ty,
            },
        });
        if (r.ok) {
            const data = r.result;
            let s = "";
            this.oldpwd = this.pwd;
            if (data.web.pwd) {
                ConsolePWD.setPWD(data.web.pwd, this);
            }
            if (data.web.error) {
                s = data.web.error;
                s = "<pre>" + s + "</pre>";
            } else {
                s = data.web.console ?? "";
                if (!this.markup.isHtml) {
                    s = "<pre>" + s + "</pre>";
                }
            }
            this.submit(s);
        } else {
            console.log(["protocol error", r.result.error]);
            this.submit("Endless loop?");
        }
    }

    toggleSize() {
        if (this.currentSize === "normal") {
            this.currentSize = "enlarged";
        } else {
            this.currentSize = "normal";
        }
    }

    async submit(result: string) {
        this.history.push({
            istem: this.isShell
                ? this.history.length + " " + this.oldpwd + "$"
                : "in_" + this.history.length + ": ",
            ostem: this.isShell ? "" : "out_" + this.history.length + ": ",
            input: this.currentInput,
            response: result,
        });
        this.currentInput = "";
        this.cursor = this.history.length;
        await new Promise((resolve) => {
            setTimeout(resolve);
        });
        const el = this.getRootElement().querySelector(".console-output");
        if (el) {
            el.scrollTop = el.scrollHeight;
        }
    }

    load() {
        if (this.cursor >= this.history.length) {
            this.currentInput = "";
            this.cursor = this.history.length;
            return;
        }
        const norm = Math.min(
            this.history.length - 1,
            Math.max(0, this.cursor)
        );
        this.currentInput = this.history[norm].input;
        this.cursor = norm;
    }

    up() {
        if (!this.cursor) {
            return;
        }
        this.cursor--;
        this.load();
    }

    down() {
        this.cursor++;
        this.load();
    }

    handleKey(ev: KeyboardEvent) {
        if (ev.which === 13) {
            this.handler();
        }
        if (ev.which === 40) {
            this.down();
        }
        if (ev.which === 38) {
            this.up();
        }
    }
}
