import {Component, ChangeDetectorRef, ElementRef} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {DomSanitizer} from "@angular/platform-browser";
import {CsController} from "./csPlugin";

@Component({
    selector: "cs-text-runner",
    template: `
        <div [ngClass]="{csRunDiv: markup.borders}" class="csTinyDiv" style="text-align: left;">
            <h4 *ngIf="header" [innerHTML]="header | purify"></h4>
            <span *ngIf="stem"
                class="stem"
                [innerHTML]="stem | purify"></span>
            <input class="csTinyText no-popup-menu"
                [ngClass]="{warnFrame: isUnSaved()}"
                *ngIf="!noeditor || viewCode"
                [size]="cols"
                [(ngModel)]="usercode"
                [placeholder]="placeholder"
                (ngModelChange)="onContentChange($event)"
                (keypress)="runCodeIfCR($event)"/>
            <button *ngIf="isRun"
                    [disabled]="isRunning || preventSave || (disableUnchanged && !isUnSaved())"
                    class = "timButton"
                    title="(Ctrl-S)"
                    (click)="runCode();"
                    [innerHTML]="buttonText() | purify"></button>
            <a href="#" *ngIf="undoButton && isUnSaved()" title="{{undoTitle}}"
                    (click)="tryResetChanges($event)">
                    &nbsp;{{undoButton}}
                    </a>
            <span *ngIf="savedText"
                        class="savedText"
                        [innerHTML]="savedText | purify"></span>
            <div *ngIf="connectionErrorMessage" class="error" style="font-size: 12px" [innerHTML]="connectionErrorMessage"></div>

            &nbsp;&nbsp;<a href="#"
                        *ngIf="muokattu"
                        (click)="initCode(); $event.preventDefault()">{{resetText}}</a>&nbsp;&nbsp;
            <pre class="console"
                *ngIf="result">{{result}}</pre>
            <span class="csRunError"
                *ngIf="runError"
                [style]="tinyErrorStyle">{{error}}</span>
            <div class="htmlresult" *ngIf="htmlresult">
                <span [innerHTML]="htmlresult | purify"></span>
            </div>
        </div>`,
})
export class CsTextComponent extends CsController {
    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer, cdr);
    }
}
