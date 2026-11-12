import {ChangeDetectorRef, Component, ElementRef} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {DomSanitizer} from "@angular/platform-browser";
import {CsController} from "./csPlugin";

@Component({
    selector: "cs-text-runner",
    template: `
        <div [ngClass]="{csRunDiv: markup.borders}" [class.cs-has-header]="header" class="csTinyDiv" style="text-align: left;">
          @if (header) {
            <h4 [innerHTML]="header | purify"></h4>
          }
          @if (stem) {
            <span
              class="stem"
            [innerHTML]="stem | purify"></span>
          }
          @if (!noeditor || viewCode) {
            <input class="csTinyText no-popup-menu"
              [ngClass]="{warnFrame: isUnSaved()}"
              [size]="cols"
              [(ngModel)]="usercode"
              [placeholder]="placeholder"
              (ngModelChange)="onContentChange($event)"
              (keypress)="runCodeIfCR($event)"/>
          }
          @if (isRun) {
            <button
              [disabled]="isRunning || preventSave || (disableUnchanged && !isUnSaved())"
              class = "timButton"
              title="(Ctrl-S)"
              (click)="runCode();"
            [innerHTML]="buttonText() | purify"></button>
          }
          @if (undoButton && isUnSaved()) {
            <a href="#" title="{{undoTitle}}"
              (click)="tryResetChanges($event)">
              &nbsp;{{undoButton}}
            </a>
          }
          @if (savedText) {
            <span
              class="savedText"
            [innerHTML]="savedText | purify"></span>
          }
          @if (connectionErrorMessage) {
            <div class="error" style="font-size: 12px" [innerHTML]="connectionErrorMessage"></div>
          }
        
          <ng-container> &nbsp;&nbsp;</ng-container>@if (muokattu) {
          <a href="#"
          (click)="initCode(); $event.preventDefault()">{{resetText}}</a>
          }<ng-container>&nbsp;&nbsp; </ng-container>
          @if (result) {
            <pre class="console"
            >{{result}}</pre>
          }
          @if (runError) {
            <span class="csRunError"
            [style]="tinyErrorStyle">{{error}}</span>
          }
          @if (htmlresult) {
            <div class="htmlresult">
              <span [innerHTML]="htmlresult | purify"></span>
            </div>
          }
        </div>`,
    standalone: false,
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
