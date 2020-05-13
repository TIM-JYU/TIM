import {Component, Input} from "@angular/core";

@Component({
    selector: "tim-goto-link",
    template: `
        <ng-template #content><ng-content></ng-content></ng-template>
        <button *ngIf="isButton; else linkStyle" [disabled]="isGoing" (click)="startGoto()" class="timButton">
                <ng-container *ngTemplateOutlet="content"></ng-container>
        </button>
        <ng-template #linkStyle>
            <a [class.disabled]="isGoing" (click)="startGoto()">
            <ng-container *ngTemplateOutlet="content"></ng-container>
        </a>
        </ng-template>
        <div class="load-text" *ngIf="isGoing">
            <tim-loading></tim-loading>
            <span>
                <ng-template *ngIf="waitText else defaultText">{{waitText}}</ng-template>
                <ng-template #defaultText i18n>Loading, please wait.</ng-template>
            </span>
        </div>
    `,
    styleUrls: ["./goto-link.component.scss"],
})
export class GotoLinkComponent {
    @Input() href = "#";
    @Input() waitText?: string;
    @Input() resetTime = 15;
    @Input() maxWait = 0;
    @Input() isButton = false;
    isGoing = false;

    startGoto() {
        this.isGoing = true;
        const waitTime = Math.random() * Math.max(this.maxWait, 0) * 1000;
        const realResetTime = Math.max(this.resetTime * 1000, waitTime);

        setTimeout(() => {
            window.location.href = this.href;
        }, waitTime);

        setTimeout(() => {
           this.isGoing = false;
        }, realResetTime);
    }
}
