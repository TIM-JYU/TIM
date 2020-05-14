import {Component, Input} from "@angular/core";

@Component({
    selector: "tim-goto-link",
    template: `
        <a [class.disabled]="isGoing" [class.timButton]="isButton" (click)="startGoto()">
            <ng-content></ng-content>
        </a>
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
    @Input() target = "_self";
    isGoing = false;

    startGoto() {
        this.isGoing = true;
        const waitTime = Math.random() * Math.max(this.maxWait, 0) * 1000;
        const realResetTime = Math.max(this.resetTime * 1000, waitTime);

        setTimeout(() => {
            window.open(this.href, this.target);
        }, waitTime);

        setTimeout(() => {
           this.isGoing = false;
        }, realResetTime);
    }
}
