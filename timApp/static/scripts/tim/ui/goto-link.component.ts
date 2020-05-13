import {Component, Input} from "@angular/core";

@Component({
    selector: "tim-goto-link",
    template: `
        <div class="goto-link-container">
        <button [disabled]="isGoing" (click)="startGoto()" class="timButton btn-primary">
            <ng-content></ng-content>
        </button>
        <span class="load-text" *ngIf="isGoing">
            <tim-loading></tim-loading>
            {{waitText}}
        </span>
        </div>
    `,
    styleUrls: ["./goto-link.component.scss"],
})
export class GotoLinkComponent {
    @Input() href = "#";
    @Input() waitText = "Loading, please wait.";
    @Input() resetTime = 15;
    @Input() maxWait = 0;
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
