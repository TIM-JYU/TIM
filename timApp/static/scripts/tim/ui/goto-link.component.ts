import {Component, Input} from "@angular/core";

@Component({
    selector: "tim-goto-link",
    template: `
        <div class="goto-link-container">
        <button [disabled]="isGoing" (click)="startGoto()" class="timButton btn-primary">
            <ng-content></ng-content>
        </button>
        <span class="load-text" *ngIf="isGoing">
            <i class="glyphicon glyphicon-refresh glyphicon-refresh-animate"></i>
            {{waitText}}
        </span>
        </div>
    `,
    styleUrls: ["./goto-link.component.scss"],
})
export class GotoLinkComponent {
    @Input() href: string = "#";
    @Input() waitText: string = "Loading, please wait.";
    @Input() resetTime: number = 15;
    @Input() maxWait: number = 0;
    isGoing: boolean = false;

    startGoto() {
        this.isGoing = true;
        const waitTime = Math.random() * this.maxWait * 1000;
        const realResetTime = Math.max(this.resetTime * 1000, waitTime);

        setTimeout(() => {
            window.location.href = this.href;
        }, waitTime);

        setTimeout(() => {
           this.isGoing = false;
        }, realResetTime);
    }
}
