import {Component, Input, Output, EventEmitter} from "@angular/core";

@Component({
    selector: "bootstrap-panel",
    template: `
        <div class="panel panel-default" [hidden]="show === false">
            <div class="panel-heading">
                {{ title }}
                <a *ngIf="showClose">
                    <tim-close-button (click)="close()"></tim-close-button>
                </a>
            </div>
            <div class="panel-body">
                <ng-content></ng-content>
            </div>
        </div>
    `,
})
export class BootstrapPanelComponent {
    @Output() closed = new EventEmitter<void>();
    @Input() show?: boolean;
    @Input() showClose?: boolean;
    @Input() title?: string;

    close() {
        this.show = false;
        this.closed.emit();
    }
}
