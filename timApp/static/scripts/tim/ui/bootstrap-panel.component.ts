import {
    Component,
    EventEmitter,
    Input,
    Output,
    TemplateRef,
} from "@angular/core";
import {AlertSeverity} from "tim/ui/formErrorMessage";

@Component({
    selector: "bootstrap-panel",
    template: `
        <div class="panel panel-{{ severity ? severity : 'default' }}" [hidden]="show === false">
            <div class="panel-heading" [attr.id]="anchorID">
                <ng-container *ngIf="title">{{ title }}</ng-container>
                <ng-container *ngIf="titleTemplate">
                    <ng-container *ngTemplateOutlet="titleTemplate"></ng-container>
                </ng-container>
                <a *ngIf="showClose">
                    <tim-close-button (click)="close()"></tim-close-button>
                </a>
                <span class="headerlink" *ngIf="showHeadingAnchors && anchorID">
                    <a href="#{{ anchorID }}" title="Permanent link to paragraph" class="">
                        <span class="header-anchor">#</span>
                    </a>
                </span>
            </div>
            <div class="panel-body">
                <ng-content></ng-content>
            </div>
        </div>
    `,
})
export class BootstrapPanelComponent {
    @Input() severity?: AlertSeverity;
    @Output() closed = new EventEmitter<void>();
    @Input() show?: boolean;
    @Input() showClose?: boolean;
    @Input() title?: string;
    @Input() titleTemplate?: TemplateRef<unknown>;
    @Input() anchorID?: string;
    @Input() showHeadingAnchors?: boolean = false;

    close() {
        this.show = false;
        this.closed.emit();
    }
}

@Component({
    selector: "bootstrap-form-panel",
    template: `
        <bootstrap-panel [severity]="severity" [show]="show" [showClose]="showClose" [title]="title" [titleTemplate]="titleTemplate" (closed)="closed.emit($event)" [anchorID]="anchorID" [showHeadingAnchors]="showHeadingAnchors">
            <form>
                <fieldset [disabled]="disabled">
                    <ng-content></ng-content>
                </fieldset>
            </form>
        </bootstrap-panel>
    `,
    styles: [` :host { display: block; } `],
})
export class BootstrapFormPanelComponent {
    @Input() disabled: boolean = false;
    @Input() severity?: AlertSeverity;
    @Output() closed = new EventEmitter<void>();
    @Input() show?: boolean;
    @Input() showClose?: boolean;
    @Input() title?: string;
    @Input() titleTemplate?: TemplateRef<unknown>;
    @Input() anchorID?: string;
    @Input() showHeadingAnchors?: boolean = false;
}
