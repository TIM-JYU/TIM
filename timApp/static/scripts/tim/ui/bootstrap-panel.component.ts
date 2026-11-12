import {
    Component,
    EventEmitter,
    Input,
    Output,
    TemplateRef,
    ChangeDetectionStrategy,
} from "@angular/core";
import {AlertSeverity} from "tim/ui/formErrorMessage";

@Component({
    selector: "bootstrap-panel",
    template: `
        <div class="panel panel-{{ severity ? severity : 'default' }}" [hidden]="show === false">
          <div class="panel-heading" [attr.id]="anchorId">
            @if (title) {
              {{ title }}
            }
            @if (titleTemplate) {
              <ng-container *ngTemplateOutlet="titleTemplate"></ng-container>
            }
            @if (showClose) {
              <a>
                <tim-close-button (click)="close()"></tim-close-button>
              </a>
            }
            @if (showHeadingAnchors && anchorId) {
              <span class="headerlink">
                <a href="#{{ anchorId }}" title="Permanent link to paragraph" class="">
                  <span class="header-anchor">#</span>
                </a>
              </span>
            }
          </div>
          <div class="panel-body">
            <ng-content></ng-content>
          </div>
        </div>
        `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class BootstrapPanelComponent {
    @Input() severity?: AlertSeverity;
    @Output() closed = new EventEmitter<void>();
    @Input() show?: boolean;
    @Input() showClose?: boolean;
    @Input() title?: string;
    @Input() titleTemplate?: TemplateRef<unknown>;
    @Input() anchorId?: string;
    @Input() showHeadingAnchors?: boolean = false;

    close() {
        this.show = false;
        this.closed.emit();
    }
}

@Component({
    selector: "bootstrap-form-panel",
    template: `
        <bootstrap-panel [severity]="severity" [show]="show" [showClose]="showClose" [title]="title"
                         [titleTemplate]="titleTemplate" (closed)="closed.emit($event)" [anchorId]="anchorId"
                         [showHeadingAnchors]="showHeadingAnchors">
            <form>
                <fieldset [disabled]="disabled">
                    <ng-content></ng-content>
                </fieldset>
            </form>
        </bootstrap-panel>
    `,
    styles: [` :host { display: block; } `],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class BootstrapFormPanelComponent {
    @Input() disabled: boolean = false;
    @Input() severity?: AlertSeverity;
    @Output() closed = new EventEmitter<void>();
    @Input() show?: boolean;
    @Input() showClose?: boolean;
    @Input() title?: string;
    @Input() titleTemplate?: TemplateRef<unknown>;
    @Input() anchorId?: string;
    @Input() showHeadingAnchors?: boolean = false;
}
