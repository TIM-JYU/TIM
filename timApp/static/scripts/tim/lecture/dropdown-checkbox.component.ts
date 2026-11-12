import {
    Component,
    EventEmitter,
    Input,
    Output,
    ChangeDetectionStrategy,
} from "@angular/core";

@Component({
    selector: "tim-dropdown-checkbox",
    template: `
        <li role="menuitem"><a (click)="for = !for; forChange.emit(for); $event.stopPropagation()">
            <i class="glyphicon"
               [ngClass]="{'glyphicon-check': for, 'glyphicon-unchecked': !for}"></i>&ngsp;
            <ng-content></ng-content>
        </a>
        </li>
    `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class DropdownCheckboxComponent {
    @Input() for!: boolean;
    @Output() forChange = new EventEmitter<boolean>();
}
