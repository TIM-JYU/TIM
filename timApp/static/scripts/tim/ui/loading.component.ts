import {Component, Input, ChangeDetectionStrategy} from "@angular/core";

@Component({
    selector: "tim-loading",
    template: `
<i class="glyphicon glyphicon-refresh" [style.color]="color"></i>
    `,
    styleUrls: ["./loading.component.scss"],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class LoadingComponent {
    @Input() color?: string;
}
