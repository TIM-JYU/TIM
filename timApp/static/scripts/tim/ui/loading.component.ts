import {Component, Input} from "@angular/core";

@Component({
    selector: "tim-loading",
    template: `
<i class="glyphicon glyphicon-refresh" [style.color]="color"></i>
    `,
    styleUrls: ["./loading.component.scss"],
})
export class LoadingComponent {
    @Input() color?: string;
}
