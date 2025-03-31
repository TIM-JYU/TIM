import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";

@Component({
    selector: "tim-group-name",
    template: `
    <ng-container>
        <h2>Rename your group</h2>
    </ng-container>
  `,
    styleUrls: ["./group-name.component.scss"],
})
@NgModule({
    declarations: [GroupNameModule],
    exports: [GroupNameModule],
    imports: [CommonModule],
})
export class GroupNameModule {}
