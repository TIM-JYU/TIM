import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";

@Component({
    selector: "timGroupName",
    template: `
        <ng-container>
            <fieldset>
                <label>
                    <button>Change name</button>
                </label>
            </fieldset>
        </ng-container>
    `,
    styleUrls: ["./group-name.component.scss"],
})
export class GroupNameComponent implements OnInit {
    groupName: string | undefined;

    ngOnInit(): void {
        console.log("Group name tool");
    }
}

@NgModule({
    declarations: [GroupNameComponent],
    exports: [GroupNameComponent],
    imports: [CommonModule],
})
export class GroupNameModule {}
