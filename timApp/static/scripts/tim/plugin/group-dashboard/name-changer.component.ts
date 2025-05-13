import {
    Component,
    Input,
    NgModule,
    OnInit,
    EventEmitter,
    Output,
} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormControl, ReactiveFormsModule, Validators} from "@angular/forms";
import {manageglobals} from "tim/util/globals";
import {IFolder, IFullDocument} from "tim/item/IItem";
import {GroupService} from "tim/plugin/group-dashboard/group.service";
import {ICurrentUser} from "tim/user/IUser";

@Component({
    selector: "tim-name-changer",
    template: `
        <ng-container>
            <div class="name-changer">
                <p>Current group name: <b>{{ displayedName }}</b></p>
                <p>New name: <b>{{prettyName}}</b></p>
            
            <div class="buttons-section">
    <button *ngIf="canEditName" (click)="toggleInput()">Change group name</button>
</div>
                <div *ngIf="showInput" class="input-buttons">
                    <input [formControl]="newName" placeholder="Enter new group name"/>
                    <button (click)="saveName()" [disabled]="newName.invalid">Save</button>
                    <button (click)="toggleInput()" class="cancelButton">Cancel</button>
                </div>
                </div>
        </ng-container>
    `,
    styleUrls: ["./name-changer.component.scss"],
})
export class NameChangerComponent implements OnInit {
    @Input() group!: string;
    groupName: string | null = null;
    prettyName: string | null = null;
    subGroup: string | undefined;
    group_id: number | undefined;
    item: IFullDocument | IFolder | undefined;
    newName = new FormControl("", [Validators.required]);
    displayedName: string | null | undefined;
    canEditName: boolean | undefined;
    showInput: boolean = false;
    showFullName = true;
    user: ICurrentUser | null = null;
    doc: IFullDocument | IFolder | undefined;

    constructor(private groupService: GroupService) {}

    ngOnInit(): void {
        this.item = manageglobals().curr_item;
        this.getGroup();
    }

    /**
     * Fetch group from group service with the name that user has provided for component.
     * Access group's full name with .name, pretty_name with .description and id with .id
     */
    async getGroup() {
        if (!this.group) {
            return;
        }

        const fetchedGroup = await this.groupService.getCurrentGroup(
            this.group
        );
        if (fetchedGroup) {
            this.groupName = fetchedGroup.name;
            this.group_id = fetchedGroup.id;

            this.prettyName = fetchedGroup.description || "";
            this.displayedName = this.showFullName
                ? this.groupName
                : this.subGroup;

            this.canEditName = fetchedGroup.isMember || fetchedGroup.isTeacher;
        }
    }

    /**
     * Saves a new pretty name (description) for the current group,
     * updates local prettyName variable for display in user interface.
     */
    async saveName() {
        if (
            !this.newName.valid ||
            !this.groupName ||
            !this.group_id ||
            !this.newName.value
        ) {
            return;
        }

        const newPrettyName = this.newName.value;

        await this.groupService.updateGroupName(this.groupName, newPrettyName);

        this.prettyName = newPrettyName;
        this.newName.setValue("");
        this.showInput = !this.showInput;
    }

    toggleInput() {
        this.showInput = !this.showInput;
    }
}

@NgModule({
    declarations: [NameChangerComponent],
    exports: [NameChangerComponent],
    imports: [CommonModule, ReactiveFormsModule],
})
export class NameChangerModule {}
