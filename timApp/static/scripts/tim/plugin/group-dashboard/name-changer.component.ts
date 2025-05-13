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
    @Input() username!: string;
    @Output() contextGroupChange = new EventEmitter<string>();
    @Output() groupNameChange = new EventEmitter<string>();
    groupName: string | null = null;
    prettyName: string | null = null;
    parentGroup: string | undefined;
    subGroup: string | undefined;
    group_id: number | undefined;
    item: IFullDocument | IFolder | undefined;
    newName = new FormControl("", [Validators.required]);
    displayedName: string | null | undefined;
    canEditName: boolean | undefined;
    showInput: boolean = false;
    showFullName = true;
    storedGroup: {name: string; id: number} | null = null;
    user: ICurrentUser | null = null;
    doc: IFullDocument | IFolder | undefined;

    constructor(private groupService: GroupService) {}

    ngOnInit(): void {
        this.item = manageglobals().curr_item;
        this.getGroupName();
    }

    async getGroupName() {
        if (!this.group) {
            return;
        }

        const fetchedGroup = await this.groupService.getCurrentGroup(
            this.group
        );
        if (fetchedGroup) {
            this.groupName = fetchedGroup.name; // store raw name for parsing
            this.group_id = fetchedGroup.id;
            this.storedGroup = {
                name: fetchedGroup.name,
                id: fetchedGroup.id,
            };

            this.prettyName = fetchedGroup.description || "";
            this.displayedName = this.showFullName
                ? this.groupName
                : this.subGroup;

            this.canEditName = fetchedGroup.isMember || fetchedGroup.isTeacher;
        }
    }

    async saveName() {
        if (!this.newName.valid || !this.storedGroup || !this.newName.value) {
            return;
        }

        const newPrettyName = this.newName.value;

        if (this.storedGroup) {
            await this.groupService.updateGroupName(
                this.storedGroup.id,
                this.storedGroup.name,
                newPrettyName
            );

            this.prettyName = newPrettyName;
            this.groupName = this.storedGroup.name;
            this.newName.setValue("");
            this.showInput = !this.showInput;
        }
        this.groupService.notifyGroupNameChange(
            this.storedGroup.id,
            newPrettyName
        );
        this.groupNameChange.emit(this.prettyName!);
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
