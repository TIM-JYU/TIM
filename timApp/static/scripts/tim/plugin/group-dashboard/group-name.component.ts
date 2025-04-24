import {
    Component,
    Inject,
    Input,
    NgModule,
    OnInit,
    SimpleChanges,
    EventEmitter,
    Output,
} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormControl, ReactiveFormsModule, Validators} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {manageglobals} from "tim/util/globals";
import {IFolder, IFullDocument} from "tim/item/IItem";
import {GroupService} from "tim/plugin/group-dashboard/group.service";
import {ICurrentUser, IUser} from "tim/user/IUser";

@Component({
    selector: "tim-group-name",
    template: `
        <ng-container>
            <div class="current">
                <p>Current group name: <b>{{ displayedName }}</b></p>
                <p>Pretty name: <b>{{prettyName}}</b></p>
            </div>
            <div class="changeName">
                <button (click)="toggleInput()">Change group name</button>
                <button (click)="toggleFullName()">
    {{ showFullName ? "Show sub-group only" : "Show full group name" }}
</button>

            </div>
            <div *ngIf="showInput">
                <input [formControl]="newName" placeholder="Enter new group name"/>
                <button (click)="saveName()" [disabled]="newName.invalid">Save</button>
                <button (click)="toggleInput()">Cancel</button>
            </div>
        </ng-container>
    `,
    styleUrls: ["./group-name.component.scss"],
})
export class GroupNameComponent implements OnInit {
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
    showInput: boolean = false;
    showFullName = true;
    storedGroup: {name: string; id: number} | null = null;
    user: ICurrentUser | null = null;
    doc: IFullDocument | IFolder | undefined;

    constructor(
        private badgeService: BadgeService,
        private groupService: GroupService
    ) {}

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

            this.parseParentGroup(this.groupName);
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

    // Koko nimi on edelleen tallessa storedGroup.name:ssa tai prentGroupissa, jos käyttöä
    parseParentGroup(groupName: string | null) {
        if (!this.groupName) return;
        const nameParts = this.groupName.split("-");
        this.parentGroup = nameParts[0];
        this.subGroup = nameParts.slice(1).join(".");
        this.displayedName = this.groupName;
        this.contextGroupChange.emit(this.parentGroup);
    }

    toggleFullName() {
        this.showFullName = !this.showFullName;
        if (this.showFullName) {
            this.displayedName = this.groupName;
        } else {
            this.displayedName = this.subGroup;
        }
    }

    toggleInput() {
        this.showInput = !this.showInput;
    }
}

@NgModule({
    declarations: [GroupNameComponent],
    exports: [GroupNameComponent],
    imports: [CommonModule, ReactiveFormsModule],
})
export class GroupNameModule {}
