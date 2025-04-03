import {
    Component,
    Inject,
    Input,
    NgModule,
    OnInit,
    SimpleChanges,
} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormControl, ReactiveFormsModule, Validators} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {UserService} from "tim/user/userService";
import {UserGroupDialogComponent} from "tim/user/user-group-dialog.component";
import {BadgeService} from "./badge.service";
import {cons} from "fp-ts/ReadonlyNonEmptyArray";
import {manageglobals} from "tim/util/globals";
import {IFolder, IFullDocument} from "tim/item/IItem";

@Component({
    selector: "tim-group-name",
    template: `
        <ng-container>
            <div class="current">
                <p>Current group name: <b>{{ showFullName ? groupName : subGroup }}</b></p>
            </div>
            <div class="changeName">
                <button (click)="toggleFullName()">Toggle parent group</button>
                <button (click)="toggleInput()">Change group name</button>
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
    groupName: string | null = null;
    parentGroup: string | undefined;
    subGroup: string | undefined;
    group_id: number | undefined;
    item: IFullDocument | IFolder | undefined;
    newName = new FormControl("", [Validators.required]);
    displayedName: string | null | undefined;
    showInput: boolean = false;
    showFullName = false;
    storedGroup: {name: string; id: number} | null = null;

    constructor(private badgeService: BadgeService) {}

    /**
     * TODO: Kommentoitu koodin pätkä toimii, jos routes.py:ssä käytetään returnissa .human_name
     */
    async getGroupName() {
        if (this.group) {
            const fetchedGroup = await this.badgeService.getCurrentGroup(
                this.group
            );
            if (fetchedGroup) {
                this.groupName = fetchedGroup.name;
                this.group_id = fetchedGroup.id; // Assign ID as well
                this.storedGroup = {
                    name: fetchedGroup.name,
                    id: fetchedGroup.id,
                }; // Store fetched group
                console.log(fetchedGroup);
            }
        }
        this.parseParentGroup(this.groupName);
    }

    //TODO: placeholder input kenttään, halutaan vain aliryhmän nimi

    async saveName() {
        if (this.newName.valid) {
            this.groupName = this.newName.value;
            this.showInput = false;
        }

        if (this.storedGroup) {
            // Access stored group name and ID
            console.log(this.storedGroup.id);
            console.log(this.storedGroup.name);

            // Call your badgeService with the storedGroup information
            await this.badgeService.updateGroupName(
                this.storedGroup.id,
                this.storedGroup.name,
                this.newName.value
            );
        }
        return "Name successfully changed to: " + this.groupName;
    }

    parseParentGroup(groupName: string | null) {
        if (!this.groupName) return;
        const nameParts = this.groupName.split("-");
        this.parentGroup = nameParts[0];
        this.subGroup = nameParts.slice(1).join(".");
        this.displayedName = this.subGroup;
    }

    toggleInput() {
        this.showInput = !this.showInput;
    }

    toggleFullName() {
        this.showFullName = !this.showFullName;
        this.displayedName = this.showFullName ? this.groupName : this.subGroup;
    }

    ngOnInit(): void {
        console.log(
            "ngOnInit - group:",
            this.group,
            "username:",
            this.username
        );
        this.item = manageglobals().curr_item;
        this.getGroupName();
    }
}

@NgModule({
    declarations: [GroupNameComponent],
    exports: [GroupNameComponent],
    imports: [CommonModule, ReactiveFormsModule],
})
export class GroupNameModule {}
