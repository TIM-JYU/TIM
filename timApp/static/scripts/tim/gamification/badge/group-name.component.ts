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
                <p>Current group name: <b>{{ showFullName ? displayedName : subGroup }}</b></p>
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
     * TODO: Muuta hakemaan myös description, jota muokataan savessa
     * TODO: korjaa nimen päivittymistä komponennttin
     * TODO: Laita muutetut "pretty namet" päivittymään myös muihin komponentteihin, esim. giverin group valintaan
     * TODO: oma service ja metodien siirto sinne?
     */
    async getGroupName() {
        if (!this.group) {
            return;
        }

        const fetchedGroup = await this.badgeService.getCurrentGroup(
            this.group
        );
        if (fetchedGroup) {
            // Use description (pretty name) if it exists, fallback to group name
            this.groupName = fetchedGroup.name; // store raw name for parsing
            this.group_id = fetchedGroup.id;
            this.storedGroup = {
                name: fetchedGroup.name,
                id: fetchedGroup.id,
            };

            // Set the display name shown to users
            this.displayedName = fetchedGroup.description || fetchedGroup.name;

            // Split the raw group name to show subgroup if needed
            this.parseParentGroup(fetchedGroup.name);
        }
    }

    //TODO: placeholder input kenttään, halutaan vain aliryhmän nimi

    async saveName() {
        if (!this.newName.valid || !this.storedGroup || !this.newName.value) {
            return;
        }

        try {
            await this.badgeService.updateGroupName(
                this.storedGroup.id, // this is okay to keep even if unused
                this.storedGroup.name, // used in URL
                this.newName.value
            );

            // Update UI
            this.groupName = this.newName.value;
            this.parseParentGroup(this.groupName); // update displayed subGroup
            this.showInput = false;

            console.log("Name successfully changed to:", this.groupName);
        } catch (error) {
            console.error("Failed to update group name:", error);
            alert("Could not update group name. Please try again.");
        }
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
