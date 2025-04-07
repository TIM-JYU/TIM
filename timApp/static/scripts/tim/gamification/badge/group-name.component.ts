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
    groupName: string | null = null;
    prettyName: string | null = null;
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
            console.log("Ryhmän tiedot: ", fetchedGroup);
            this.groupName = fetchedGroup.name; // store raw name for parsing
            this.group_id = fetchedGroup.id;
            this.storedGroup = {
                name: fetchedGroup.name,
                id: fetchedGroup.id,
            };

            this.prettyName = fetchedGroup.description || "";
            // Set the display name shown to users
            this.displayedName = this.groupName;

            this.parseParentGroup(this.groupName);
        }
    }

    //TODO: placeholder input kenttään, halutaan vain aliryhmän nimi

    async saveName() {
        if (!this.newName.valid || !this.storedGroup || !this.newName.value) {
            return;
        }

        const newPrettyName = this.newName.value;

        if (this.storedGroup) {
            await this.badgeService.updateGroupName(
                this.storedGroup.id,
                this.storedGroup.name,
                newPrettyName
            );

            this.prettyName = newPrettyName;
            this.groupName = this.storedGroup.name;
            this.newName.setValue("");
            this.showInput = !this.showInput;
        }
    }

    // Koko nimi on edelleen tallessa storedGroup.name:ssa tai prentGroupissa, jos käyttöä
    parseParentGroup(groupName: string | null) {
        if (!this.groupName) return;
        const nameParts = this.groupName.split("-");
        this.parentGroup = nameParts[0];
        this.subGroup = nameParts.slice(1).join(".");
        this.displayedName = this.subGroup;
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

    ngOnInit(): void {
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
