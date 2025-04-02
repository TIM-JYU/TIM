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

    constructor(private badgeService: BadgeService) {}

    /**
     * TODO: Kommentoitu koodin pätkä toimii, jos routes.py:ssä käytetään returnissa .human_name
     */
    async getGroupName() {
        if (this.group) {
            const fetchedGroupName = await this.badgeService.getCurrentGroup(
                this.group
            );
            if (fetchedGroupName) {
                this.groupName = fetchedGroupName.name;
                this.group_id = fetchedGroupName.id; //Jos käytetään human_name(), ei saada ID:tä
                console.log(fetchedGroupName);
            }
            /*
            if (fetchedGroupName) {
                this.groupName = fetchedGroupName; // toimii, kun routessa .human_name()
                //this.group_id = fetchedGroupName.id; //Jos käytetään human_name(), ei saada ID:tä
                console.log(fetchedGroupName);
            }
             */
        }
        this.parseParentGroup(this.groupName);
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

    async saveName() {
        if (this.newName.valid) {
            this.groupName = this.newName.value;
            this.showInput = false;
        }

        if (this.item) {
            //console.log(this.item.id);
            //console.log(this.item.name);
            await this.badgeService.updateGroupName(
                this.item.id,
                this.item.name
            );
        }
        return "Name successfully changed to: " + this.groupName;
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
