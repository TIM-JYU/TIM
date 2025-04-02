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

@Component({
    selector: "tim-group-name",
    template: `
        <ng-container>
            <div class="current">
                <p>Current group name: <b>{{this.groupName}}</b></p>
            </div>
            <div class="changeName">
                <button (click)="toggleInput()">Change group name</button>
            </div>
            <div *ngIf="showInput">
                <input [formControl]="newName" placeholder="Enter new group name" />
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
    group_id: number | undefined;
    newName = new FormControl("", [Validators.required]);
    showInput: boolean = false;

    constructor(private badgeService: BadgeService) {}

    /**
     * TODO: Saven pitää toimia oikeasti; tee routes.py:n uusi reitti (tallenna block-taulukkoon description kohtaan
     */
    async getGroupName() {
        console.log("fetching...");
        if (this.group) {
            const fetchedGroupName = await this.badgeService.getCurrentGroup(
                this.group
            );
            if (fetchedGroupName) {
                this.groupName = fetchedGroupName.name;
                this.group_id = fetchedGroupName.id;
            }
        }
    }

    toggleInput() {
        this.showInput = !this.showInput;
    }

    async saveName() {
        console.log("Nykyinen nimi: ", this.groupName);
        if (this.newName.valid) {
            this.groupName = this.newName.value;
            this.showInput = false;
        }
        //console.log("Uusi nimi: ", this.groupName);
        if (this.group_id && this.groupName) {
            await this.badgeService.updateGroupName(
                this.group_id,
                this.groupName
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
        this.getGroupName();
    }
}

@NgModule({
    declarations: [GroupNameComponent],
    exports: [GroupNameComponent],
    imports: [CommonModule, ReactiveFormsModule],
})
export class GroupNameModule {}
