import {Component, Inject, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormControl, ReactiveFormsModule, Validators} from "@angular/forms";
import type {
    IBadge,
    IPersonalGroup,
    IGroup,
    IUser,
} from "tim/gamification/badge/badge.interface";

@Component({
    selector: "timGroupName",
    template: `
        <ng-container>
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
/**
 * TODO: Tämä komponentti pitää siirtää "scripts/tim/ui/" kansioon toimiakseen groupin asetuksissa
 * TODO: template pitää wrapata vielä <tim-plugin-frame> -sisään
 */
export class GroupNameComponent implements OnInit {
    groupName: string | null | undefined;
    newName = new FormControl("", [Validators.required]);
    showInput: boolean = false;

    ngOnInit(): void {}

    toggleInput() {
        this.showInput = !this.showInput;
    }

    saveName() {
        if (this.newName.valid) {
            this.groupName = this.newName.value;
            this.showInput = false;
        }
    }
}

@NgModule({
    declarations: [GroupNameComponent],
    exports: [GroupNameComponent],
    imports: [CommonModule, ReactiveFormsModule],
})
export class GroupNameModule {}
