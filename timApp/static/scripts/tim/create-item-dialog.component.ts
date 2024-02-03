import type {OnInit} from "@angular/core";
import {ViewChild} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {Users} from "tim/user/userService";
import {slugify} from "tim/util/slugify";
import {PurifyModule} from "tim/util/purify.module";
import {CreateItemComponent} from "tim/item/create-item.component";
import {AppModule} from "tim/app.module";
import {DialogFrame} from "tim/ui/angulardialog/dialog-frame.component";
import App from "../../../modules/jsrunner/server/app";

@Component({
    selector: "tim-create-item-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                Courses yo
            </ng-container>
            <ng-container body>
                    <create-item #creator item-type="oscar" itemLocation="OSCAR" [showLocation]="false" [showCreateButton]="false"></create-item>
            </ng-container>
            <ng-container footer>
                <button class="timButton" type="button" (click)="create()">OK</button>
                <button class="btn btn-default" type="button" (click)="dismiss()">Cancel</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class CreateItemDialogComponent
    extends AngularDialogComponent<void, unknown>
    implements OnInit
{
    protected dialogName = "CreateItem";
    coursePartCount: number = 3;
    partNames: string[] = [];
    itemTitle: string = "";
    itemLocation: string = "";
    creating = false;
    error = "";
    @ViewChild(CreateItemComponent) creator!: CreateItemComponent;

    constructor(private http: HttpClient) {
        super();
    }

    getCurrentUserFolderPath() {
        return Users.getCurrentPersonalFolderPath();
    }

    ngOnInit() {
        this.partNames = Array(this.coursePartCount).fill("");
        this.itemLocation = Users.getCurrentPersonalFolderPath() ?? "";
    }

    modifyCount(val: number) {
        this.coursePartCount += val;
        if (this.coursePartCount < 0) {
            this.coursePartCount = 0;
        }
        if (this.coursePartCount < this.partNames.length) {
            this.partNames = this.partNames.splice(0, this.coursePartCount);
        } else if (this.coursePartCount > this.partNames.length) {
            this.partNames = [
                ...this.partNames,
                ...Array(this.coursePartCount - this.partNames.length).fill(""),
            ];
        }
    }

    trackByFn(index: number) {
        return index;
    }

    create() {
        console.log("child", this.creator);
        this.creator.createItem();
    }
}

@NgModule({
    declarations: [CreateItemDialogComponent],
    imports: [
        CommonModule,
        DialogModule,
        TimUtilityModule,
        FormsModule,
        PurifyModule,
    ],
})
export class CreateItemDialogModule {}
