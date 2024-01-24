import type {OnInit} from "@angular/core";
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

@Component({
    selector: "tim-create-course-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                Courses yo
            </ng-container>
            <ng-container body>
            <form>
                <div>
                <label>
                    Title:
                    <input class="form-control" required [(ngModel)]="itemTitle" name="itemTitle"
                           type="text">
                </label>
                </div>
                <div>
                <label>
                    Location: <input size="50" class="form-control" type="text"
                                     timLocation [(ngModel)]="itemLocation" name="itemLocation">
                </label>
                </div>
                <label>
                    Part count
                </label>
                <button (click)="modifyCount(-1)">-</button>
                <input type="number" name="partCount" [(ngModel)]="coursePartCount">
                <button (click)="modifyCount(1)">+</button>
                <ng-container *ngFor="let p of partNames; let i = index; trackBy:trackByFn">
                <div class="input">
                    <label for="pn{{i}}">Part name:</label><input type="text" id="pn{{i}}" name="pn{{i}}" [(ngModel)]="partNames[i]">
                </div>
                </ng-container>
            </form>
            <div *ngIf="error" class="error" style="font-size: 12px" [innerHtml]="error | purify"></div>

            </ng-container>
            <ng-container footer>
                <button class="timButton" type="button" (click)="create()">OK</button>
                <button class="btn btn-default" type="button" (click)="dismiss()">Cancel</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class CreateCourseDialogComponent
    extends AngularDialogComponent<void, unknown>
    implements OnInit
{
    protected dialogName = "CreateCourse";
    coursePartCount: number = 3;
    partNames: string[] = [];
    itemTitle: string = "";
    itemLocation: string = "";
    creating = false;
    error = "";

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

    async create() {
        if (this.itemLocation == "" || this.itemTitle == "") {
            this.error = "Location and title are required";
            return;
        }
        console.log(this.partNames);
        console.log(this.itemLocation + "/" + slugify(this.itemTitle));
        this.creating = true;
        const r = await toPromise(
            this.http.post<{path: string}>("/createItem", {
                item_path: this.itemLocation + "/" + slugify(this.itemTitle),
                item_type: "course",
                item_title: this.itemTitle,
                part_names: this.partNames,
            })
        );
        if (!r.ok) {
            console.log(r.result.error.error);
            this.error = r.result.error.error;
            return;
        }
        window.location.href = "/view/" + r.result.path;
    }
}

@NgModule({
    declarations: [CreateCourseDialogComponent],
    imports: [
        CommonModule,
        DialogModule,
        TimUtilityModule,
        FormsModule,
        PurifyModule,
    ],
})
export class CreateCourseDialogModule {}
