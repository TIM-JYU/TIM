import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {DatetimePickerModule} from "tim/ui/datetime-picker/datetime-picker.component";

export type INameAreaOptions = {
    alttext?: string;
    collapse: boolean;
    collapsible: boolean;
    endtime?: Date;
    hlevel: number;
    starttime?: Date;
    timed?: boolean;
    title?: string;
};

@Component({
    selector: "tim-name-area-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <form #f="ngForm" class="form-horizontal">
                    <div class="form-group">
                        <label for="areaname" class="col-sm-3 control-label">Area name:</label>
                        <div class="col-sm-9">
                            <input id="areaname"
                                   (keydown.enter)="addArea()"
                                   class="form-control"
                                   type="text"
                                   [(ngModel)]="areaName"
                                   name="areaName"
                                   pattern="[^ ,.]+"
                                   required
                                   focusMe/>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-offset-2 col-sm-9">
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox"
                                           [(ngModel)]="options.collapsible"
                                           name="collapsible"> Collapsible
                                </label>
                            </div>
                        </div>
                    </div>
                    <div *ngIf="options.collapsible" class="form-group">
                        <label for="areatitle" class="col-sm-3 control-label">Area title:</label>
                        <div class="col-sm-9">
                            <input id="areatitle"
                                   type="text"
                                   class="form-control"
                                   [(ngModel)]="options.title"
                                   name="title"
                                   placeholder="Optional"/>
                        </div>
                    </div>
                    <div *ngIf="options.collapsible" class="form-group">
                        <label for="hlevel" class="col-sm-3 control-label">Heading level:</label>
                        <div class="col-sm-9">
                            <input id="hlevel"
                                   type="number"
                                   min="0"
                                   max="5"
                                   class="form-control"
                                   [(ngModel)]="options.hlevel"
                                   name="hlevel"
                                   placeholder="Optional"/>
                        </div>
                    </div>
                    <div *ngIf="options.collapsible" class="form-group">
                        <div class="col-sm-offset-2 col-sm-9">
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox"
                                           [(ngModel)]="options.collapse"
                                           name="collapse"> Collapsed by default
                                </label>
                            </div>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-offset-2 col-sm-9">
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox" [(ngModel)]="options.timed" name="timed"> Time restriction
                                </label>
                            </div>
                        </div>
                    </div>
                    <div *ngIf="options.timed" class="form-group">
                        <label for="starttime" class="col-sm-3 control-label">Available from:</label>
                        <div class="col-sm-9">
                            <tim-datetime-picker [(time)]="options.starttime"></tim-datetime-picker>
                        </div>
                    </div>
                    <div *ngIf="options.timed" class="form-group">
                        <label for="endtime" class="col-sm-3 control-label">Available until:</label>
                        <div class="col-sm-9">
                            <tim-datetime-picker [(time)]="options.endtime"></tim-datetime-picker>
                        </div>
                    </div>
                    <div *ngIf="options.timed" class="form-group">
                        <label for="alttext" class="col-sm-3 control-label">Text when inaccessible:</label>
                        <div class="col-sm-9">
                <textarea id="alttext"
                          rows="3"
                          class="form-control"
                          [(ngModel)]="options.alttext"
                          name="alttext">
                </textarea>
                        </div>
                    </div>
                </form>
            </ng-container>
            <ng-container footer>
                <button class="timButton" (click)="addArea()" [disabled]="!f.valid">Add
                </button>
                <button class="btn btn-default" (click)="dismiss()">Cancel</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class NameAreaDialogComponent extends AngularDialogComponent<
    void,
    {areaName: string; options: INameAreaOptions}
> {
    protected dialogName = "NameArea";
    areaName = "";
    options: INameAreaOptions = {
        collapse: true,
        collapsible: false,
        hlevel: 0,
    };

    getTitle() {
        return "Name area";
    }

    addArea() {
        if (!this.areaName) {
            return;
        }
        this.close({areaName: this.areaName, options: this.options});
    }
}

@NgModule({
    declarations: [NameAreaDialogComponent],
    imports: [BrowserModule, DialogModule, FormsModule, DatetimePickerModule],
})
export class NameAreaDialogModule {}
