import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import type {IBookmark} from "tim/bookmark/bookmark.service";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";

@Component({
    selector: "tim-bookmark-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                Create bookmark
            </ng-container>
            <ng-container body>
                <form #f="ngForm" class="form-horizontal">
                    <div class="form-group"
                         [ngClass]="{'has-error': name.touched && name.errors?.required}">
                        <label for="name" class="col-sm-2 control-label">Name</label>
                        <div class="col-sm-10">
                            <input required focusMe [(ngModel)]="bookmark.name" #name="ngModel" name="name"
                                   type="text"
                                   class="form-control" id="name" placeholder="Bookmark name">
                        </div>
                    </div>
                    <div class="form-group">
                        <label for="group" class="col-sm-2 control-label">Folder</label>
                        <div class="col-sm-10">
                            <input [(ngModel)]="bookmark.group" name="group"
                                   type="text"
                                   class="form-control" id="group"
                                   placeholder="Folder name or blank to make a top-level bookmark">
                        </div>
                    </div>
                    <div class="form-group">
                        <label for="link" class="col-sm-2 control-label">Link</label>
                        <div class="col-sm-10">
                            <input [(ngModel)]="bookmark.link" type="text" class="form-control" name="linkField"
                                   id="link"
                                   placeholder="Leave blank to add current page">
                        </div>
                    </div>
                    <div *ngIf="showParamsCheckbox" class="form-group">
                        <div class="col-sm-offset-2 col-sm-10">
                            <div class="checkbox">
                                <label>
                                    <input [(ngModel)]="includeParams" [disabled]="bookmark.link.length > 0" name="includeParams"
                                           type="checkbox">
                                    Include URL parameters in link
                                </label>
                            </div>
                        </div>
                    </div>
                    <div *ngIf="showHashCheckbox" class="form-group">
                        <div class="col-sm-offset-2 col-sm-10">
                            <div class="checkbox">
                                <label>
                                    <input [(ngModel)]="includeHash" [disabled]="bookmark.link.length > 0" name="includeHash"
                                           type="checkbox">
                                    Include URL hash in link
                                </label>
                            </div>
                        </div>
                    </div>
                </form>
            </ng-container>
            <ng-container footer>
                <button [disabled]="!f.valid" class="timButton" type="button" (click)="ok()">Save
                </button>
                <button class="btn btn-default" type="button" (click)="cancel()">Cancel</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class BookmarkDialogComponent extends AngularDialogComponent<
    IBookmark,
    IBookmark
> {
    showParamsCheckbox?: boolean;
    showHashCheckbox?: boolean;
    bookmark!: IBookmark; // ngOnInit
    includeParams?: boolean;
    includeHash?: boolean;
    protected dialogName = "bookmark";

    ngOnInit() {
        this.bookmark = this.data;
        if (
            this.bookmark.group === "Last edited" ||
            this.bookmark.group === "Last read"
        ) {
            this.bookmark.group = "";
        }
        this.showParamsCheckbox = window.location.search.length > 1;
        this.showHashCheckbox = window.location.hash.length > 1;
    }

    public ok() {
        if (!this.bookmark.link) {
            this.bookmark.link = window.location.pathname;
            if (this.includeParams) {
                this.bookmark.link += window.location.search;
            }
            if (this.includeHash) {
                this.bookmark.link += window.location.hash;
            }
        }

        this.close(this.bookmark);
    }

    public cancel() {
        this.dismiss();
    }
}

@NgModule({
    declarations: [BookmarkDialogComponent],
    imports: [CommonModule, DialogModule, FormsModule, TimUtilityModule],
})
export class BookmarkDialogModule {}
