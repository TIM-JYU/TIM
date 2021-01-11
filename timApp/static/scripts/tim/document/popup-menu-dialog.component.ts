import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule, NgZone} from "@angular/core";
import $ from "jquery";
import {watchEditMode} from "tim/document/editing/editmode";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {$rootScope} from "tim/util/ngimport";
import {documentglobals} from "../util/globals";
import {to2} from "../util/utils";
import {ViewCtrl} from "./viewctrl";
import {IMenuFunctionEntry, MenuFunctionList} from "./viewutils";

export type EditMode = "par" | "area";

export interface IPopupParams {
    actions: MenuFunctionList;
    contenturl?: string;
    editbutton: boolean;
    editcontext?: EditMode;
    save: boolean;
    srcid: string;
    vctrl: ViewCtrl;
}

/**
 * A popup menu component that is used in the document view.
 */
@Component({
    selector: "tim-popup-menu-dialog",
    template: `
        <tim-dialog-frame size="xs"
                          [minimizable]="false"
                          [mightBeAsync]="false"
                          anchor="absolute">
            <ng-container body>
                <div class="flex cl">
                    <div class="error" *ngIf="vctrl.notification" [innerText]="vctrl.notification"></div>
                    <div class="pastePreview" *ngIf="content" [innerHtml]="content"></div>

                    <div class="flex rw align-center" *ngFor="let f of actions; let first = first">
                        <button class="timButton btn-sm flex-grow-5"
                                focusMe
                                [enable]="first"
                                [innerText]="f.desc"
                                (click)="callFunc($event, f)">
                        </button>

                        <input *ngIf="save"
                               [checked]="getChecked(f.desc)"
                               (click)="clicked(f)"
                               style="margin: 5px 0 5px 15px;"
                               title="{{ getInputTitle(f) }}"
                               type="radio">
                    </div>

                    <div class="flex rw btn-group"
                         style="padding-top: 6px; margin-right: 28px"
                         *ngIf="editbutton && editable">
                        <button class="timButton parEditButton flex-grow-5"
                                (click)="parModeClicked()"
                                [class.btn-toggled]="editState === 'par'"
                                title="Toggle paragraph edit mode">
                            <i class="glyphicon glyphicon-minus"></i>
                            <i class="glyphicon glyphicon-pencil"></i>
                        </button>
                        <button class="timButton areaEditButton flex-grow-5"
                                [disabled]="true"
                                title="Toggle area edit mode">
                            <i class="glyphicon glyphicon-align-justify"></i>
                            <i class="glyphicon glyphicon-pencil"></i>
                        </button>
                    </div>
                </div>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class PopupMenuDialogComponent extends AngularDialogComponent<
    IPopupParams,
    void
> {
    protected dialogName = "popupMenu";
    public editState: EditMode | null = documentglobals().editMode;
    content?: string;
    private olds: Partial<IPopupParams> & {editState?: EditMode | null} = {
        contenturl: undefined,
        editState: undefined,
    };
    private p!: IPopupParams;

    constructor(private http: HttpClient, private zone: NgZone) {
        super();
    }

    updateAttrs(p: Partial<IPopupParams>) {
        this.p = {...this.p, ...p};
        this.zone.run(() => {});
    }

    get editable() {
        return this.vctrl.item.rights.editable;
    }

    get vctrl() {
        return this.p.vctrl;
    }

    get editbutton() {
        return this.p.editbutton;
    }

    get actions() {
        return this.p.actions.filter((a) => a.show);
    }

    get save() {
        return this.p.save;
    }

    ngOnInit() {
        this.p = this.data;
    }

    ngDoCheck() {
        if (this.p.contenturl != this.olds.contenturl && this.p.contenturl) {
            this.olds.contenturl = this.p.contenturl;
            this.getContent(this.p.contenturl);
        }
        if (this.editState != this.olds.editState) {
            this.olds.editState = this.editState;
            watchEditMode(this.editState, this.olds.editState);
        }
    }

    callFunc(e: MouseEvent, f: IMenuFunctionEntry) {
        /*
         runOutsideAngular is needed here.
         Otherwise, for example, the following sequence of actions will fail:

          * Click edit bar
          * Click "Add paragraph above"
          * Add a plugin (e.g. csPlugin)
          * Save
          * Run plugin

         The last step (run plugin) would fail to update plugin UI.
        */
        this.zone.runOutsideAngular(() => {
            f.func(e, $(this.p.srcid));
        });

        if (f.closeAfter || f.closeAfter == null) {
            this.close();
        }
    }

    getChecked(fDesc: string) {
        const stored = this.vctrl.defaultActionStorage.get();
        if (fDesc == null || stored == null) {
            return "";
        }

        return fDesc === stored ? "checked" : "";
    }

    getInputTitle(f: IMenuFunctionEntry) {
        return `Set "${f.desc}" as the default action for double-click`;
    }

    clicked(f: IMenuFunctionEntry) {
        if (!this.p.save) {
            return;
        }
        if (
            this.vctrl.defaultAction &&
            this.vctrl.defaultAction.desc === f.desc
        ) {
            this.vctrl.defaultAction = undefined;
            this.vctrl.defaultActionStorage.set(null);
        } else {
            this.vctrl.defaultAction = f;
            this.vctrl.defaultActionStorage.set(f.desc);
        }
    }

    async getContent(contentUrl: string) {
        if (!contentUrl) {
            this.content = undefined;
            return;
        }

        const r = await to2(
            this.http
                .get<{texts: string}>(contentUrl, {
                    params: {doc_id: this.vctrl.item.id.toString()},
                })
                .toPromise()
        );
        if (r.ok) {
            this.content = r.result.texts;
        }
    }

    parModeClicked() {
        if (this.editState === "par") {
            this.editState = null;
        } else {
            this.editState = "par";
        }
        $rootScope.$evalAsync();
    }
}

@NgModule({
    declarations: [PopupMenuDialogComponent],
    imports: [BrowserModule, DialogModule, TimUtilityModule, HttpClientModule],
})
export class PopupMenuDialogModule {}
