import $ from "jquery";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule, NgZone} from "@angular/core";
import {watchEditMode} from "tim/document/editing/editmode";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {$rootScope} from "tim/util/ngimport";
import {PurifyModule} from "tim/util/purify.module";
import {HelpPar} from "tim/document/structure/helpPar";
import {ParContext} from "tim/document/structure/parContext";
import {documentglobals} from "../util/globals";
import {copyToClipboard, toPromise} from "../util/utils";
import {ViewCtrl} from "./viewctrl";
import {IMenuFunctionEntry, MenuFunctionList} from "./viewutils";
import ClickEvent = JQuery.ClickEvent;
import TriggeredEvent = JQuery.TriggeredEvent;

export type EditMode = "par" | "area";

export interface IPopupParams {
    actions: MenuFunctionList;
    contenturl?: string;
    editbutton: boolean;
    editcontext?: EditMode;
    save: boolean;
    srcid: ParContext | HelpPar;
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
                    <div class="paragraphs pastePreview" *ngIf="content" [innerHtml]="content | purify"></div>

                    <div class="flex rw align-center"
                         *ngFor="let f of actions; let first = first"
                         [style.margin-top.px]="f.spacingBefore">
                        <button class="timButton btn-sm flex-grow-5"
                                focusMe
                                [enable]="first"
                                [innerText]="f.desc"
                                (click)="callFunc($event, f)">
                        </button>

                        <input *ngIf="(save && (f.allowAsDefault === undefined || f.allowAsDefault)); else emptyMargin"
                               [checked]="getChecked(f.desc)"
                               (click)="clicked(f)"
                               style="margin: 5px 0 5px 15px;"
                               title="{{ getInputTitle(f) }}"
                               type="radio">
                        <ng-template #emptyMargin>
                            <input type="radio" disabled style="margin: 5px 0 5px 15px; visibility: hidden">
                        </ng-template>
                    </div>

                    <div class="flex rw btn-group"
                         style="padding-top: 10px; margin-right: 28px"
                         *ngIf="editbutton && editable">
                        <button class="timButton parEditButton flex-grow-5"
                                (click)="parModeClicked()"
                                [class.btn-toggled]="editState === 'par'"
                                title="Toggle advanced edit mode">
                            <i class="glyphicon glyphicon-minus"></i>
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
    private labelClickHandlerSet = false;
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
            watchEditMode(this.editState);
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
            f.func(e);
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
        if (this.vctrl.defaultAction === f.desc) {
            this.vctrl.defaultAction = undefined;
            this.vctrl.defaultActionStorage.set(null);
        } else {
            this.vctrl.defaultAction = f.desc;
            this.vctrl.defaultActionStorage.set(f.desc);
        }
    }

    async getContent(contentUrl: string) {
        if (!contentUrl) {
            this.content = undefined;
            return;
        }

        const r = await toPromise(
            this.http.get<{texts: string}>(contentUrl, {
                params: {doc_id: this.vctrl.item.id.toString()},
            })
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
            if (!this.labelClickHandlerSet) {
                $("div.paragraphs")
                    .on("click", ".cnt-labels", (e) => {
                        this.copyReference(e);
                    })
                    .on("click", ".header-name", (e) => {
                        this.copyReference(e);
                    });
                this.labelClickHandlerSet = true;
            }
        }
        $rootScope.$evalAsync();
    }

    doCopyReference(text: string, remote: boolean): string {
        const texts = text.split("=");
        if (texts.length > 1) {
            text = texts[1];
        }
        let remotePrefix = "";
        if (remote) {
            let rrn = "";
            const ac = this.data.vctrl.docSettings.autocounters;
            if (ac) {
                rrn = ac.remoteRefName;
            }
            if (!rrn) {
                rrn = this.data.vctrl.reviewCtrl.item.name;
            }
            remotePrefix = rrn + ".";
        }
        return `%%"${remotePrefix}${text}"|ref%%`;
    }

    copyRemoteReference(
        e: TriggeredEvent<HTMLElement, undefined, unknown, HTMLElement>
    ) {
        this.doCopyReference(e.target.innerText, true);
    }

    copyReference(e: ClickEvent<HTMLElement, undefined, unknown, HTMLElement>) {
        const s = this.doCopyReference(e.target.innerText, e.ctrlKey);
        try {
            copyToClipboard(s);
        } catch (ex) {
            alert(ex);
        }
    }

    getCtx() {
        return this.p.srcid;
    }
}

@NgModule({
    declarations: [PopupMenuDialogComponent],
    imports: [
        BrowserModule,
        DialogModule,
        TimUtilityModule,
        HttpClientModule,
        PurifyModule,
    ],
})
export class PopupMenuDialogModule {}
