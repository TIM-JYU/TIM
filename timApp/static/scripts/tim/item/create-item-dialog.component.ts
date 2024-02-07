import type {OnInit} from "@angular/core";
import {Component, NgModule, ViewChild} from "@angular/core";
import {CommonModule} from "@angular/common";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {PurifyModule} from "tim/util/purify.module";
import {CreateItemComponent} from "tim/item/create-item.component";
import type {ICreateItemDialogParams} from "tim/item/showCreateItemDialog";

@Component({
    selector: "tim-create-item-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                Create a {{itemType}}
            </ng-container>
            <ng-container body>
                    <create-item #creator [itemType]="itemType" [params]="{source}" [itemLocation]="itemLocation" [showButton]="false"></create-item>
            </ng-container>
            <ng-container footer>
                <button class="timButton" type="button" [disabled]="creator.form.invalid" (click)="create()">Create {{itemType}}</button>
                <button class="btn btn-default" type="button" (click)="dismiss()">Cancel</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class CreateItemDialogComponent
    extends AngularDialogComponent<ICreateItemDialogParams | undefined, unknown>
    implements OnInit
{
    protected dialogName = "CreateItem";
    itemTitle: string = "";
    itemLocation: string = "";
    source: string = "";
    creating = false;
    error = "";
    @ViewChild(CreateItemComponent) creator!: CreateItemComponent;
    itemType = "document";

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {
        this.itemLocation = this.data?.location ?? this.itemLocation;
        this.source = this.data?.source ?? this.source;
        this.itemType = this.data?.itemType ?? this.itemType;
    }

    create() {
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
