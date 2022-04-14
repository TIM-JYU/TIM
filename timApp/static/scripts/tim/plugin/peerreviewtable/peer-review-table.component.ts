import {
    ApplicationRef,
    Component,
    DoBootstrap,
    NgModule,
    OnInit,
    StaticProvider,
} from "@angular/core";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClientModule} from "@angular/common/http";
import {
    CdkDragDrop,
    DragDropModule,
    moveItemInArray,
    transferArrayItem,
} from "@angular/cdk/drag-drop";
import * as t from "io-ts";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {GenericPluginMarkup, getTopLevelFields} from "../attributes";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {toPromise} from "../../util/utils";

const PeerReviewTableMarkUp = t.intersection([
    t.partial({}),
    GenericPluginMarkup,
]);

const PeerReviewTableFields = t.intersection([
    getTopLevelFields(PeerReviewTableMarkUp),
    t.type({}), // määritellään dataa joka tulee palvelinpuolelta
]);

interface IResult {
    result: IPeerReviewData[];
}

interface IPeerReviewData {
    name: string;
    reviewers: string[];
}

@Component({
    selector: "tim-peerreview-table",
    templateUrl: "./peer-review-table.component.html",
    styleUrls: ["./peer-review-table.component.css"],
})
export class PeerReviewTableComponent
    extends AngularPluginBase<
        t.TypeOf<typeof PeerReviewTableMarkUp>,
        t.TypeOf<typeof PeerReviewTableFields>,
        typeof PeerReviewTableFields
    >
    implements OnInit
{
    data?: IPeerReviewData[] = [];
    message?: string;

    ngOnInit() {
        super.ngOnInit();
        this.getReviews();
    }

    async getReviews() {
        const r = await toPromise(this.http.get<IResult>("/myplugin/reviews"));
        if (r.ok) {
            this.data = r.result.result;
        } else {
            // Käsittele virhe
            console.log(r.result.error.error);
        }
    }

    drop(event: CdkDragDrop<string[]>) {
        if (event.previousContainer === event.container) {
            moveItemInArray(
                event.container.data,
                event.previousIndex,
                event.currentIndex
            );
        } else {
            transferArrayItem(
                event.previousContainer.data,
                event.container.data,
                event.previousIndex,
                event.currentIndex
            );
        }
    }

    getAttributeType() {
        return PeerReviewTableFields;
    }

    getDefaultMarkup() {
        return {};
        // return PeerReviewTableMarkUp;
    }

    TestFunction(): void {
        this.message = "Arvotaan uudet parit";
        setTimeout(() => {
            this.message = undefined;
        }, 1500);
    }
}
// Tämä osa importataan StaticDynamicImport tiedostossa->
@NgModule({
    declarations: [PeerReviewTableComponent],
    exports: [PeerReviewTableComponent],
    imports: [BrowserModule, HttpClientModule, DragDropModule],
})
export class PeerReviewTableModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

const angularJsModule = createDowngradedModule((providers: StaticProvider[]) =>
    platformBrowserDynamic(providers).bootstrapModule(PeerReviewTableModule)
);

doDowngrade(angularJsModule, "timPeerreviewTable", PeerReviewTableComponent);

export const moduleDefs = [angularJsModule];
