import {
    ApplicationRef,
    Component,
    DoBootstrap,
    NgModule,
    NgZone,
} from "@angular/core";
import {BrowserModule} from "@angular/platform-browser";
import {AccordionModule} from "ngx-bootstrap/accordion";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {NoopAnimationsModule} from "@angular/platform-browser/animations";
import {ViewCtrl} from "../document/viewctrl";
import {getItem, IItem} from "../item/IItem";
import {isScreenSizeOrLower} from "../util/utils";
import {createDowngradedModule, Digest, doDowngrade} from "../downgrade";
import {LectureController} from "./lectureController";

@Component({
    selector: "tim-lecture-menu",
    template: `
        <accordion *ngIf="lctrl.lectureViewOrInLecture()">
            <accordion-group [(isOpen)]="isOpen">
                <div accordion-heading>
                    {{ isOpen ? 'Hide' : 'Show' }} lecture info
                    <i class="pull-right glyphicon"
                       [ngClass]="{'glyphicon-chevron-down': isOpen, 'glyphicon-chevron-right': !isOpen}"></i>
                </div>

                <div class="row" *ngIf="lctrl.lecture && lctrl.lectureSettings.inLecture">
                    <div class="col-xs-12">
                        <h4>Currently in lecture: {{ lctrl.lecture.lecture_code }}</h4>

                        <p>Started: {{ lctrl.lecture.start_time | timdate }}</p>

                        <p>Ends: {{ lctrl.lecture.end_time | timdate }}</p>
                        <div>
                            <button class="timButton btn-md"
                                    (click)="lctrl.leaveLecture()" title="Leave lecture">
                                <span class="glyphicon glyphicon-log-out" aria-hidden="true"></span>
                            </button>
                            &ngsp;
                            <button class="timButton btn-md"
                                    (click)="lctrl.endLecture()" title="End lecture"
                                    *ngIf="lctrl.isLecturer">
                                <span class="glyphicon glyphicon-stop" aria-hidden="true"></span>
                            </button>
                            &ngsp;
                            <button class="timButton btn-md"
                                    (click)="lctrl.editLecture(lctrl.lecture.lecture_id)" title="Edit lecture"
                                    *ngIf="lctrl.isLecturer">
                                <span class="glyphicon glyphicon-pencil" aria-hidden="true"></span>
                            </button>
                            <a *ngIf="item && (!vctrl || item.id !== vctrl.item.id)"
                               href="/lecture/{{item.path}}">Go to lecture document</a>
                        </div>
                    </div>
                </div>

                <div class="row">
                    <div class="col-lg-6" *ngIf="!lctrl.lectureSettings.inLecture">
                        <h4>Ongoing:</h4>
                        <div *ngIf="lctrl.lectures.length > 0">
                            <ul>
                                <li *ngFor="let l of lctrl.lectures">
                                    {{ l.lecture_code }}
                                    <button class="timButton btn-md"
                                            (click)="lctrl.joinLecture(l)"
                                            title="Join lecture"
                                            *ngIf="lctrl.lectureSettings">
                                        <span class="glyphicon glyphicon-log-in" aria-hidden="true"></span>
                                    </button>
                                    &ngsp;
                                    <button class="timButton btn-md"
                                            (click)="lctrl.editLecture(l.lecture_id)"
                                            title="Edit lecture"
                                            *ngIf="lctrl.isLecturer">
                                        <span class="glyphicon glyphicon-pencil" aria-hidden="true"></span>
                                    </button>
                                </li>
                            </ul>
                        </div>
                        <div *ngIf="lctrl.lectures.length <= 0">
                            <p>No running lectures</p>
                        </div>
                        <button class="timButton btn-md"
                                (click)="lctrl.toggleLecture()"
                                title="Create a new lecture"
                                *ngIf="lctrl.isLecturer">
                            <span class="glyphicon glyphicon-plus-sign" aria-hidden="true"></span>
                        </button>
                    </div>
                    <div class="col-lg-6" *ngIf="!lctrl.lectureSettings.inLecture">
                        <h4>Upcoming:</h4>

                        <div>
                            <p *ngIf="lctrl.futureLectures.length <= 0">No upcoming lectures</p>

                            <div *ngIf="lctrl.futureLectures.length > 0">
                                <ul>
                                    <li *ngFor="let l of lctrl.futureLectures">
                                        {{ l.lecture_code }} {{ l.start_time | timdate }}
                                        <button class="timButton btn-md"
                                                (click)="lctrl.startFutureLecture(l)" title="Start lecture now"
                                                *ngIf="lctrl.isLecturer">
                                            <span class="glyphicon glyphicon-play" aria-hidden="true"></span>
                                        </button>
                                        &ngsp;
                                        <button class="timButton btn-md"
                                                (click)="lctrl.editLecture(l.lecture_id)"
                                                title="Edit lecture"
                                                *ngIf="lctrl.isLecturer">
                                            <span class="glyphicon glyphicon-pencil" aria-hidden="true"></span>
                                        </button>
                                    </li>
                                </ul>
                            </div>
                        </div>
                    </div>
                </div>
            </accordion-group>
        </accordion>
    `,
})
export class LectureMenuComponent {
    vctrl?: ViewCtrl;
    item: IItem | undefined;
    isOpen = false;
    lctrl!: LectureController;

    constructor(private zone: NgZone) {}

    async ngOnInit() {
        this.vctrl = vctrlInstance;
        if (this.vctrl) {
            this.vctrl.lectureCtrl.registerLectureMenu(this);
        }
        this.lctrl =
            this.vctrl?.lectureCtrl ??
            LectureController.createAndInit(this.vctrl);
        if (this.lctrl.lecture) {
            this.item = await getItem(this.lctrl.lecture.doc_id);
        }
        if (
            !isScreenSizeOrLower("md") ||
            this.lctrl.lectureSettings.inLecture
        ) {
            this.isOpen = true;
        }
    }

    checkChanges() {
        this.zone.run(() => {});
    }
}

@NgModule({
    declarations: [LectureMenuComponent],
    imports: [
        BrowserModule,
        TimUtilityModule,
        AccordionModule.forRoot(),
        NoopAnimationsModule,
    ],
})
export class LectureMenuModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                LectureMenuModule
            )
        ),
        "timLectureMenu",
        LectureMenuComponent,
        Digest.Propagate
    ),
];
