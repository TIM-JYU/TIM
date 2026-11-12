import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, NgModule, NgZone} from "@angular/core";
import {BrowserModule} from "@angular/platform-browser";
import {AccordionModule} from "ngx-bootstrap/accordion";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {NoopAnimationsModule} from "@angular/platform-browser/animations";
import type {ViewCtrl} from "tim/document/viewctrl";
import type {IItem} from "tim/item/IItem";
import {getItem} from "tim/item/IItem";
import {isScreenSizeOrLower} from "tim/util/utils";
import {registerModuleServerComponents} from "tim/serverComponents";
import {LectureController} from "tim/lecture/lectureController";

@Component({
    selector: "tim-lecture-menu",
    template: `
        @if (lctrl.lectureViewOrInLecture()) {
          <accordion>
            <accordion-group [(isOpen)]="isOpen">
              <div accordion-heading>
                {{ isOpen ? 'Hide' : 'Show' }} lecture info
                <i class="pull-right glyphicon"
                [ngClass]="{'glyphicon-chevron-down': isOpen, 'glyphicon-chevron-right': !isOpen}"></i>
              </div>
              @if (lctrl.lecture && lctrl.lectureSettings.inLecture) {
                <div class="row">
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
                      @if (lctrl.isLecturer) {
                        <button class="timButton btn-md"
                          (click)="lctrl.endLecture()" title="End lecture"
                          >
                          <span class="glyphicon glyphicon-stop" aria-hidden="true"></span>
                        </button>
                      }
                      <ng-container> &ngsp; </ng-container>
                      @if (lctrl.isLecturer) {
                        <button class="timButton btn-md"
                          (click)="lctrl.editLecture(lctrl.lecture.lecture_id)" title="Edit lecture"
                          >
                          <span class="glyphicon glyphicon-pencil" aria-hidden="true"></span>
                        </button>
                      }
                      @if (item && (!vctrl || item.id !== vctrl.item.id)) {
                        <a
                        href="/lecture/{{item.path}}">Go to lecture document</a>
                      }
                    </div>
                  </div>
                </div>
              }
              <div class="row">
                @if (!lctrl.lectureSettings.inLecture) {
                  <div class="col-lg-6">
                    <h4>Ongoing:</h4>
                    @if (lctrl.lectures.length > 0) {
                      <div>
                        <ul>
                          @for (l of lctrl.lectures; track l) {
                            <li>
                              {{ l.lecture_code }}
                              @if (lctrl.lectureSettings) {
                                <button class="timButton btn-md"
                                  (click)="lctrl.joinLecture(l)"
                                  title="Join lecture"
                                  >
                                  <span class="glyphicon glyphicon-log-in" aria-hidden="true"></span>
                                </button>
                              }
                              <ng-container> &ngsp; </ng-container>
                              @if (lctrl.isLecturer) {
                                <button class="timButton btn-md"
                                  (click)="lctrl.editLecture(l.lecture_id)"
                                  title="Edit lecture"
                                  >
                                  <span class="glyphicon glyphicon-pencil" aria-hidden="true"></span>
                                </button>
                              }
                            </li>
                          }
                        </ul>
                      </div>
                    }
                    @if (lctrl.lectures.length <= 0) {
                      <div>
                        <p>No running lectures</p>
                      </div>
                    }
                    @if (lctrl.isLecturer) {
                      <button class="timButton btn-md"
                        (click)="lctrl.toggleLecture()"
                        title="Create a new lecture"
                        >
                        <span class="glyphicon glyphicon-plus-sign" aria-hidden="true"></span>
                      </button>
                    }
                  </div>
                }
                @if (!lctrl.lectureSettings.inLecture) {
                  <div class="col-lg-6">
                    <h4>Upcoming:</h4>
                    <div>
                      @if (lctrl.futureLectures.length <= 0) {
                        <p>No upcoming lectures</p>
                      }
                      @if (lctrl.futureLectures.length > 0) {
                        <div>
                          <ul>
                            @for (l of lctrl.futureLectures; track l) {
                              <li>
                                {{ l.lecture_code }} {{ l.start_time | timdate }}
                                @if (lctrl.isLecturer) {
                                  <button class="timButton btn-md"
                                    (click)="lctrl.startFutureLecture(l)" title="Start lecture now"
                                    >
                                    <span class="glyphicon glyphicon-play" aria-hidden="true"></span>
                                  </button>
                                }
                                <ng-container> &ngsp; </ng-container>
                                @if (lctrl.isLecturer) {
                                  <button class="timButton btn-md"
                                    (click)="lctrl.editLecture(l.lecture_id)"
                                    title="Edit lecture"
                                    >
                                    <span class="glyphicon glyphicon-pencil" aria-hidden="true"></span>
                                  </button>
                                }
                              </li>
                            }
                          </ul>
                        </div>
                      }
                    </div>
                  </div>
                }
              </div>
            </accordion-group>
          </accordion>
        }
        `,
    standalone: false,
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
        AccordionModule,
        NoopAnimationsModule,
    ],
})
export class LectureMenuModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerModuleServerComponents(LectureMenuModule, {
    "tim-lecture-menu": LectureMenuComponent,
});
