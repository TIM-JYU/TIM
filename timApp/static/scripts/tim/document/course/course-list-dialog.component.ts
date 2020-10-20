/**
 * Dialog for displaying active courses grouped by their subjects.
 */

import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {TimStorage, to2} from "tim/util/utils";
import {
    getCourseCode,
    ICourseSettings,
    ISubjectList,
    ITaggedItem,
    tagIsExpired,
    TagType,
} from "tim/item/IItem";
import {
    HTTP_INTERCEPTORS,
    HttpClient,
    HttpClientModule,
} from "@angular/common/http";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {TimeStampToMomentConverter} from "tim/util/time-stamp-to-moment-converter.service";
import * as t from "io-ts";

export interface ICourseListParams {
    settings: ICourseSettings;
}

export interface IGroupedCourses {
    subject: string; // TODO: More than one subject.
    closed: boolean;
    docs: ITaggedItem[];
    subsubjects: IGroupedCourses[];
}

@Component({
    selector: "tim-course-list-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <h5>Listing available courses
                    <a>
                        <i (click)="toggleAll()"
                           class="glyphicon"
                           [ngClass]="toggleCollapseAll ? 'glyphicon-minus-sign' : 'glyphicon-plus-sign'"></i>
                    </a>
                </h5>
                <div class="input-group">
                    <input class="form-control"
                           [(ngModel)]="filterText"
                           placeholder="Input filter word"
                           title="Filter by course code and title"
                           (keydown.enter)="enterPressed()">
                    <span class="input-group-addon btn" (click)="refresh()" title="Filter courses">
            <i class="glyphicon glyphicon-search"></i></span>
                </div>
                <div>
                    <ul class="list-unstyled" *ngIf="grouped.length > 0" id="courses">
                        <ng-container *ngFor="let subject of grouped">
                            <li *ngIf="subject.docs.length > 0"><span class="cursor-pointer"
                                                                      (click)="subject.closed = !subject.closed">
                    <a><span style="width: 1.3em;" class="glyphicon"
                             [ngClass]="subject.closed ? 'glyphicon-plus' : 'glyphicon-minus'"></span></a>
                    <span>{{subject.subject | capitalize}} <i>({{subject.docs.length}}
                        <ng-container
                                [ngPlural]="subject.docs.length">
                            <ng-template ngPluralCase="=1">course</ng-template>
                            <ng-template ngPluralCase="other">courses</ng-template>
                        </ng-container>)</i></span>
                </span>
                                <ul class="list-unstyled" *ngIf="!subject.closed">
                                    <ng-container *ngFor="let course of subject.docs">
                                        <li *ngIf="courseCode(course)">
                                            <a href="/view/{{course.path}}" title="Open {{course.title}}">
                                                <span class="btn-xs btn-primary">{{courseCode(course)}}</span>
                                                {{course.title}}
                                            </a></li>
                                    </ng-container>
                                </ul>
                            </li>
                        </ng-container>
                    </ul>
                    <span *ngIf="grouped.length == 0">No documents found!</span>
                </div>
            </ng-container>
            <ng-container footer>
                <button class="timButton" (click)="dismiss()">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class CourseListDialogComponent extends AngularDialogComponent<
    ICourseListParams,
    void
> {
    protected dialogName = "CourseList";
    private docList: ITaggedItem[] = [];
    private subjects: ISubjectList | undefined;
    grouped: IGroupedCourses[] = [];
    private closedSubjects: boolean[] = [];
    toggleCollapseAll: boolean;
    filterText: string = "";
    storage = new TimStorage("openSubjects", t.array(t.boolean));

    constructor(private http: HttpClient) {
        super();
        this.toggleCollapseAll = !this.allClosed(this.grouped);
    }

    /**
     * Show tag list when dialog loads and focus on tag field.
     */
    ngOnInit() {
        (async () => {
            await this.getDocumentsByTag("", false, true);
            this.subjects = this.data.settings.course_subjects;
            this.closedSubjects = this.storage.get() ?? [];
            this.groupBySubject();
            this.loadCollapseStates();
            this.toggleCollapseAll = !this.allClosed(this.grouped);
        })();
    }

    ngOnDestroy() {
        this.saveCollapseStates();
    }

    /**
     * Loads subjects collapse states.
     */
    private loadCollapseStates() {
        if (
            this.grouped &&
            this.closedSubjects &&
            this.closedSubjects.length === this.grouped.length
        ) {
            for (const {subject, i} of this.grouped.map((s, ind) => ({
                subject: s,
                i: ind,
            }))) {
                subject.closed = this.closedSubjects[i];
            }
        }
    }

    /**
     * Saves subject collapse states (whether the subject list is closed or not) into local storage.
     */
    private saveCollapseStates() {
        this.closedSubjects = [];
        if (this.grouped) {
            for (const subject of this.grouped) {
                this.closedSubjects.push(subject.closed);
            }
        }
        this.storage.set(this.closedSubjects);
    }

    public getTitle() {
        return "Available courses";
    }

    /**
     * Filter documents by tag.
     * @param tagName Tag word to search with.
     * @param exactMatch Search only documents with the whole tag.
     * @param listDocTags Get also tags in each document.
     * If false will also search for partial matches.
     */
    private async getDocumentsByTag(
        tagName: string,
        exactMatch: boolean,
        listDocTags: boolean
    ) {
        const response = await to2(
            this.http
                .get<ITaggedItem[]>("/tags/getDocs", {
                    params: {
                        exact_search: exactMatch.toString(),
                        list_doc_tags: listDocTags.toString(),
                        name: tagName,
                    },
                })
                .toPromise()
        );
        if (!response.ok) {
            return;
        }
        this.docList = response.result;
    }

    /**
     * Checks collapse states of all subjects.
     * @param {IGroupedCourses[]} courses List of grouped courses.
     * @returns {boolean} True if all course categories are closed, false if one or more are open.
     */
    private allClosed(courses: IGroupedCourses[]) {
        for (const course of courses) {
            if (!course.closed) {
                return false;
            }
        }
        return true;
    }

    /**
     * Groups courses by their subject tags. Leaves out expired courses, subjects
     * with no non-expired courses and filtered courses, if filter is used.
     */
    private groupBySubject() {
        let close = true;
        if (!this.subjects) {
            return;
        }
        for (const s of this.subjects) {
            const documents = [];
            if (typeof s === "string") {
                for (const d of this.docList) {
                    let isSameSubject = false;
                    let isNonExpiredCourse = false;
                    let passesFilter = false; // Won't be added unless this is true.
                    if (
                        this.filterText.length === 0 ||
                        d.title
                            .toLowerCase()
                            .includes(this.filterText.toLowerCase())
                    ) {
                        passesFilter = true;
                    }
                    for (const tag of d.tags) {
                        if (tag.type === TagType.Subject && tag.name === s) {
                            isSameSubject = true;
                        }
                        if (
                            tag.type === TagType.CourseCode &&
                            !tagIsExpired(tag)
                        ) {
                            if (
                                tag.name
                                    .toLowerCase()
                                    .includes(this.filterText.toLowerCase())
                            ) {
                                passesFilter = true;
                            }
                            isNonExpiredCourse = true;
                        }
                    }
                    if (isSameSubject && isNonExpiredCourse && passesFilter) {
                        documents.push(d);
                    }
                }
                // If a subject has no non-expired course documents in it, don't display it.
                if (documents.length > 0) {
                    this.grouped.push({
                        subject: s,
                        closed: close,
                        docs: documents.sort((a, b) =>
                            (getCourseCode(a.tags, true) ?? "").localeCompare(
                                getCourseCode(b.tags, true) ?? ""
                            )
                        ),
                        subsubjects: [],
                    });
                    close = true;
                }
            } else {
                // TODO: Subsubjects.
            }
        }
    }

    /**
     * Gets course code of an unexpired course.
     * @param {ITaggedItem} d Document with tags.
     * @returns {string} Coursecode or undefined, if non-existent or expired.
     */
    courseCode(d: ITaggedItem) {
        return getCourseCode(d.tags, true);
    }

    /**
     * Collapse or open all subjects.
     */
    toggleAll() {
        for (const item of this.grouped) {
            item.closed = this.toggleCollapseAll;
        }
        this.toggleCollapseAll = !this.toggleCollapseAll;
    }

    /**
     * Update course list according to filter word.
     */
    refresh() {
        this.grouped = [];
        this.groupBySubject();
        this.toggleCollapseAll = false;
        this.toggleAll();
    }

    enterPressed() {
        this.refresh();
    }
}

@NgModule({
    declarations: [CourseListDialogComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        DialogModule,
    ],
    providers: [
        {
            provide: HTTP_INTERCEPTORS,
            useClass: TimeStampToMomentConverter,
            multi: true,
        },
    ],
})
export class CourseListDialogModule {}
