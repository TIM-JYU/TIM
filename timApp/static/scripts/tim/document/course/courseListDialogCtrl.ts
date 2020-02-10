/**
 * Dialog for displaying active courses grouped by their subjects.
 */

import {IScope} from "angular";
import {ngStorage} from "ngstorage";
import {to} from "tim/util/utils";
import {getCourseCode, ICourseSettings, ISubjectList, ITaggedItem, tagIsExpired, TagType} from "../../item/IItem";
import {DialogController, registerDialogComponent, showDialog} from "../../ui/dialog";
import {KEY_ENTER} from "../../util/keycodes";
import {$http, $localStorage} from "../../util/ngimport";

export interface ICourseListParams {
    settings: ICourseSettings;
}

export interface IGroupedCourses {
    subject: string; // TODO: More than one subject.
    closed: boolean;
    docs: ITaggedItem[];
    subsubjects: IGroupedCourses[];
}

/**
 * Tag search dialog's controller.
 */
export class CourseListDialogController extends DialogController<{ params: ICourseListParams }, {}> {
    static component = "timCourseListDialog";
    static $inject = ["$element", "$scope"] as const;
    private docList: ITaggedItem[] = [];
    private subjects: ISubjectList | undefined;
    private grouped: IGroupedCourses[];
    private closedSubjects: boolean[] = [];
    private storage: ngStorage.StorageService & {subjectsStorage: null | boolean[]};
    private toggleCollapseAll: boolean = false;
    private filterText: string = "";

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);
        this.storage = $localStorage.$default({
            subjectsStorage: null,
        });
        this.grouped = [];
        this.toggleCollapseAll = !this.allClosed(this.grouped);
    }

    /**
     * Show tag list when dialog loads and focus on tag-field.
     */
    $onInit() {
        super.$onInit();
        (async () => {
            await this.getDocumentsByTag("", false, true);
            this.subjects = this.resolve.params.settings.course_subjects;
            if (this.storage.subjectsStorage) {
                this.closedSubjects = this.storage.subjectsStorage;
            }
            this.groupBySubject();
            this.loadCollapseStates();
            this.toggleCollapseAll = !this.allClosed(this.grouped);
        })();
    }

    $onDestroy() {
        this.saveCollapseStates();
    }

    /**
     * Loads subjects collapse states.
     */
    private loadCollapseStates() {
        if (this.grouped && this.closedSubjects && this.closedSubjects.length === this.grouped.length) {
            for (const {subject, i} of this.grouped.map((s, ind) => ({ subject: s, i: ind }))) {
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
            for (const {subject, i} of this.grouped.map((s, ind) => ({ subject: s, i: ind }))) {
                this.closedSubjects.push(subject.closed);
            }
        }
        this.storage.subjectsStorage = this.closedSubjects;
    }

    /**
     * Dialog title.
     */
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
    private async getDocumentsByTag(tagName: string, exactMatch: boolean, listDocTags: boolean) {
        const response = await to($http<ITaggedItem[]>({
            method: "GET",
            params: {
                exact_search: exactMatch,
                list_doc_tags: listDocTags,
                name: tagName,
            },
            url: "/tags/getDocs",
        }));
        if (!response.ok) {
            return;
        }
        this.docList = response.result.data;
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
                    let passesFilter = false;  // Won't be added unless this is true.
                    if (this.filterText.length === 0 ||
                        d.title.toLowerCase().includes(this.filterText.toLowerCase())) {
                        passesFilter = true;
                    }
                    for (const tag of d.tags) {
                        if (tag.type === TagType.Subject && tag.name === s) {
                            isSameSubject = true;
                        }
                        if (tag.type === TagType.CourseCode && !tagIsExpired(tag)) {
                            if (tag.name.toLowerCase().includes(this.filterText.toLowerCase())) {
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
                    this.grouped.push({subject: s, closed: close, docs: documents, subsubjects: []});
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
    private courseCode(d: ITaggedItem) {
        return getCourseCode(d.tags, true);
    }

    /**
     * Collapse or open all subjects.
     */
    private toggleAll() {
        for (const item of this.grouped) {
            item.closed = this.toggleCollapseAll;
        }
        this.toggleCollapseAll = !this.toggleCollapseAll;
    }

    /**
     * Update course list according to filter word.
     */
    private refresh() {
        this.grouped = [];
        this.groupBySubject();
        this.toggleCollapseAll = false;
        this.toggleAll();
    }

    /**
     * Filter when Enter is pressed.
     * @param event Keyboard event.
     */
    private keyPressed(event: KeyboardEvent) {
        if (event.which === KEY_ENTER) {
            this.refresh();
        }
    }

    /**
     * "some string" -> "Some string"
     * @param {string} str String to capitalize.
     * @returns {string} Capitalized string.
     */
    private firstToUpper(str: string) {
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
}

registerDialogComponent(CourseListDialogController,
    {
        template:
            `<tim-dialog>
    <dialog-header>
    </dialog-header>
    <dialog-body>
    <div>
        <div class="col-md-8">
            <h5>Listing available courses <a><i ng-click="$ctrl.toggleAll()" class="glyphicon"
            ng-class="$ctrl.toggleCollapseAll ? 'glyphicon-minus-sign' : 'glyphicon-plus-sign'"></i></a></h5>
        </div>
        <div class="input-group col-md-4">
            <input class="form-control" ng-model="$ctrl.filterText" placeholder="Input filter word"
            title="Filter by course code and title" ng-keypress="$ctrl.keyPressed($event)">
            <span class="input-group-addon btn" ng-click="$ctrl.refresh()" title="Filter courses">
            <i class="glyphicon glyphicon-search"></i></span>
        </div>
    </div>
    <div>
        <h5></h5>
        <ul class="list-unstyled" ng-if="$ctrl.grouped.length > 0" id="courses">
            <li ng-repeat="subject in $ctrl.grouped" ng-if="subject.docs.length > 0">
                <span class="cursor-pointer" ng-click="subject.closed = !subject.closed">
                    <a><span style="width: 1.3em;" class="glyphicon"
                    ng-class="subject.closed ? 'glyphicon-plus' : 'glyphicon-minus'"></span></a>
                    <span>{{$ctrl.firstToUpper(subject.subject)}} <i>({{subject.docs.length}} <ng-pluralize
                    count="subject.docs.length"
                    when="{'1': 'course', 'other': 'courses'}"></ng-pluralize>)</i></span>
                </span>
                <ul class="list-unstyled" ng-hide="subject.closed">
                    <li ng-repeat="course in subject.docs | orderBy:$ctrl.courseCode"
                    ng-if="$ctrl.courseCode(course)" style="text-indent: 20px;">
                        <a href="/view/{{course.path}}" title="Open {{course.title}}">
                        <span class="btn-xs btn-primary">{{$ctrl.courseCode(course)}}</span>
                         {{course.title}}
                        </a>
                    </li>
                </ul>
            </li>
        </ul>
    <span ng-if="$ctrl.grouped.length == 0">No documents found!</span>
    </div>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showCourseListDialog(d: ICourseListParams) {
    return await showDialog(CourseListDialogController, {params: () => d}).result;
}
