/**
 * Dialog for displaying active courses grouped by their subjects.
 */

import {IRootElementService, IScope} from "angular";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {ICourseSettings, IItem, ISubjectList, ITag, ITaggedItem, TagType} from "../IItem";
import {$http, $localStorage} from "../ngimport";
import {ngStorage} from "ngstorage";
import moment from "moment";

export interface ICourseListParams {
    item: IItem;
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
export class ShowCourseListDialogController extends DialogController<{ params: ICourseListParams }, {}, "timCourseListDialog"> {
    private static $inject = ["$element", "$scope"];
    private docList: ITaggedItem[];
    private subjects: ISubjectList | undefined;
    private grouped: IGroupedCourses[];
    private closedSubjects: boolean[];
    private storage: ngStorage.StorageService & {subjectsStorage: null | boolean[]};

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    /**
     * Show tag list when dialog loads and focus on tag-field.
     */
    async $onInit() {
        super.$onInit();
        await this.getDocumentsByTag("", false, true);
        this.subjects = this.resolve.params.settings.course_subjects;
        this.storage = $localStorage.$default({
            subjectsStorage: null,
        });
        if (this.storage.subjectsStorage) {
            this.closedSubjects = this.storage.subjectsStorage;
        }
        this.grouped = [];
        this.groupBySubject();
        this.loadCollapseStates();
    }

    $onDestroy() {
        this.saveCollapseStates();
    }

    /**
     * Loads subjects collapse states.
     */
    private loadCollapseStates() {
        if (this.grouped && this.closedSubjects && this.closedSubjects.length === this.grouped.length) {
            for (const {subject, i} of this.grouped.map((subject, i) => ({ subject, i }))) {
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
            for (const {subject, i} of this.grouped.map((subject, i) => ({ subject, i }))) {
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
        const response = await $http<ITaggedItem[]>({
            method: "GET",
            params: {
                exact_search: exactMatch,
                list_doc_tags: listDocTags,
                name: tagName,
            },
            url: "/tags/getDocs",
        });
        this.docList = response.data;
    }

    /**
     * Returns course code if it exists for the item and and the code hasn't expired.
     * @param {ITaggedItem} d Document and its tags.
     * @returns {string} Course code or empty strring, if none were found.
     */
    private getCourseCode(d: ITaggedItem) {
        for (const tag of d.tags) {
            if (tag.type === TagType.CourseCode) {
                if (!isExpired(tag)) {
                    return tag.name;
                }
            }
        }
        return "";
    }

    /**
     * Groups courses by their subject tags. Leaves out expired courses and subjects
     * with no non-expired courses.
     */
    private groupBySubject() {
        let close = false;
        if (!this.subjects) {
            return;
        }
        for (const s of this.subjects) {
            let documents = [];
            if (typeof s === "string") {
                for (const d of this.docList) {
                    let isSameSubject = false;
                    let isNonExpiredCourse = false;
                    for (const tag of d.tags) {
                        if (tag.type === TagType.Subject && tag.name === s) {
                            isSameSubject = true;
                        }
                        if (tag.type === TagType.CourseCode && !isExpired(tag)) {
                            isNonExpiredCourse = true;
                        }
                    }
                    if (isSameSubject && isNonExpiredCourse) {
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
}

registerDialogComponent("timCourseListDialog",
    ShowCourseListDialogController,
    {
        template:
            `<tim-dialog>
    <dialog-header>
    </dialog-header>
    <dialog-body>
    <h5>Listing available courses</h5>
    <div>
        <ul class="list-unstyled" ng-if="$ctrl.grouped.length > 0" id="courses" aria-multiselectable="true">
            <li ng-repeat="subject in $ctrl.grouped" ng-if="subject.docs.length > 0">
                <span class="cursor-pointer" ng-click="subject.closed = !subject.closed"
                data-toggle="collapse" data-parent="#courses" href="#{{subject.subject}}" aria-expanded="false"
                aria-controls="{{subject.subject}}">
                    <i class="glyphicon" ng-class="subject.closed ? 'glyphicon-plus-sign' : 'glyphicon-minus-sign'"></i>
                    {{subject.subject}}
                </span>
                <ul class="list-unstyled well well-sm" uib-collapse="subject.closed">
                    <li ng-repeat="course in subject.docs" ng-if="$ctrl.getCourseCode(course)">
                        <a href="/view/{{course.path}}" title="Open {{course.title}}">
                        <span class="btn-xs btn-primary">{{$ctrl.getCourseCode(course)}}</span>
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
    return await showDialog<ShowCourseListDialogController>("timCourseListDialog", {params: () => d}).result;
}

/**
 * Checks if the tag has expired.
 * @param {ITag} tag
 * @returns {boolean} False if the tag has no expiration or hasn't yet expired.
 */
function isExpired(tag: ITag) {
    if (tag.expires) {
        if (tag.expires.diff(moment.now()) < 0) {
            return true;
        }
    } else {
        return false;
    }
}
