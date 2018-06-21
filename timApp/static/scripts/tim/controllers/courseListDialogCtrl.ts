/***
 * Dialog for tagging course meta data including course code and subject.
 */

import {IFormController, IRootElementService, IScope} from "angular";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {IItem, ITag, ITaggedItem, TagType} from "../IItem";
import {to} from "../utils";
import {$http} from "../ngimport";
import {IParResponse} from "../edittypes";
import {default as moment, Moment} from "moment";

/*
 * Tag search dialog's controller.
 */
export class ShowCourseListDialogController extends DialogController<{ params: IItem }, {}, "timCourseListDialog"> {
    private static $inject = ["$element", "$scope"];
    private doc: IItem;
    private docList: ITaggedItem[];

    // TODO: Get this from a TIM page.
    private subjects = ["biologia", "filosofia", "fysiikka", "kasvatus", "kemia", "matematiikka", "musiikki",
        "psykologia", "tietotekniikka", "tilastotiede"];

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    /*
     * Show tag list when dialog loads and focus on tag-field.
     */
    async $onInit() {
        super.$onInit();
        await this.getDocumentsByTag("", false, true);
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Available courses";
    }

    /***
     * Filter documents by tag.
     * @param tagName Tag word to search with.
     * @param exactMatch Search only documents with the whole tag.
     * @param list_doc_tags Get also tags in each document.
     * If false will also search for partial matches.
     */
    private async getDocumentsByTag(tagName: string, exactMatch: boolean, listDocTags: boolean) {
        const response = await $http<ITaggedItem[]>({
            method: "GET",
            url: "/tags/getDocs",
            params: {
                exact_search: exactMatch,
                list_doc_tags: listDocTags,
                name: tagName,
            },
        });
        this.docList = response.data;
    }

    /***
     * Returns course code if it exists for the item and and the code hasn't expired.
     * @param {ITaggedItem} d
     * @returns {string}
     */
    private getCourseCode(d: ITaggedItem) {
        for (const tag of d.tags) {
            if (tag.type === TagType.CourseCode) {
                if (!tag.expires || tag.expires.diff(moment.now()) > 0) {
                    return tag.name;
                }
            }
        }
        return "";
    }

    /***
     *
     */
    private groupBySubject() {
        let grouped = [];
        for (const s of this.subjects) {
            grouped.push({subject: s, doc: null});
        }
        for (const d of this.docList) {
            // TODO: Finish.
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
    </div>
        <ul ng-if="$ctrl.docList.length > 0">
            <li class="list-unstyled" ng-repeat="d in $ctrl.docList" ng-if="$ctrl.getCourseCode(d)">
                <a href="/view/{{d.path}}" title="Open {{d.title}}">
                <span class="btn-xs btn-primary">{{$ctrl.getCourseCode(d)}}</span>
                 {{d.title}}
                </a>
            </li>
        </ul>
    <span ng-if="$ctrl.docList.length == 0">No documents found!</span>
    </div>
    </dialog-body>
    <dialog-footer>
        <button class="btn timButton" ng-click="$ctrl.dismiss()"><span>Close</span></button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showCourseListDialog(d: IItem) {
    return await showDialog<ShowCourseListDialogController>("timCourseListDialog", {params: () => d}).result;
}
