import moment from "moment";
import {Component, OnInit} from "@angular/core";
import {BookmarkService, IBookmarkGroup} from "tim/bookmark/bookmark.service";
import {TagService} from "tim/item/tag.service";
import {getVisibilityVars, IVisibilityVars} from "tim/timRoot";
import {rootInstance} from "tim/rootinstance";
import {HttpClient} from "@angular/common/http";
import {IDocSettings} from "../document/IDocSettings";
import {DocumentOrFolder, IFolder, isRootFolder, ITag, ITranslation, TagType} from "../item/IItem";
import {showMessageDialog} from "../ui/dialog";
import {Users} from "../user/userService";
import {genericglobals, someglobals} from "../util/globals";
import {capitalizeFirstLetter, to} from "../util/utils";

/**
 * Checks if the tag type is course code.
 * @param {string} tag
 * @returns {boolean} Whether the tag has course code tag.
 */
function isCourse(tag: ITag) {
    return tag.type === TagType.CourseCode;
}

const courseFolder = "My courses";

/**
 * Checks if the tag has expired.
 * @param {string} tag
 * @returns {boolean} False if the tag has no expiration or hasn't yet expired.
 */
function isExpired(tag: ITag) {
    return tag.expires && tag.expires.diff(moment.now()) < 0;
}

interface IItemLink {
    route: string;
    title: string;
}

@Component({
  selector: "tim-header",
  template: `
<div *ngIf="!hideLinks && item">
    <div class="pull-right">
        <button *ngIf="!hideVars.headerDocumentActions && showAddToMyCourses()"
                (click)="addToBookmarkFolder()"
                title="Add this page to 'My courses' bookmark folder"
                class="timButton label">
            Add to My courses
        </button>
        <span *ngFor="let tr of translations">
            <a class="label label-primary" href="/{{ route }}/{{ tr.path }}">{{ tr.lang_id }}</a>
        </span>
    </div>
    <ng-container *ngIf="!hideVars.headerNav">
        <div class="nav nav-tabs">
            <li *ngFor="let link of itemLinks"
                role="presentation"
                [ngClass]="{active: isActive(link)}">
                <a href="/{{ link.route }}/{{ item.path }}">{{ link.title }}</a>
            </li>
        </div>
        <ol class="breadcrumb">
            <li *ngFor="let c of crumbs">
                <a href="/{{ route }}/{{ c.path }}">{{ c.title }}</a>
            </li>
            <li class="active">{{ item.title }}</li>
        </ol>
    </ng-container>
</div>
  `,
})
export class HeaderComponent implements OnInit {
    public hideVars: IVisibilityVars = getVisibilityVars();
    // To show a button that adds the document to bookmark folder 'My courses'.
    private taggedAsCourse = false;
    public item?: DocumentOrFolder = genericglobals().curr_item;
    private bookmarked: boolean = false;
    private bookmarks: IBookmarkGroup[] = [];
    public route?: string;
    public itemLinks?: IItemLink[];
    public translations?: ITranslation[];
    public crumbs?: IFolder[];
    private docSettings?: IDocSettings;
    public hideLinks?: boolean;

    constructor(
        private bookmarkService: BookmarkService,
        private tagService: TagService,
        private http: HttpClient,
        ) {
    }

    ngOnInit() {
        const g = someglobals();
        this.hideLinks = "hideLinks" in g ? g.hideLinks : false;
        this.crumbs = "breadcrumbs" in g ? [...g.breadcrumbs].reverse() : undefined;
        this.translations = "translations" in g ? g.translations : [];
        this.docSettings = "docSettings" in g ? g.docSettings : undefined;
        this.route = document.location.pathname.split("/")[1];
        if (!this.item) {
            return;
        }
        const allowedRoutes = ["view"];
        if (!isRootFolder(this.item)) {
            allowedRoutes.push("manage");
        }
        if (!this.item.isFolder) {
            if (this.item.rights.teacher) {
                allowedRoutes.push("teacher");
            }
            if (this.item.rights.see_answers) {
                allowedRoutes.push("answers");
            }
            allowedRoutes.push("lecture", "velp", "slide");
        }
        this.itemLinks = allowedRoutes.map((r) => ({route: r, title: capitalizeFirstLetter(r)}));
        (async () => {
            await this.checkIfBookmarked();
            void this.checkIfTaggedAsCourse();
        })();
    }

    isActive(i: IItemLink) {
        return this.route === i.route;
    }

    getMainCourseDocPath() {
        if (!this.item) {
            return;
        }
        if (this.docSettings?.course_main) {
            return this.docSettings.course_main;
        }
        return this.item.path;
    }

    /**
     * Checks if the document has been tagged as a course and the tag hasn't expired.
     */
    private async checkIfTaggedAsCourse() {
        if (this.item?.isFolder) {
            return; // Folders don't have tags for now.
        }
        this.taggedAsCourse = false;
        const mainCourseDocPath = this.getMainCourseDocPath();
        if (!mainCourseDocPath) {
            return;
        }
        const r = await this.tagService.getTags(mainCourseDocPath);
        if (r.ok) {
            for (const tag of r.result) {
                if (isCourse(tag)) {
                    if (!isExpired(tag)) {
                        this.taggedAsCourse = true;
                    }
                    return;
                }
            }
        }
    }

    /**
     * Adds the current page to course bookmark folder.
     */
    async addToBookmarkFolder() {
        if (!Users.isLoggedIn()) {
            showMessageDialog("Log in to bookmark this course");
            return;
        }
        const viewctrl = rootInstance;
        if (!viewctrl) {
            throw new Error("viewctrl not registered");
        }
        const mainCourseDocPath = this.getMainCourseDocPath();
        if (!mainCourseDocPath) {
            return;
        }
        const r = await this.bookmarkService.addCourse(this.http, mainCourseDocPath);
        if (!r.ok) {
            await showMessageDialog(r.result.error.error);
            return;
        }
        if (r.result.added_to_group) {
            await to(showMessageDialog("You were successfully added to the course group."));
        }
        await viewctrl.bookmarksCtrl?.refresh();
        this.checkIfBookmarked();
    }

    /**
     * Marks page as bookmarked if it's in the course bookmark folder.
     */
    private checkIfBookmarked() {
        this.bookmarked = false;
        if (!Users.isLoggedIn() || genericglobals().bookmarks == null) {
            return;
        }
        const groups = this.bookmarkService.getGroups();
        if (!groups) {
            return;
        }
        this.bookmarks = groups;
        for (const folder of this.bookmarks) {
            if (folder.name === courseFolder) {
                for (const bookmark of folder.items) {
                    if (bookmark.link === `/view/${this.getMainCourseDocPath()}`) {
                        this.bookmarked = true;
                        return;
                    }
                }
            }
        }
    }

    showAddToMyCourses() {
        return this.taggedAsCourse && !this.bookmarked;
    }
}
