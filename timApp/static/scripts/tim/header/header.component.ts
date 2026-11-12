import moment from "moment";
import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";
import type {IBookmarkGroup} from "tim/bookmark/bookmark.service";
import {BookmarkService} from "tim/bookmark/bookmark.service";
import {TagService} from "tim/item/tag.service";
import type {IVisibilityVars} from "tim/timRoot";
import {getVisibilityVars} from "tim/timRoot";
import {rootInstance} from "tim/rootinstance";
import {HttpClient} from "@angular/common/http";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import type {IItemLink} from "tim/header/utils";
import {getAvailableViews} from "tim/header/utils";
import type {IDocSettings} from "tim/document/IDocSettings";
import type {
    DocumentOrFolder,
    IFolder,
    ITag,
    ITranslation,
} from "tim/item/IItem";
import {TagType} from "tim/item/IItem";
import {Users} from "tim/user/userService";
import {genericglobals, someglobals} from "tim/util/globals";
import {getUrlParams, getViewName, to} from "tim/util/utils";

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

@Component({
    selector: "tim-header",
    template: `
        @if (!hideLinks && item) {
          <div>
            <div class="pull-right">
              @if (!hideVars.headerDocumentActions && showAddToMyCourses()) {
                <button
                  (click)="addToBookmarkFolder()"
                  title="Add this page to 'My courses' bookmark folder"
                  i18n-title
                  class="timButton label" i18n>
                    Add to 'My courses'
                </button>
              }
              @if (translations && translations.length > 1) {
                @for (tr of translations; track tr) {
                  <span>
                    <a class="label label-primary"
                    href="/{{ route }}/{{ tr.path + this.sanitizeSearch() }}">{{ getLanguageDisplayName(tr) }}</a>&ngsp;
                  </span>
                }
              }
            </div>
            @if (!hideVars.headerNav) {
              <div class="doc-header">
                <div class="nav nav-tabs">
                  @for (link of itemLinks; track link) {
                    <li
                      role="presentation"
                      [ngClass]="{active: isActive(link)}">
                      <a [href]="getViewURL(link)">{{ link.title }}</a>
                    </li>
                  }
                </div>
                <ol class="breadcrumb">
                  @for (c of crumbs; track c) {
                    <li>
                      <a href="/{{ route }}/{{ c.path }}">{{ c.title }}</a>
                    </li>
                  }
                  <li class="current">{{ item.title }}</li>
                </ol>
              </div>
            }
          </div>
        }
        `,
    styleUrls: ["./header.component.scss"],
    standalone: false,
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
        private http: HttpClient
    ) {}

    ngOnInit() {
        const g = someglobals();
        this.hideLinks = "hideLinks" in g ? g.hideLinks : false;
        this.crumbs =
            "breadcrumbs" in g ? [...g.breadcrumbs].reverse() : undefined;
        this.translations = "translations" in g ? g.translations : [];
        this.docSettings = "docSettings" in g ? g.docSettings : undefined;

        this.route = getViewName();
        if (!this.item) {
            return;
        }
        this.itemLinks = getAvailableViews();
        (async () => {
            await this.checkIfBookmarked();
            void this.checkIfTaggedAsCourse();
        })();
    }

    getLanguageDisplayName(tr: ITranslation) {
        if (this.docSettings?.showFullLanguageNames && tr.lang_name) {
            return `${tr.lang_name} (${tr.lang_id})`;
        }
        return tr.lang_id || "language not set";
    }

    isActive(i: IItemLink) {
        return this.route === i.route;
    }

    getViewURL(link: IItemLink) {
        if (!this.item) {
            return "";
        }
        return `/${link.route}/${this.item.path}${this.sanitizeSearch()}`;
    }

    sanitizeSearch(): string {
        if (getViewName() != "review") {
            return location.search;
        } else {
            const urlParams = getUrlParams();
            urlParams.delete("b");
            urlParams.delete("size");
            urlParams.delete("area");
            const str = urlParams.toString();
            return `${str.length > 0 ? "?" : ""}${str}`;
        }
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
            showMessageDialog($localize`Log in to bookmark this course`);
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
        const r = await this.bookmarkService.addCourse(
            this.http,
            mainCourseDocPath
        );
        if (!r.ok) {
            await showMessageDialog(r.result.error.error);
            return;
        }
        if (r.result.added_to_group) {
            await to(
                showMessageDialog(
                    "You were successfully added to the course group."
                )
            );
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
        const mainCourseDocPath = this.getMainCourseDocPath();
        if (!mainCourseDocPath) {
            return;
        }
        for (const folder of this.bookmarks) {
            if (folder.name === courseFolder) {
                for (const bookmark of folder.items) {
                    if (bookmark.link === `/view/${mainCourseDocPath}`) {
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
