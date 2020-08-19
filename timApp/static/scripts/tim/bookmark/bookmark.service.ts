import {Injectable} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {clone, to2} from "tim/util/utils";
import {genericglobals} from "tim/util/globals";

export interface IBookmarkGroup {
    name: string;
    isOpen: boolean;
    items: IBookmark[];
    editable: boolean;
}

export interface IBookmark {
    group: string;
    link: string;
    name: string;
}

@Injectable(
    {
        providedIn: "platform",
    }
)
export class BookmarkService {
    private groups: IBookmarkGroup[] | undefined;

    constructor() {
        this.groups = clone(genericglobals().bookmarks) ?? undefined;
    }

    fetchBookmarks(http: HttpClient) {
        return to2(http.get<IBookmarkGroup[]>("/bookmarks/get").toPromise());
    }

    async addCourse(http: HttpClient, path: string) {
        const resp = to2(http.post<{ bookmarks: IBookmarkGroup[], added_to_group: boolean }>(
            "/bookmarks/addCourse",
            {path: path}).toPromise(),
        );
        const r = await resp;
        if (r.ok) {
            this.groups = r.result.bookmarks;
        }
        return r;
    }

    getGroups() {
        return this.groups;
    }
}
