import {Injectable} from "@angular/core";
import type {HttpClient} from "@angular/common/http";
import {clone, toPromise} from "tim/util/utils";
import {genericglobals} from "tim/util/globals";

export interface IBookmarkGroup {
    name: string;
    isOpen: boolean;
    items: IBookmark[];
    editable: boolean;
}

export type IBookmark = {
    group: string;
    link: string;
    name: string;
};

@Injectable({
    providedIn: "platform",
})
export class BookmarkService {
    private groups: IBookmarkGroup[] | undefined;

    constructor() {
        this.groups = clone(genericglobals().bookmarks) ?? undefined;
    }

    fetchBookmarks(http: HttpClient) {
        return toPromise(http.get<IBookmarkGroup[]>("/bookmarks/get"));
    }

    async addCourse(http: HttpClient, path: string) {
        const resp = toPromise(
            http.post<{bookmarks: IBookmarkGroup[]; added_to_group: boolean}>(
                "/bookmarks/addCourse",
                {path: path}
            )
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
