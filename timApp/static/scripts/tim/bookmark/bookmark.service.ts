import {Injectable} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {IBookmarkGroup} from "tim/bookmark/bookmarks";
import {to2} from "tim/util/utils";

@Injectable(
    {
        providedIn: "root",
    }
)
export class BookmarkService {
    constructor(private http: HttpClient) {
    }

    getBookmarks() {
        return to2(this.http.get<IBookmarkGroup[]>("/bookmarks/get").toPromise());
    }

    addCourse(path: string) {
        return to2(this.http.post<{bookmarks: IBookmarkGroup[], added_to_group: boolean}>(
            "/bookmarks/addCourse",
            {path: path}).toPromise(),
        );
    }
}
