import {Injectable} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {to2} from "tim/util/utils";
import {ITag} from "tim/item/IItem";

@Injectable(
    {
        providedIn: "root",
    }
)
export class TagService {
    constructor(private http: HttpClient) {
    }

    getTags(path: string) {
        return to2(this.http.get<ITag[]>(`/tags/getTags/${path}`).toPromise());
    }
}
