import {HttpClient} from "@angular/common/http";
import {toPromise} from "../util/utils";

export function markAsRead(http: HttpClient, message_id: number) {
    return toPromise(
        http.post("/timMessage/mark_as_read", {
            message_id: message_id,
        })
    );
}
