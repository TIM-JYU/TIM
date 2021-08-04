import {HttpClient} from "@angular/common/http";
import {to2} from "../util/utils";

export function markAsRead(http: HttpClient, message_id: number) {
    return to2(
        http
            .post("/timMessage/mark_as_read", {
                message_id: message_id,
            })
            .toPromise()
    );
}
