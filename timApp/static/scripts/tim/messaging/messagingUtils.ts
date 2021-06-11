import {HttpClient} from "@angular/common/http";
import {to2} from "../util/utils";

export async function markAsRead(http: HttpClient, message_id: number) {
    const result = await to2(
        http
            .post("/timMessage/mark_as_read", {
                message_id: message_id,
            })
            .toPromise()
    );
    if (!result.ok) {
        console.error(result.result.error.error); // TODO throw error
    }

    return result;
}
