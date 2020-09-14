import {Pipe, PipeTransform} from "@angular/core";
import moment from "moment";
import {secondsToShortTime} from "tim/util/utils";
import {Users} from "tim/user/userService";

@Pipe({
    name: "timeSince",
})
export class TimeSincePipe implements PipeTransform {
    transform(date: string): string {
        return secondsToShortTime(
            moment().utc().diff(date, "seconds"),
            ["d"],
            Users.getCurrentLanguage()
        );
    }
}
