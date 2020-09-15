import {Pipe, PipeTransform} from "@angular/core";
import {secondsToShortTime} from "tim/util/utils";
import {Users} from "tim/user/userService";

@Pipe({
    name: "timeSince",
})
export class TimeSincePipe implements PipeTransform {
    transform(diffSeconds: number): string {
        return secondsToShortTime(
            diffSeconds,
            ["d"],
            Users.getCurrentLanguage()
        );
    }
}
