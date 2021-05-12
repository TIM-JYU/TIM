import {Pipe, PipeTransform} from "@angular/core";
import {ReadonlyMoment} from "tim/util/readonlymoment";

@Pipe({name: "relative_time"})
export class RelativeTimestampPipe implements PipeTransform {
    transform(value: ReadonlyMoment): string {
        return value.fromNow();
    }
}
