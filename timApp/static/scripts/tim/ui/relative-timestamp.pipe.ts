import type {PipeTransform} from "@angular/core";
import {Pipe} from "@angular/core";
import type {ReadonlyMoment} from "tim/util/readonlymoment";

@Pipe({name: "relative_time"})
export class RelativeTimestampPipe implements PipeTransform {
    transform(value: ReadonlyMoment): string {
        return value.fromNow();
    }
}
