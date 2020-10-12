import {Pipe, PipeTransform} from "@angular/core";
import {Moment} from "moment";

@Pipe({name: "relative_time"})
export class RelativeTimestampPipe implements PipeTransform {
    transform(value: Moment): string {
        return value.fromNow();
    }
}

@Pipe({name: "timdate"})
export class TimDatePipe implements PipeTransform {
    transform(value: Moment): string {
        return value.format("DD.MM.YYYY HH:mm:ss");
    }
}
