import {Pipe, PipeTransform} from "@angular/core";
import {ReadonlyMoment} from "tim/util/readonlymoment";

@Pipe({name: "timtime"})
export class TimePipe implements PipeTransform {
    transform(value: ReadonlyMoment): string {
        return value.format("HH:mm:ss");
    }
}
