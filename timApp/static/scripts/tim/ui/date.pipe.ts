import {Pipe, PipeTransform} from "@angular/core";
import {ReadonlyMoment} from "tim/util/readonlymoment";

@Pipe({name: "timdate"})
export class DatePipe implements PipeTransform {
    transform(value: ReadonlyMoment): string {
        return value.format("DD.MM.YYYY HH:mm:ss");
    }
}
