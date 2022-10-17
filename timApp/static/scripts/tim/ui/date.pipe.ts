import type {PipeTransform} from "@angular/core";
import {Pipe} from "@angular/core";
import type {ReadonlyMoment} from "tim/util/readonlymoment";

@Pipe({name: "timdate"})
export class DatePipe implements PipeTransform {
    transform(value: ReadonlyMoment): string {
        return value.format("DD.MM.YYYY HH:mm:ss");
    }
}
