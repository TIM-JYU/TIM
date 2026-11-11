import type {PipeTransform} from "@angular/core";
import {Pipe} from "@angular/core";
import type {ReadonlyMoment} from "tim/util/readonlymoment";

@Pipe({
    name: "timtime",
    standalone: false,
})
export class TimePipe implements PipeTransform {
    transform(value: ReadonlyMoment): string {
        return value.format("HH:mm:ss");
    }
}
