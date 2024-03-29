import type {PipeTransform} from "@angular/core";
import {Pipe} from "@angular/core";
import {capitalizeFirstLetter} from "tim/util/utils";

@Pipe({name: "capitalize"})
export class CapitalizePipe implements PipeTransform {
    transform(value: string): string {
        return capitalizeFirstLetter(value);
    }
}
