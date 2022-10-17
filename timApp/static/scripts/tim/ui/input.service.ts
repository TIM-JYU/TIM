import {Injectable} from "@angular/core";
import type {NgModel} from "@angular/forms";
import {TimDefer} from "tim/util/timdefer";

@Injectable()
export class InputService {
    defer = new TimDefer<NgModel>();
}
