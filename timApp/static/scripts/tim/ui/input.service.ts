import {Injectable} from "@angular/core";
import {NgModel} from "@angular/forms";
import {TimDefer} from "../util/timdefer";

@Injectable()
export class InputService {
    defer = new TimDefer<NgModel>();
}
