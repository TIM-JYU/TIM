import { Pipe, PipeTransform } from "@angular/core";
import moment from "moment";

@Pipe({
  name: "timeSince",
})
export class TimeSincePipe implements PipeTransform {

  transform(date: string): string {
    return moment(date).fromNow();
  }

}
