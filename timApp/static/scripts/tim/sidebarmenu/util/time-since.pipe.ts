import { Pipe, PipeTransform } from "@angular/core";
import moment from "moment";

@Pipe({
  name: "timeSince",
  // Mark the pipe as pure to replicate AngularJS filter behaviour and to save from constant updates
  pure: true,
})
export class TimeSincePipe implements PipeTransform {

  transform(date: string): string {
    return moment(date).fromNow();
  }

}
