import {Component, Input, OnInit} from "@angular/core";
import {showMessageDialog} from "tim/ui/dialog";

@Component({
  selector: "tim-time-left",
  template: `
    <span class="label label-default" i18n>
      Time left: <tim-countdown [displayUnits]="['d']" [endTime]="endTime" (finishCallback)="onTimeUp()"></tim-countdown>
    </span>
  `,
  styleUrls: ["./time-left.component.scss"],
})
export class TimeLeftComponent implements OnInit {
  @Input() endTime?: string;

  constructor() { }

  ngOnInit(): void {
  }

  onTimeUp() {
    showMessageDialog("Time is up. You can still save answers for a while, but they will be marked invalid by default.");
  }

}
