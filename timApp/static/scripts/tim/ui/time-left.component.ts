import {Component, Input, OnInit} from "@angular/core";
import {showMessageDialog} from "tim/ui/dialog";

@Component({
  selector: "tim-time-left",
  template: `
    <span class="label label-default" [class.low-time]="isLowTime" i18n>
      Time left: <tim-countdown [displayUnits]="['d']" [endTime]="endTime" [lowTimeThreshold]="60" (onFinish)="onTimeUp()" (onLowTime)="onLowTime()"></tim-countdown>
    </span>
    <span class="low-time-warn alert alert-danger" *ngIf="isLowTime">The time is about to run out, please save your answers.</span>
  `,
  styleUrls: ["./time-left.component.scss"],
})
export class TimeLeftComponent implements OnInit {
  @Input() endTime?: string;
  isLowTime = false;

  constructor() { }

  ngOnInit(): void {
  }

  onLowTime() {
    this.isLowTime = true;
  }

  onTimeUp() {
    this.isLowTime = false;
    showMessageDialog("Time is up. You can still save answers for a while, but they will be marked invalid by default.");
  }

}
