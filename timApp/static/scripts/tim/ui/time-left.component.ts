import {Component, EventEmitter, Input, OnInit, Output} from "@angular/core";
import humanizeDuration from "humanize-duration";
import {formatString, secondsToHHMMSS, to2} from "tim/util/utils";
import {Users} from "tim/user/userService";
import Timeout = NodeJS.Timeout;
import moment from "moment";
import {HttpClient} from "@angular/common/http";

const DAY_LIMIT = 24 * 60 * 60;

@Component({
  selector: "tim-time-left",
  template: `
    <ng-container>
      <ng-container *ngIf="template else defaultPrefix">{{this.formatString(template, timeLeft)}}</ng-container>
      <ng-template #defaultPrefix i18n>Time left: {{timeLeft}}</ng-template>
    </ng-container>
  `,
})
export class TimeLeftComponent implements OnInit {
  @Input() endTime?: string;
  @Input() countdown?: number;
  @Input() displayUnits: humanizeDuration.Unit[] = ["d"];
  @Input() noAutoStart: boolean = false;
  @Input() template?: string;
  @Output() finishCallback: EventEmitter<void> = new EventEmitter();

  currentCountdown = 0;
  locale = Users.getCurrentLanguage();
  currentInterval?: Timeout;
  formatString = formatString;

  constructor(private http: HttpClient) {
  }

  get timeLeft() {
    let prefix = "";
    let time = this.currentCountdown;
    if (this.currentCountdown > DAY_LIMIT && this.displayUnits.length != 0) {
      prefix = humanizeDuration(this.currentCountdown * 1000, {units: this.displayUnits, round: true, language: this.locale}) + " + ";
      time %= DAY_LIMIT;
    }
    return `${prefix}${secondsToHHMMSS(time)}`;
  }

  private async getCountdownStart() {
    if (this.countdown) {
      return this.countdown;
    }
    if (this.endTime) {
      const serverTime = await to2(this.http.get<{time: moment.Moment}>("/time").toPromise());
      if (!serverTime.ok) {
        return 0;
      }
      return moment.utc(this.endTime).diff(serverTime.result.time.utc(), "seconds");
    }
    return 0;
  }

  ngOnInit() {
    if (this.noAutoStart) {
      return;
    }
    this.start();
  }

  async start() {
    if (this.currentInterval) { return; }
    this.currentCountdown = await this.getCountdownStart();
    if (this.checkCountdown()) { return; }
    this.currentInterval = setInterval(() => this.checkCountdown(), 1000);
  }

  stop() {
    if (!this.currentInterval) { return; }
    clearInterval(this.currentInterval);
  }

  reset() {
    this.currentInterval = undefined;
  }

  private checkCountdown() {
    this.currentCountdown--;
    const timeEnded = this.currentCountdown < 0;
    if (timeEnded) {
      this.finishCallback.emit();
    }
    return timeEnded;
  }
}
