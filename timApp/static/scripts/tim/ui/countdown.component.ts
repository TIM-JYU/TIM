import {Component, EventEmitter, Input, OnInit, Output} from "@angular/core";
import humanizeDuration from "humanize-duration";
import {formatString, secondsToHHMMSS, to2} from "tim/util/utils";
import {Users} from "tim/user/userService";
import moment from "moment";
import {HttpClient} from "@angular/common/http";

const DAY_LIMIT = 24 * 60 * 60;

@Component({
    selector: "tim-countdown",
    template: `
        {{timeLeftText}}
    `,
})
export class CountdownComponent implements OnInit {
    @Input() endTime?: string;
    @Input() seconds?: number;
    @Input() displayUnits: humanizeDuration.Unit[] = [];
    @Input() noAutoStart: boolean = false;
    @Input() template: string = "{0}";
    @Input() lowTimeThreshold: number = -1;
    @Output() onFinish: EventEmitter<void> = new EventEmitter<void>();
    @Output() onLowTime: EventEmitter<void> = new EventEmitter<void>();

    isLowTime = false;
    currentCountdown = 0;
    currentEndDate?: moment.Moment;
    locale = Users.getCurrentLanguage();
    currentInterval?: number;
    timeLeftText?: string;

    constructor(private http: HttpClient) {
    }

    get timeLeft() {
        let prefix = "";
        // We need time as a whole number so we won't render fractional parts
        let time = Math.ceil(Math.max(this.currentCountdown, 0));
        if (time > DAY_LIMIT && this.displayUnits.length != 0) {
            prefix = humanizeDuration(time * 1000, {units: this.displayUnits, round: true, language: this.locale}) + " + ";
            time %= DAY_LIMIT;
        }
        return `${prefix}${secondsToHHMMSS(time)}`;
    }

    private async getEndDate() {
        if (this.seconds) {
            return moment().add(this.seconds, "s");
        }
        if (this.endTime) {
            const start = window.performance.now();
            const serverTime = await to2(this.http.get<{time: moment.Moment}>("/time").toPromise());
            if (!serverTime.ok) {
                return moment();
            }
            const remaining = moment(this.endTime).diff(serverTime.result.time, "s", true);
            const end = window.performance.now();

            // Attempt to alleviate potential error due to the RTT of the request by subtracting the run time of
            // the /time request + all processing
            // Note that all timing in JS is inherently imprecise on purpose (but should be enough for our needs):
            // https://developer.mozilla.org/en-US/docs/Web/API/Performance/now#Reduced_time_precision
            return moment().add(remaining - (end - start), "s");
        }
        return moment();
    }

    ngOnInit() {
        if (this.noAutoStart) { return; }
        this.start();
    }

    async start() {
        if (this.currentInterval) { return; }
        this.currentEndDate = await this.getEndDate();
        if (this.checkCountdown()) { return; }
        this.currentInterval = window.setInterval(() => this.checkCountdown(), 1000);
    }

    stop() {
        if (!this.currentInterval) { return; }
        window.clearInterval(this.currentInterval);
    }

    reset() {
        this.currentInterval = undefined;
        this.isLowTime = false;
    }

    private checkCountdown() {
        this.currentCountdown = this.currentEndDate?.diff(moment(), "s", true) ?? 0;
        this.timeLeftText = formatString(this.template, this.timeLeft);
        const timeEnded = this.currentCountdown <= 0;
        if (!this.isLowTime && this.currentCountdown < this.lowTimeThreshold) {
            this.onLowTime.emit();
            this.isLowTime = true;
        }
        if (timeEnded) {
            this.onFinish.emit();
            this.stop();
        }
        return timeEnded;
    }
}
