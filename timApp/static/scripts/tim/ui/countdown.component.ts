import {Component, EventEmitter, Input, OnInit, Output} from "@angular/core";
import humanizeDuration from "humanize-duration";
import {formatString, secondsToHHMMSS, setIntervalHighRes, TimeoutToken, to2} from "tim/util/utils";
import {Users} from "tim/user/userService";
import moment from "moment";
import {HttpClient} from "@angular/common/http";

const DAY_LIMIT = 24 * 60 * 60;

@Component({
    selector: "tim-countdown",
    template: `
        {{formatString(template, timeLeft)}}
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
    locale = Users.getCurrentLanguage();
    currentInterval?: TimeoutToken;
    formatString = formatString;

    constructor(private http: HttpClient) {
    }

    get timeLeft() {
        let prefix = "";
        let time = Math.max(this.currentCountdown, 0);
        if (this.currentCountdown > DAY_LIMIT && this.displayUnits.length != 0) {
            prefix = humanizeDuration(this.currentCountdown * 1000, {units: this.displayUnits, round: true, language: this.locale}) + " + ";
            time %= DAY_LIMIT;
        }
        return `${prefix}${secondsToHHMMSS(time)}`;
    }

    private async getCountdownStart() {
        // Ceil countdown seconds so we always include possible fractions of a second and account for possible
        // floating point precision errors
        if (this.seconds) {
            return Math.ceil(this.seconds);
        }
        if (this.endTime) {
            const serverTime = await to2(this.http.get<{time: moment.Moment}>("/time").toPromise());
            if (!serverTime.ok) {
                return 0;
            }
            return Math.ceil(moment(this.endTime).diff(serverTime.result.time, "seconds", true));
        }
        return 0;
    }

    ngOnInit() {
        if (this.noAutoStart) { return; }
        this.start();
    }

    async start() {
        if (this.currentInterval) { return; }
        this.currentCountdown = await this.getCountdownStart();
        if (this.checkCountdown(false)) { return; }
        this.currentInterval = new TimeoutToken();
        setIntervalHighRes(1000, this.checkCountdown, this.syncTime, this.currentInterval);
    }

    syncTime = async (dt: number) => {
        // If we have end date, we can sync with the server to find the real countdown
        // Otherwise if we only have countdown:
        // * dt < 0 (went back in time), do nothing, just keep counting (we can't to any better anyway)
        // * dt > 1000 (time jumped forward), decrease the countdown by missed seconds
        if (this.endTime) {
            this.currentCountdown = await this.getCountdownStart();
        } else if (dt > 1000) {
            const missedSeconds = Math.floor((dt - 1000) / 1000);
            this.currentCountdown -= missedSeconds;
        }
    };

    stop() {
        if (!this.currentInterval) { return; }
        this.currentInterval.stop();
        this.currentInterval = undefined;
    }

    reset() {
        this.currentInterval = undefined;
        this.isLowTime = false;
    }

    private checkCountdown = (count = true) => {
        if (count) {
            this.currentCountdown--;
        }
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
    };
}
