import {
    Component,
    EventEmitter,
    Input,
    NgModule,
    OnChanges,
    OnInit,
    Output,
    SimpleChanges,
} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {NoopAnimationsModule} from "@angular/platform-browser/animations";
import moment from "moment";
import {DatetimePopupModule} from "vendor/ngx-bootstrap-datetime-popup/ngx-bootstrap-datetime-popup.module";

const datetimeFormat = "DD.MM.YYYY HH:mm:ss";

@Component({
    selector: "tim-datetime-picker",
    template: `
        <div class="input-group">
            <input class="form-control"
                   [placeholder]="placeholder"
                   (focus)="onFocus($event)"
                   (blur)="onBlur($event)"
                   (mouseup)="onFocus($event)"
                   [(ngModel)]="timeStr"
            />
            <datetime-popup [value]="time || currDate"
                            (valueChange)="popupChanged($event)"
                            [(showPopup)]="showPicker"></datetime-popup>
            <span class="input-group-addon" (click)="showPicker = true">
                <i class="glyphicon glyphicon-calendar" aria-hidden="true"></i>
            </span>
        </div>
    `,
    styleUrls: ["./datetime-picker.component.scss"],
})
export class DatetimePickerComponent implements OnInit, OnChanges {
    showPicker = false;
    timeStr!: string;
    @Input() time: Date | undefined;
    @Input() placeholder?: string = "";
    @Output() timeChange = new EventEmitter<Date | undefined>();
    currDate = new Date();

    ngOnInit() {
        this.updatetimeStr();
    }

    ngOnChanges(c: SimpleChanges) {
        if (c.time) {
            this.updatetimeStr();
        }
    }

    updatetimeStr() {
        this.timeStr = this.time
            ? moment(this.time).format(datetimeFormat)
            : "";
    }

    popupChanged(value: Date) {
        this.time = value;
        this.updatetimeStr();
        this.timeChange.emit(this.time);
    }

    onFocus(event: FocusEvent) {}

    onBlur(event: FocusEvent) {
        const s = this.timeStr;
        this.time = s ? moment(s, datetimeFormat).toDate() : undefined;
        this.timeChange.emit(this.time);
    }
}

@NgModule({
    declarations: [DatetimePickerComponent],
    imports: [
        BrowserModule,
        NoopAnimationsModule,
        DatetimePopupModule,
        FormsModule,
    ],
    exports: [DatetimePickerComponent],
})
export class DatetimePickerModule {}
