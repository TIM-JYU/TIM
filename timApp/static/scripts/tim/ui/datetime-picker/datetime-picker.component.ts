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
import {DatetimePopupModule} from "ngx-bootstrap-datetime-popup";
import {DatepickerModule} from "ngx-bootstrap/datepicker";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {TimepickerModule} from "ngx-bootstrap/timepicker";

const datetimeFormat = "DD.MM.YYYY HH:mm:ss";

@Component({
    selector: "tim-datetime-picker",
    template: `
        <div class="input-group">
            <input class="form-control"
                   [placeholder]="placeholder"
                   (focus)="onFocus($event)"
                   (mouseup)="onFocus($event)"
                   [ngModel]="timeStr"
                   (ngModelChange)="timeStrModelChanged($event)"/>
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
    @Input() time!: Date | undefined;
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

    timeStrModelChanged(s: string) {
        this.time = s ? moment(s, datetimeFormat).toDate() : undefined;
        this.timeChange.emit(this.time);
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
}

@NgModule({
    declarations: [DatetimePickerComponent],
    imports: [
        BrowserModule,
        NoopAnimationsModule,
        BsDropdownModule.forRoot(),
        DatepickerModule.forRoot(),
        TimepickerModule.forRoot(),
        DatetimePopupModule,
        FormsModule,
    ],
    exports: [DatetimePickerComponent],
})
export class DatetimePickerModule {}
