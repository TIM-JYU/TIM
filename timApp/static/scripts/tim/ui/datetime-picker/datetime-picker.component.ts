import {
    Component,
    EventEmitter,
    Input,
    OnChanges,
    OnInit,
    Output,
    SimpleChanges,
} from "@angular/core";
import moment from "moment";

const datetimeFormat = "DD.MM.YYYY HH:mm:ss";

@Component({
    selector: "tim-datetime-picker",
    template: `
        <div class="input-group">
            <input class="form-control"
                   (focus)="onFocus($event)"
                   (mouseup)="onFocus($event)"
                   [ngModel]="timeStr"
                   (ngModelChange)="timeStrModelChanged($event)"/>
            <datetime-popup [value]="time"
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
    @Input() time!: Date;
    @Output() timeChange = new EventEmitter<Date>();

    ngOnInit() {
        this.updatetimeStr();
    }

    ngOnChanges(c: SimpleChanges) {
        if (c.time) {
            this.updatetimeStr();
        }
    }

    timeStrModelChanged(s: string) {
        this.time = moment(s, datetimeFormat).toDate();
        this.timeChange.emit(this.time);
    }

    updatetimeStr() {
        this.timeStr = moment(this.time).format(datetimeFormat);
    }

    popupChanged(value: Date) {
        this.time = value;
        this.updatetimeStr();
        this.timeChange.emit(this.time);
    }

    onFocus(event: FocusEvent) {}
}
