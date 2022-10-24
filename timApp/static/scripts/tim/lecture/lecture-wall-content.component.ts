import {Component, Input, NgModule} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {ILectureMessage} from "tim/lecture/lecturetypes";
import {CommonModule} from "@angular/common";

@Component({
    selector: "tim-lecture-wall-content",
    template: `
        <ul class="list-unstyled">
            <li *ngFor="let m of messages">
                <span *ngIf="showName">{{m.user.name}}</span>
                <span *ngIf="showTime">&lt;{{ m.timestamp | timtime }}&gt;</span>
                <span *ngIf="showTime || showName">:</span>
                {{m.message}}
            </li>
        </ul>
    `,
})
export class LectureWallContentComponent {
    @Input() messages!: ILectureMessage[];
    @Input() showName = true;
    @Input() showTime = true;
}

@NgModule({
    declarations: [LectureWallContentComponent],
    imports: [CommonModule, TimUtilityModule],
    exports: [LectureWallContentComponent],
})
export class LectureWallContentModule {}
