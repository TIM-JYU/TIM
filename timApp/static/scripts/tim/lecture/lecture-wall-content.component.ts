import {
    Component,
    Input,
    NgModule,
    ChangeDetectionStrategy,
} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {ILectureMessage} from "tim/lecture/lecturetypes";
import {CommonModule} from "@angular/common";

@Component({
    selector: "tim-lecture-wall-content",
    template: `
        <ul class="list-unstyled">
          @for (m of messages; track m) {
            <li>
              @if (showName) {
                <span>{{m.user.name}}</span>
              }
              @if (showTime) {
                <span>&lt;{{ m.timestamp | timtime }}&gt;</span>
              }
              @if (showTime || showName) {
                <span>:</span>
              }
              {{m.message}}
            </li>
          }
        </ul>
        `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
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
