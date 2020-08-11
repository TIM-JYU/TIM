/**
 * The directive handles velp summary.
 *
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
 */

import {Component, EventEmitter, Input, OnChanges, Output, SimpleChanges} from "@angular/core";
import {IUser} from "tim/user/IUser";
import {Annotation} from "./velptypes";

@Component({
    selector: "velp-summary",
    template: `
        <div class="velpArea">
            <div class="velpSummary">
                <h5>{{ selectedUser.name }} task annotations</h5>
                <table>
                    <tr>
                        <th class="summSecColumn">Annotation</th>
                        <th class="summThiColumn">Points</th>
                    </tr>
                    <tr *ngFor="let a of taskAnns">
                        <td class="summSecColumn"><a
                                (click)="annotationselected.emit(a)">{{ a.content }}</a>
                        </td>
                        <td class="summThiColumn"><span>{{ a.points }}</span></td>
                    </tr>
                    <tr>
                        <th class="summSecColumn">Points total in all answers</th>
                        <th class="summThiColumn"><span>{{ getTotalPoints(taskAnns) }}</span></th>
                    </tr>
                </table>
                <h5>Other document annotations</h5>
                <table>
                    <tr>
                        <th class="summSecColumn">Annotation</th>
                        <th class="summThiColumn">Points</th>
                    </tr>
                    <tr *ngFor="let b of docAnns">
                        <td class="summSecColumn"><a
                                (click)="annotationselected.emit(b)">{{ b.content }}</a>
                        </td>
                        <td class="summThiColumn"><span>{{ b.points }}</span></td>
                    </tr>
                </table>
            </div>
        </div>
    `,
    styleUrls: ["./velp-summary.component.scss"],
})
export class VelpSummaryComponent implements OnChanges {
    @Input() private annotations!: Array<Annotation>;
    @Input() selectedUser!: IUser;
    @Output() annotationselected = new EventEmitter<Annotation>();
    taskAnns!: Annotation[];
    docAnns!: Annotation[];

    /**
     * Gets total number of points.
     */
    getTotalPoints(annotations: Annotation[]) {
        let p = 0;
        for (let i = 0; i < annotations.length; i++) {
            p += annotations[i].points ?? 0;
        }
        // Cast back to a number, the string has trailing zeros.
        return Number(p.toPrecision(4));
    }

    private updateFilters(anns: Annotation[]) {
        const sortFn = (a: Annotation, b: Annotation) => a.creation_time.diff(b.creation_time);
        this.taskAnns = anns.filter(
            (a) => a.answer?.users.map((u) => u.id).includes(this.selectedUser.id)
        ).sort(sortFn);
        this.docAnns = anns.filter((a) => a.answer == null).sort(sortFn);
    }

    ngOnChanges(changes: SimpleChanges) {
        if (changes.annotations) { // TODO check if these are redundant
            this.updateFilters(changes.annotations.currentValue as Annotation[]);
        }
        if (changes.selectedUser) {
            this.updateFilters(this.annotations);
        }
    }
}
