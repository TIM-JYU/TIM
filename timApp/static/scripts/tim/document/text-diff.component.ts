import type {OnChanges} from "@angular/core";
import {Component, Input, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import DiffMatchPatch from "diff-match-patch";

const DIFF_DELETE = -1;
const DIFF_INSERT = 1;

export type DiffFormat = "SideBySide" | "LineByLine";

interface IDiffPart {
    content: string;
    isDiff: boolean;
}

interface IDiffLine {
    lineNumber: number;
    content: string;
    parts: IDiffPart[];
    prefix: "" | "-" | "+";
}

interface IDiffRow {
    left?: IDiffLine;
    right?: IDiffLine;
    hasDiffs: boolean;
}

function splitLines(text: string): string[] {
    const lines = text.split("\n");
    if (lines[lines.length - 1] === "") {
        lines.pop();
    }
    return lines;
}

/**
 * Splits a line into parts that are the same as in the other line and parts that differ.
 */
function getDiffParts(value: string, compareValue: string): IDiffPart[] {
    const parts: IDiffPart[] = [];
    let shared = "";
    let diff = "";
    for (let i = 0; i < value.length; i++) {
        if (i < compareValue.length && value[i] === compareValue[i]) {
            if (diff !== "") {
                parts.push({content: diff, isDiff: true});
                diff = "";
            }
            shared += value[i];
        } else {
            if (shared !== "") {
                parts.push({content: shared, isDiff: false});
                shared = "";
            }
            diff += value[i];
        }
    }
    if (diff !== "") {
        parts.push({content: diff, isDiff: true});
    } else if (shared !== "") {
        parts.push({content: shared, isDiff: false});
    }
    return parts;
}

/**
 * Computes a line-based diff of two texts.
 * A deleted line and an inserted line with the same line number are paired into one row.
 */
export function computeDiffRows(left: string, right: string): IDiffRow[] {
    const dmp = new DiffMatchPatch();
    // eslint-disable-next-line no-underscore-dangle
    const a = dmp.diff_linesToChars_(left, right);
    const diffs = dmp.diff_main(a.chars1, a.chars2, false);
    // eslint-disable-next-line no-underscore-dangle
    dmp.diff_charsToLines_(diffs, a.lineArray);

    const rows: IDiffRow[] = [];
    let lineLeft = 1;
    let lineRight = 1;
    const pair = (row: IDiffRow) => {
        row.left!.parts = getDiffParts(row.left!.content, row.right!.content);
        row.right!.parts = getDiffParts(row.right!.content, row.left!.content);
    };
    for (const [type, value] of diffs) {
        for (const line of splitLines(value)) {
            if (type === DIFF_DELETE) {
                const leftLine: IDiffLine = {
                    lineNumber: lineLeft,
                    content: line,
                    parts: [{content: line, isDiff: true}],
                    prefix: "-",
                };
                const n = lineLeft;
                const other = rows.find(
                    (r) => !r.left && r.right?.lineNumber === n && r.hasDiffs
                );
                if (other) {
                    other.left = leftLine;
                    pair(other);
                } else {
                    rows.push({left: leftLine, hasDiffs: true});
                }
                lineLeft++;
            } else if (type === DIFF_INSERT) {
                const rightLine: IDiffLine = {
                    lineNumber: lineRight,
                    content: line,
                    parts: [{content: line, isDiff: true}],
                    prefix: "+",
                };
                const n = lineRight;
                const other = rows.find(
                    (r) => !r.right && r.left?.lineNumber === n && r.hasDiffs
                );
                if (other) {
                    other.right = rightLine;
                    pair(other);
                } else {
                    rows.push({right: rightLine, hasDiffs: true});
                }
                lineRight++;
            } else {
                rows.push({
                    left: {
                        lineNumber: lineLeft,
                        content: line,
                        parts: [],
                        prefix: "",
                    },
                    right: {
                        lineNumber: lineRight,
                        content: line,
                        parts: [],
                        prefix: "",
                    },
                    hasDiffs: false,
                });
                lineLeft++;
                lineRight++;
            }
        }
    }
    return rows;
}

/**
 * Shows the differences of two texts either line by line or side by side.
 */
@Component({
    selector: "tim-text-diff",
    template: `
        @if (showToolbar) {
          <div class="diff-toolbar">
            <label class="normalLabel">
              <input type="checkbox" [(ngModel)]="hideMatchingLines" (ngModelChange)="updateRows()">
              Only show lines with differences ({{ diffsCount }})
            </label>
            <div class="btn-group">
              <button class="btn btn-default btn-sm"
                [class.active]="format === 'SideBySide'"
                (click)="format = 'SideBySide'">Side by side
              </button>
              <button class="btn btn-default btn-sm"
                [class.active]="format === 'LineByLine'"
                (click)="format = 'LineByLine'">Line by line
              </button>
            </div>
          </div>
        }
        <div class="diff-table-container">
          @if (format === 'LineByLine') {
            <table class="diff-table">
              <tbody>
                @for (row of lineRows; track row) {
                  <tr>
                    <td class="content-col"
                      [class.delete-row]="row.left?.prefix === '-'"
                      [class.insert-row]="row.right?.prefix === '+'">
                      <ng-container
                        *ngTemplateOutlet="line; context: {$implicit: row.left ?? row.right, hasDiffs: row.hasDiffs}">
                      </ng-container>
                    </td>
                  </tr>
                }
              </tbody>
            </table>
          }
          @if (format === 'SideBySide') {
            <table class="diff-table side-by-side">
              <tbody>
                @for (row of shownRows; track row) {
                  <tr>
                    <td class="content-col"
                      [class.delete-row]="row.left?.prefix === '-'"
                      [class.empty-row]="!row.left">
                      <ng-container
                        *ngTemplateOutlet="line; context: {$implicit: row.left, hasDiffs: row.hasDiffs}">
                      </ng-container>
                    </td>
                    <td class="content-col"
                      [class.insert-row]="row.right?.prefix === '+'"
                      [class.empty-row]="!row.right">
                      <ng-container
                        *ngTemplateOutlet="line; context: {$implicit: row.right, hasDiffs: row.hasDiffs}">
                      </ng-container>
                    </td>
                  </tr>
                }
              </tbody>
            </table>
          }
        </div>
        <ng-template #line let-l let-hasDiffs="hasDiffs">
          @if (l) {
            @if (!hasDiffs) {
              <span>{{ l.content || ' ' }}</span>
            }
            @if (hasDiffs) {
              @for (p of l.parts; track p) {
                <span [class.highlight]="p.isDiff">{{ p.content }}</span>
              }
              @if (l.parts.length === 0) {
                <span>&#32;</span>
              }
            }
          }
        </ng-template>
        `,
    styleUrls: ["./text-diff.component.scss"],
    standalone: false,
})
export class TextDiffComponent implements OnChanges {
    @Input() left = "";
    @Input() right = "";
    @Input() format: DiffFormat = "LineByLine";
    @Input() showToolbar = true;
    hideMatchingLines = false;
    diffsCount = 0;
    private rows: IDiffRow[] = [];
    shownRows: IDiffRow[] = [];
    lineRows: IDiffRow[] = [];

    ngOnChanges() {
        this.rows = computeDiffRows(this.left, this.right);
        this.diffsCount = this.rows.filter((r) => r.hasDiffs).length;
        this.updateRows();
    }

    updateRows() {
        this.shownRows = this.hideMatchingLines
            ? this.rows.filter((r) => r.hasDiffs)
            : this.rows;
        this.lineRows = [];
        for (const r of this.shownRows) {
            if (r.hasDiffs && r.left && r.right) {
                this.lineRows.push(
                    {left: r.left, hasDiffs: true},
                    {right: r.right, hasDiffs: true}
                );
            } else {
                this.lineRows.push(r);
            }
        }
    }
}

@NgModule({
    declarations: [TextDiffComponent],
    exports: [TextDiffComponent],
    imports: [CommonModule, FormsModule],
})
export class TextDiffModule {}
