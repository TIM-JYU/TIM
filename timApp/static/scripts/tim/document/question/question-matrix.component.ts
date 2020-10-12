import {
    Component,
    ElementRef,
    Input,
    QueryList,
    ViewChildren,
} from "@angular/core";
import type {QuestionEditDialogComponent} from "./question-edit-dialog.component";

// noinspection JSConstantReassignment
@Component({
    selector: "tim-question-matrix",
    template: `
        <table class="table">
            <thead>
            <tr *ngIf="qctrl.question.questionType == 'matrix' || qctrl.question.questionType == 'true-false' ">
                <th></th>
                <ng-container *ngIf="qctrl.isAskedQuestion()">
                    <th *ngFor="let item of qctrl.columnHeaders" class="text-center"
                        [innerHtml]="item.text"></th>
                </ng-container>
                <ng-container *ngIf="!qctrl.isAskedQuestion()">
                    <th *ngFor="let item of qctrl.columnHeaders; let index = index" class="text-center">
                        <div class="pos-rel" *ngIf="!qctrl.isAskedQuestion()">
                        <textarea class="form-control" rows="1" cols="10"
                                  [(ngModel)]="item.text"></textarea>
                            <div class="buttonsTop" *ngIf="qctrl.question.questionType != 'true-false'">
                                <button class="timButton btn-xs" tabindex="-1" (click)="qctrl.addCol(index)"><span
                                        class="glyphicon glyphicon-plus"></span></button>
                                <button class="timButton btn-xs btn-danger delCol" tabindex="-1"
                                        (click)="qctrl.delCol(index)">
                                    <span class="glyphicon glyphicon-minus"></span></button>
                            </div>
                        </div>
                    </th>
                </ng-container>
                <th *ngIf="(qctrl.question.questionType == 'matrix' ) && !qctrl.isAskedQuestion()">
                    <button class="wideButton addCol" (click)="qctrl.addCol(-1)"><span
                            class="glyphicon glyphicon-plus"></span></button>
                </th>
            </tr>
            </thead>
            <tbody>
            <tr *ngFor="let row of qctrl.rows; let index = index">
                <td *ngIf="qctrl.isAskedQuestion()" [innerHtml]="row.text"></td>
                <td *ngIf="!qctrl.isAskedQuestion()">
                    <div class="pos-rel">
                    <textarea #txt #ctrl="ngModel" [required]="qctrl.rows.length > 1" class="form-control"
                              [(ngModel)]="row.text" rows="1"
                              id="{{ 'r'+row.id }}" name="{{'r'+row.id}}"></textarea>
                        <div class="matrixButtons">
                            <button class="timButton btn-xs btn-block" tabindex="-1" (click)="qctrl.addRow(index)">
                                <span class="glyphicon glyphicon-plus"></span></button>
                            <button class="btn btn-danger btn-xs btn-block delRow" tabindex="-1"
                                    (click)="qctrl.delRow(index)"><span
                                    class="glyphicon glyphicon-minus"></span></button>
                        </div>
                        <tim-error-message [for]="ctrl"></tim-error-message>
                    </div>
                </td>
                <td *ngFor="let column of row.columns" class="text-center form-inline">
                    <!-- Horizontal radio-button-->
                    <div class="checkRadioCell"
                         *ngIf="(qctrl.question.matrixType == 'radiobutton-horizontal' && qctrl.question.questionType=='matrix') || qctrl.question.questionType == 'true-false' ">
                        <input type="radio"
                               [value]="column.id"
                               placeholder=""
                               disabled/>
                        <input type="text"
                               class="form-control"
                               size="2"
                               placeholder="pts"
                               [(ngModel)]="column.points"/>
                    </div>
                    <!-- Vertical radio-button -->
                    <div *ngIf="qctrl.question.questionType != 'true-false'">
                        <div class="checkRadioCell" *ngIf="qctrl.question.questionType == 'radio-vertical'">
                            <input type="radio"
                                   [value]="column.id"
                                   placeholder=""
                                   disabled/>
                            <input type="text"
                                   class="form-control"
                                   size="2"
                                   placeholder="pts"
                                   [(ngModel)]="column.points"/>
                        </div>
                        <textarea
                                class="form-control"
                                *ngIf="qctrl.question.matrixType == 'textArea' && qctrl.question.questionType=='matrix'"
                                type="text"
                                placeholder=""
                                disabled></textarea>
                        <div class="checkRadioCell"
                             *ngIf="(qctrl.question.matrixType == 'checkbox' && qctrl.question.questionType=='matrix') || qctrl.question.questionType=='checkbox-vertical'">
                            <input type="checkbox"
                                   disabled/>
                            <input type="text"
                                   class="form-control"
                                   size="2"
                                   placeholder="pts"
                                   [(ngModel)]="column.points"/>
                        </div>
                    </div>
                </td>
                <td [ngClass]="['explField', row.expl ? 'hasText' : '']">
                    <textarea placeholder="Optional: Explain why answer is right/wrong" class="form-control"
                              (focus)="qctrl.explFocus($event)" (blur)="qctrl.explBlur($event)"
                              [(ngModel)]="row.expl"
                              rows="1"></textarea>
                </td>
                <td *ngIf="!qctrl.isAskedQuestion() && qctrl.randomization">
                    <label><input type="checkbox" [(ngModel)]="row.locked"/> Lock position</label>
                </td>
            </tr>
            <tr *ngIf="!qctrl.isAskedQuestion()">
                <td>
                    <div>
                        <button class="wideButton addRow" (click)="qctrl.addRow(-1)"><span
                                class="glyphicon glyphicon-plus"></span></button>
                    </div>
                </td>
            </tr>
            </tbody>
        </table>
    `,
})
export class QuestionMatrixComponent {
    @Input() qctrl!: QuestionEditDialogComponent;
    @ViewChildren("txt") textareas!: QueryList<ElementRef<HTMLTextAreaElement>>;
}
