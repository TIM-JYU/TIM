import type {ElementRef} from "@angular/core";
import {Component, Input, QueryList, ViewChildren} from "@angular/core";
import {QuestionEditDialogComponent} from "tim/document/question/question-edit-dialog.component";

// noinspection JSConstantReassignment
@Component({
    selector: "tim-question-matrix",
    template: `
        <table class="table">
          <thead>
            @if (qctrl.question.questionType == 'matrix' || qctrl.question.questionType == 'true-false' ) {
              <tr>
                <th></th>
                @if (qctrl.isAskedQuestion()) {
                  @for (item of qctrl.columnHeaders; track item) {
                    <th class="text-center"
                    [innerHtml]="item.text"></th>
                  }
                }
                @if (!qctrl.isAskedQuestion()) {
                  @for (item of qctrl.columnHeaders; track item; let index = $index) {
                    <th class="text-center">
                      @if (!qctrl.isAskedQuestion()) {
                        <div class="pos-rel">
                          <textarea class="form-control" rows="1" cols="10"
                          [(ngModel)]="item.text"></textarea>
                          @if (qctrl.question.questionType != 'true-false') {
                            <div class="buttonsTop">
                              <button class="timButton btn-xs" tabindex="-1" (click)="qctrl.addCol(index)"><span
                              class="glyphicon glyphicon-plus"></span></button>
                              <button class="timButton btn-xs btn-danger delCol" tabindex="-1"
                                (click)="qctrl.delCol(index)">
                                <span class="glyphicon glyphicon-minus"></span></button>
                              </div>
                            }
                          </div>
                        }
                      </th>
                    }
                  }
                  @if ((qctrl.question.questionType == 'matrix' ) && !qctrl.isAskedQuestion()) {
                    <th>
                      <button class="wideButton addCol" (click)="qctrl.addCol(-1)"><span
                      class="glyphicon glyphicon-plus"></span></button>
                    </th>
                  }
                </tr>
              }
            </thead>
            <tbody>
              @for (row of qctrl.rows; track row; let index = $index) {
                <tr>
                  @if (qctrl.isAskedQuestion()) {
                    <td [innerHtml]="row.text"></td>
                  }
                  @if (!qctrl.isAskedQuestion()) {
                    <td>
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
                    }
                    @for (column of row.columns; track column) {
                      <td class="text-center form-inline">
                        <!-- Horizontal radio-button-->
                        @if ((qctrl.question.matrixType == 'radiobutton-horizontal' && qctrl.question.questionType=='matrix') || qctrl.question.questionType == 'true-false' ) {
                          <div class="checkRadioCell"
                            >
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
                        }
                        <!-- Vertical radio-button -->
                        @if (qctrl.question.questionType != 'true-false') {
                          <div>
                            @if (qctrl.question.questionType == 'radio-vertical') {
                              <div class="checkRadioCell">
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
                            }
                            @if (qctrl.question.matrixType == 'textArea' && qctrl.question.questionType=='matrix') {
                              <textarea
                                class="form-control"
                                type="text"
                                placeholder=""
                              disabled></textarea>
                            }
                            @if ((qctrl.question.matrixType == 'checkbox' && qctrl.question.questionType=='matrix') || qctrl.question.questionType=='checkbox-vertical') {
                              <div class="checkRadioCell"
                                >
                                <input type="checkbox"
                                  disabled/>
                                <input type="text"
                                  class="form-control"
                                  size="2"
                                  placeholder="pts"
                                  [(ngModel)]="column.points"/>
                              </div>
                            }
                          </div>
                        }
                      </td>
                    }
                    <td [ngClass]="['explField', row.expl ? 'hasText' : '']">
                      <textarea placeholder="Optional: Explain why answer is right/wrong" class="form-control"
                        (focus)="qctrl.explFocus($event)" (blur)="qctrl.explBlur($event)"
                        [(ngModel)]="row.expl"
                      rows="1"></textarea>
                    </td>
                    @if (!qctrl.isAskedQuestion() && qctrl.randomization) {
                      <td>
                        <label><input type="checkbox" [(ngModel)]="row.locked"/> Lock position</label>
                      </td>
                    }
                  </tr>
                }
                @if (!qctrl.isAskedQuestion()) {
                  <tr>
                    <td>
                      <div>
                        <button class="wideButton addRow" (click)="qctrl.addRow(-1)"><span
                        class="glyphicon glyphicon-plus"></span></button>
                      </div>
                    </td>
                  </tr>
                }
              </tbody>
            </table>
        `,
    standalone: false,
})
export class QuestionMatrixComponent {
    @Input() qctrl!: QuestionEditDialogComponent;
    @ViewChildren("txt") textareas!: QueryList<ElementRef<HTMLTextAreaElement>>;
}
