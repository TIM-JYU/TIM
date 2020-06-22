import {Component, OnInit} from "@angular/core";
import {ScoreboardService} from "tim/sidebarmenu/services/scoreboard.service";

@Component({
    selector: "score-info-tab",
    template: `
        <ng-template i18n="@@scoreInfoTabTitle">Scoreboard</ng-template>
        <div class="points-list" *ngIf="scoreBoard.currentDocScoreInfo">
            <div class="collapse-header">
                <a (click)="hidePagePoints = !hidePagePoints">
                    <h5 i18n>Points on this page</h5>
                    <i class="pull-right glyphicon glyphicon-menu-down"></i>
                </a>
            </div>
            <div [collapse]="hidePagePoints" [isAnimated]="true">
                <div class="points-header">
                    <p i18n>Task</p>
                    <p class="pull-right" i18n>Points</p>
                </div>
                <ul class="score-list">
                    <li *ngFor="let task of scoreBoard.currentDocScoreInfo.tasks" class="flex flex-wrap">
                        <div class="flex-grow-1">
                            <a href="{{pageUrl}}#{{task.fragId}}">{{task.taskName}}</a>
                        </div>
                        <div class="flex-grow-1">
                            <p>
                                {{task.points ? task.points : 0}}
                                <span class="full-points"> / {{task.maxPoints}}</span>
                            </p>
                        </div>
                    </li>
                    <li class="point-total">
                        <ng-container i18n>Total</ng-container>: {{scoreBoard.currentDocScoreInfo.total}}
                        <span class="full-points"> / {{scoreBoard.currentDocScoreInfo.maxTotal}}</span>
                    </li>
                </ul>
            </div>
        </div>
        <div *ngIf="scoreBoard.infos && scoreBoard.infos.length" class="scoreboard points-list no-bottom">
            <div class="collapse-header">
                <a (click)="hideCoursePoints = !hideCoursePoints">
                    <h5 i18n>Points in this course</h5>
                    <i class="pull-right glyphicon glyphicon-menu-down"></i>
                </a>
            </div>
            <div [collapse]="hideCoursePoints" [isAnimated]="true">
                <div class="points-header">
                    <p i18n>Page</p>
                    <p class="pull-right" i18n>Points</p>
                </div>
                <ul class="score-list">
                    <li *ngFor="let info of scoreBoard.infos">
                        <div class="flex flex-wrap">
                            <div class="flex-grow-1">
                                <a href="/view/{{info.doc.path}}">{{info.doc.title}}</a>
                            </div>
                            <div class="flex-grow-1">
                                <p>
                                    {{info.total}}
                                    <span class="full-points"> / {{info.maxTotal}}</span>
                                </p>
                            </div>
                        </div>
                    </li>
                    <li class="point-total">
                        <ng-container i18n>Total</ng-container>: {{scoreBoard.total}}
                        <span class="full-points"> / {{scoreBoard.maxTotal}}</span>
                    </li>
                </ul>
            </div>
        </div>
    `,
    styleUrls: ["./score-info-tab.component.scss"],
})
export class ScoreInfoTabComponent implements OnInit {
    scoreBoard: ScoreboardService;
    hidePagePoints: boolean = false;
    hideCoursePoints: boolean = false;
    pageUrl = `${document.location.origin}${document.location.pathname}${document.location.search}`;

    constructor(scoreBoard: ScoreboardService) {
        this.scoreBoard = scoreBoard;
    }

    ngOnInit(): void {
    }
}
