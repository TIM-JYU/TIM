import {Component} from "@angular/core";
import {getVisibilityVars} from "tim/timRoot";
import {genericglobals} from "tim/util/globals";


@Component({
    selector: "tim-footer",
    template: `
        <footer class="footer hidden-print">
            <div class="container-fluid">
                <div class="row">
                    <div class="col-lg-{{ layout.col_2_lg }} col-lg-offset-{{ layout.col_1_lg }}
                            col-md-{{ layout.col_2_md }} col-md-offset-{{ layout.col_1_md }}
                            col-xs-{{ layout.col_2_xs }} col-xs-offset-{{ layout.col_1_xs }}
                            col-sm-{{ layout.col_2_sm }} col-sm-offset-{{ layout.col_1_sm }}">
                        <div class="row">
                            <div class="col-xs-6">
                                <p class="smallNote">TIM last updated:
                                    <ng-container *ngIf="hide.links">{{config.gitLastestCommitTimestamp}}
                                        <br>Problems and questions: {{config.helpEmail}}
                                    </ng-container>
                                    <ng-container *ngIf="!hide.links">
                                        <a href="/view/tim/muutoshistoria">{{config.gitLastestCommitTimestamp}}</a>
                                        <br>Problems and questions:
                                        <a href="mailto:{{config.helpEmail}}">{{config.helpEmail}}</a>
                                    </ng-container>
                                    <ng-container *ngIf="config.gitBranch != 'master'">
                                        (branch: {{config.gitBranch}})
                                    </ng-container>
                                </p>
                            </div>
                            <div class="col-xs-6 text-right smallNote">
                                <a *ngIf="!hide.links" href="/view/tim/Rekisteriseloste">
                                    Tietosuojalauseke, rekisteriseloste ja kerätyt tiedot
                                </a>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </footer>
    `,
    styleUrls: ["./footer.component.scss"],
})
export class TimFooterComponent {
    hide = getVisibilityVars();
    config = genericglobals().config;
    layout = genericglobals().layout;
}
