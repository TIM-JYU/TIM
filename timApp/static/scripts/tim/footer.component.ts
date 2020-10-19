import {Component} from "@angular/core";
import {getVisibilityVars} from "tim/timRoot";
import {genericglobals} from "tim/util/globals";

@Component({
    selector: "tim-footer",
    template: `
        <footer class="hidden-print">
            <div class="container-fluid">
                <div class="row">
                    <div class="col-lg-{{ layout.col_2_lg }} col-lg-offset-{{ layout.col_1_lg }}
                            col-md-{{ layout.col_2_md }} col-md-offset-{{ layout.col_1_md }}
                            col-xs-{{ layout.col_2_xs }} col-xs-offset-{{ layout.col_1_xs }}
                            col-sm-{{ layout.col_2_sm }} col-sm-offset-{{ layout.col_1_sm }}">
                        <div class="row">
                            <div class="col-xs-6">
                                <p>
                                    <ng-container i18n>TIM last updated</ng-container>:
                                    <ng-container *ngIf="hide.links">{{config.gitLastestCommitTimestamp}}
                                        <br>
                                        <ng-container i18n="@@probQuest">Problems and questions</ng-container>: {{config.helpEmail}}
                                    </ng-container>
                                    <ng-container *ngIf="!hide.links">
                                        <a href="/view/tim/muutoshistoria">{{config.gitLastestCommitTimestamp}}</a>
                                        <br>
                                        <ng-container i18n="@@probQuest">Problems and questions</ng-container>:
                                        <a href="mailto:{{config.helpEmail}}">{{config.helpEmail}}</a>
                                    </ng-container>
                                    <ng-container *ngIf="config.gitBranch != 'master'">
                                        (<ng-container i18n>branch</ng-container>: {{config.gitBranch}})
                                    </ng-container>
                                </p>
                            </div>
                            <div class="col-xs-6 text-right">
                                <ng-container *ngIf="!hide.links">
                                    <a href="/view/tim/Rekisteriseloste" i18n>
                                        Privacy policy
                                    </a>
                                    <br>
                                    <a href="/view/tim/saavutettavuusseloste" i18n>
                                        Accessibility statement
                                    </a>
                                </ng-container>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </footer>
    `,
    styleUrls: ["./footer.component.scss"],
})
export class FooterComponent {
    hide = getVisibilityVars();
    config = genericglobals().config;
    layout = genericglobals().layout;
}
