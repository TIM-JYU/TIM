import {Component} from "@angular/core";
import {getVisibilityVars} from "tim/timRoot";
import {documentglobals, genericglobals} from "tim/util/globals";
import {Users} from "tim/user/userService";
import {showReportContentDialog} from "tim/ui/showReportContentDialog";
import {to2} from "tim/util/utils";

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
                                    <ng-container *ngIf="!docGlobals.requires_login || !hide.links" i18n>TIM last updated: </ng-container>
                                    <ng-container *ngIf="hide.links && !docGlobals.requires_login">{{config.gitLatestCommitTimestamp}}
                                        <br>    
                                        <ng-container i18n>Problems and questions about TIM</ng-container>: {{config.helpEmail}}
                                    </ng-container>
                                    <ng-container *ngIf="!hide.links">
                                        <a href="/view/tim/muutoshistoria">{{config.gitLatestCommitTimestamp}}</a>
                                        <br>
                                        <ng-container i18n>Problems and questions about TIM</ng-container>
                                        :
                                        <a href="mailto:{{config.helpEmail}}">{{config.helpEmail}}</a>
                                    </ng-container>
                                    <ng-container *ngIf="config.gitBranch != 'master' && !docGlobals.requires_login">
                                        (
                                        <ng-container i18n>branch</ng-container>
                                        : {{config.gitBranch}})
                                    </ng-container>
                                    <ng-container *ngIf="docGlobals.requires_login && hide.links">
                                        <ng-container i18n>Problems and questions about TIM</ng-container>
                                        :
                                        <a href="mailto:{{config.helpEmail}}">{{config.helpEmail}}</a>
                                    </ng-container>
                                </p>
                            </div>
                            <div class="col-xs-6 text-right">
                                <ng-container *ngIf="!hide.links">
                                    <a *ngIf="footerDocs.privacyNotice" href="/view/{{footerDocs.privacyNotice}}" i18n>
                                        Privacy notice
                                    </a>
                                    <ng-container *ngIf="footerDocs.termsOfService">
                                        <br>
                                        <a href="{{getLangLink('/view/' + footerDocs.termsOfService)}}" i18n>
                                            Terms of Service
                                        </a>
                                    </ng-container>
                                    <br>
                                    <a *ngIf="footerDocs.accessibilityStatement"
                                       href="/view/{{footerDocs.accessibilityStatement}}" i18n>
                                        Accessibility statement
                                    </a>
                                    <br>
                                    <a (click)="reportContent()" role="button" i18n>Report inappropriate content</a>
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
    docGlobals = documentglobals();
    config = genericglobals().config;
    layout = genericglobals().layout;
    footerDocs = genericglobals().footerDocs;

    getLangLink(link: string): string {
        const currentLang = Users.getCurrentLanguage();
        // For the basecase, with language fi, return unmodified link
        if (currentLang == "fi") {
            return link;
        }
        // For languages not handled, default to english
        return link + "/en-US";
    }

    async reportContent() {
        const windowUrl = window.location.href;
        const response = await to2(
            showReportContentDialog({currentUrl: windowUrl})
        );
        if (response.ok) {
        }
    }
}
