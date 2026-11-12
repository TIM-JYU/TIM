import {Component, ChangeDetectionStrategy} from "@angular/core";
import {getVisibilityVars} from "tim/timRoot";
import {documentglobals, genericglobals} from "tim/util/globals";
import {showReportContentDialog} from "tim/ui/showReportContentDialog";
import {getLangLink, to2} from "tim/util/utils";

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
                      @if (!docGlobals.requires_login || !hide.links) {
                        <ng-container i18n>TIM last updated: </ng-container>
                      }
                      @if (hide.links && !docGlobals.requires_login) {
                        {{config.gitLatestCommitTimestamp}}
                        <br>
                          <ng-container i18n>Problems and questions about TIM</ng-container>: {{config.helpEmail}}
                        }
                        @if (!hide.links) {
                          <a href="/view/tim/muutoshistoria">{{config.gitLatestCommitTimestamp}}</a>
                          <br>
                            <ng-container i18n>Problems and questions about TIM</ng-container>
                            :
                            <a href="mailto:{{config.helpEmail}}">{{config.helpEmail}}</a>
                          }
                          @if (config.gitBranch != 'master' && !docGlobals.requires_login) {
                            (
                            <ng-container i18n>branch</ng-container>
                            : {{config.gitBranch}})
                          }
                          @if (docGlobals.requires_login && hide.links) {
                            <ng-container i18n>Problems and questions about TIM</ng-container>
                            :
                            <a href="mailto:{{config.helpEmail}}">{{config.helpEmail}}</a>
                          }
                        </p>
                      </div>
                      <div class="col-xs-6 text-right">
                        @if (!hide.links) {
                          @if (footerDocs.privacyNotice) {
                            <a href="/view/{{footerDocs.privacyNotice}}" i18n>
                                        Privacy notice
                                    </a>
                          }
                          @if (footerDocs.termsOfService) {
                            <br>
                              <a [href]="termsOfServiceLink" i18n>
                                            Terms of Service
                                        </a>
                            }
                            <br>
                              @if (footerDocs.accessibilityStatement) {
                                <a
                                  href="/view/{{footerDocs.accessibilityStatement}}" i18n>
                                        Accessibility statement
                                    </a>
                              }
                              <br>
                                <a (click)="reportContent()" role="button" i18n>Report inappropriate content</a>
                              }
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                  </footer>
        `,
    styleUrls: ["./footer.component.scss"],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class FooterComponent {
    hide = getVisibilityVars();
    docGlobals = documentglobals();
    config = genericglobals().config;
    layout = genericglobals().layout;
    footerDocs = genericglobals().footerDocs;

    get termsOfServiceLink() {
        return getLangLink("/view/" + this.footerDocs.termsOfService);
    }

    async reportContent() {
        const windowUrl = window.location.href;
        await to2(showReportContentDialog({currentUrl: windowUrl}));
    }
}
