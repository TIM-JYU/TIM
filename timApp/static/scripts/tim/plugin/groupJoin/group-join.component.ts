import {ApplicationRef, Component, DoBootstrap, NgModule} from "@angular/core";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import * as t from "io-ts";
import {TimUtilityModule} from "../../ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {GenericPluginMarkup, getTopLevelFields} from "../attributes";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {getViewName, toPromise} from "../../util/utils";
import {documentglobals} from "../../util/globals";
import {Users} from "../../user/userService";
import {showConfirm} from "../../ui/showConfirmDialog";

const PluginMarkup = t.intersection([
    GenericPluginMarkup,
    t.partial({
        groups: t.array(t.string),
        confirm: t.boolean,
        texts: t.partial({
            join: t.string,
            joined: t.string,
            confirmTitle: t.string,
            confirmMessage: t.string,
        }),
    }),
]);

const PluginFields = t.intersection([
    getTopLevelFields(PluginMarkup),
    t.type({}),
]);

type JoinGroupResult = {ok: boolean; result: Record<string, string>};

@Component({
    selector: "tim-group-join",
    template: `
        <div class="button-panel">
            <button class="timButton" [disabled]="!enabled || loading" (click)="joinGroups()">{{status}}</button>
            <tim-loading *ngIf="loading"></tim-loading>
        </div>
        <tim-alert *ngIf="errorMessage" severity="danger" i18n>Could not join groups: {{ errorMessage }}</tim-alert>
        <tim-alert *ngIf="result && !result.ok" severity="warning">
            <p i18n>Joined some groups:</p>
            <ul>
                <li *ngFor="let group of getResultGroups(result)">{{group[0]}}: {{ group[1] }}</li>
            </ul>
            <p><a [href]="docPath" i18n>Refresh page</a></p>
        </tim-alert>
        <tim-alert *ngIf="result && result.ok" severity="success">
            <p i18n>Joined groups: {{ groups }}</p>
            <p><a [href]="docPath" i18n>Refresh page</a></p>
        </tim-alert>
  `,
    styleUrls: ["./group-join.component.scss"],
})
export class GroupJoinComponent extends AngularPluginBase<
    t.TypeOf<typeof PluginMarkup>,
    t.TypeOf<typeof PluginFields>,
    typeof PluginFields
> {
    joinText = $localize`Join groups`;
    joinedText = $localize`You already joined all groups`;

    status?: string;
    enabled = false;
    loading = false;
    errorMessage?: string;
    result?: JoinGroupResult;
    docPath?: string;

    get groups() {
        return this.markup.groups ?? [];
    }

    getResultGroups(result: JoinGroupResult): [string, string][] {
        return Object.entries(result.result);
    }

    ngOnInit(): void {
        super.ngOnInit();
        this.joinText = this.markup.texts?.join ?? this.joinText;
        this.joinedText = this.markup.texts?.joined ?? this.joinedText;

        if (this.markup.groups?.length) {
            const currentGroups = new Set(
                Users.getCurrent().groups.map((g) => g.name)
            );
            this.enabled = this.markup.groups.some(
                (g) => !currentGroups.has(g)
            );
        }
        this.status = this.enabled ? this.joinText : this.joinedText;
        this.docPath = `/${getViewName()}/${documentglobals().curr_item.path}`;
    }

    async joinGroups() {
        if (this.markup.confirm) {
            if (
                !(await showConfirm(
                    this.markup.texts?.confirmTitle ?? $localize`Join a group?`,
                    this.markup.texts?.confirmMessage ??
                        $localize`You will join the following groups: ${this.groups.join(
                            ", "
                        )}. Proceed?`
                ))
            ) {
                return;
            }
        }

        this.loading = true;
        this.errorMessage = undefined;
        this.result = undefined;

        const r = await toPromise(
            this.http.post<JoinGroupResult>("/groupJoin/joinGroups", {
                groups: this.markup.groups,
            })
        );

        if (r.ok) {
            this.result = r.result;
        } else {
            this.errorMessage = r.result.error.error;
        }

        this.loading = false;
    }

    getAttributeType() {
        return PluginFields;
    }

    getDefaultMarkup() {
        return {};
    }
}

@NgModule({
    declarations: [GroupJoinComponent],
    imports: [BrowserModule, HttpClientModule, FormsModule, TimUtilityModule],
})
export class GroupJoinModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                GroupJoinModule
            )
        ),
        "groupJoin",
        GroupJoinComponent
    ),
];
