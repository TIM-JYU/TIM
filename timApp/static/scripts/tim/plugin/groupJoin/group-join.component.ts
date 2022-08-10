import {ApplicationRef, Component, DoBootstrap, NgModule} from "@angular/core";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import * as t from "io-ts";
import {TimUtilityModule} from "../../ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
    withDefault,
} from "../attributes";
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
            join: nullable(t.string),
            joined: nullable(t.string),
            leave: nullable(t.string),
            left: nullable(t.string),
            joinConfirmTitle: nullable(t.string),
            joinConfirmMessage: nullable(t.string),
            leaveConfirmTitle: nullable(t.string),
            leaveConfirmMessage: nullable(t.string),
        }),
    }),
    t.type({
        join: withDefault(t.boolean, true),
        leave: withDefault(t.boolean, true),
        autoRefresh: withDefault(t.boolean, false),
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
            <button class="timButton" [disabled]="!enabled || loading" (click)="process()">{{status}}</button>
            <tim-loading *ngIf="loading"></tim-loading>
        </div>
        <tim-alert *ngIf="errorMessage" severity="danger" i18n>Could not change group membership: {{ errorMessage }}</tim-alert>
        <tim-alert *ngIf="result && !result.ok" severity="warning">
            <p i18n>Updated some group memberships:</p>
            <ul>
                <li *ngFor="let group of getResultGroups(result)">{{group[0]}}: {{ group[1] }}</li>
            </ul>
            <p><a [href]="docPath" i18n>Refresh page</a></p>
        </tim-alert>
        <tim-alert *ngIf="result && result.ok" severity="success">
            <p i18n>Updated group memberships for: {{ groups }}</p>
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
    leaveText = $localize`Leave groups`;
    leftText = $localize`You left all groups`;

    joined = false;
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
        this.leaveText = this.markup.texts?.leave ?? this.leaveText;
        this.leftText = this.markup.texts?.left ?? this.leftText;

        if (this.markup.groups?.length) {
            const currentGroups = new Set(
                Users.getCurrent().groups.map((g) => g.name)
            );
            this.joined = this.markup.groups.every((g) => currentGroups.has(g));
        }
        this.enabled =
            (!this.joined && this.markup.join) ||
            (this.joined && this.markup.leave);

        this.docPath = `/${getViewName()}/${documentglobals().curr_item.path}`;
    }

    get status() {
        if (this.enabled) {
            return !this.joined ? this.joinText : this.leaveText;
        } else {
            return this.joined ? this.joinedText : this.leftText;
        }
    }

    async process() {
        this.loading = true;
        this.errorMessage = undefined;
        this.result = undefined;
        const run = !this.joined ? this.joinGroups : this.leaveGroups;
        const ok = await run.call(this);
        this.loading = false;
        if (ok && this.markup.autoRefresh) {
            window.location.reload();
        }
    }

    private async joinGroups() {
        if (this.markup.confirm) {
            if (
                !(await showConfirm(
                    this.markup.texts?.joinConfirmTitle ??
                        $localize`Join a group?`,
                    this.markup.texts?.joinConfirmMessage ??
                        $localize`You will join the following groups: ${this.groups.join(
                            ", "
                        )}. Proceed?`
                ))
            ) {
                return;
            }
        }

        const r = await toPromise(
            this.http.post<JoinGroupResult>("/groupJoin/joinGroups", {
                groups: this.markup.groups,
            })
        );

        if (r.ok) {
            this.result = r.result;
            this.joined = true;
            this.enabled = this.markup.leave;
        } else {
            this.errorMessage = r.result.error.error;
        }

        return r.ok;
    }

    private async leaveGroups() {
        if (this.markup.confirm) {
            if (
                !(await showConfirm(
                    this.markup.texts?.leaveConfirmTitle ??
                        $localize`Leave a group?`,
                    this.markup.texts?.leaveConfirmMessage ??
                        $localize`You will leave the following groups: ${this.groups.join(
                            ", "
                        )}. Proceed?`
                ))
            ) {
                return;
            }
        }

        const r = await toPromise(
            this.http.post<JoinGroupResult>("/groupJoin/leaveGroups", {
                groups: this.markup.groups,
            })
        );

        if (r.ok) {
            this.result = r.result;
            this.joined = false;
            this.enabled = this.markup.join;
        } else {
            this.errorMessage = r.result.error.error;
        }

        return r.ok;
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
