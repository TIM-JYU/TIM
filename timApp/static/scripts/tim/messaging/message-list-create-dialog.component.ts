import {BrowserModule} from "@angular/platform-browser";
import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {of, Subject, Subscription} from "rxjs";
import {
    catchError,
    debounceTime,
    distinctUntilChanged,
    filter,
    switchMap,
    tap,
} from "rxjs/operators";
import {AngularDialogComponent} from "../ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {to2} from "../util/utils";
import {Users} from "../user/userService";
import {IDocument, redirectToItem} from "../item/IItem";
import {TimUtilityModule} from "../ui/tim-utility.module";
import {archivePolicyNames, ArchiveType, ListOptions} from "./listOptionTypes";

enum NameRequirements {
    ERROR = -1,
    NAME_LENGTH_BOUNDED = 0,
    START_WITH_LOWERCASE = 1,
    NO_SEQUENTIAL_DOTS = 2,
    NO_TRAILING_DOTS = 3,
    NO_FORBIDDEN_CHARS = 4,
    MIN_ONE_DIGIT = 5,
}

type AnnotatedNameRequirements = Exclude<
    NameRequirements,
    NameRequirements.ERROR
>;

const NAME_RULES: Record<AnnotatedNameRequirements, string> = {
    [NameRequirements.NAME_LENGTH_BOUNDED]: $localize`Length: 5-36 characters`,
    [NameRequirements.NO_FORBIDDEN_CHARS]: $localize`Allowed characters: letters A-z, numbers 0-9, dots, dash, underscore`,
    [NameRequirements.MIN_ONE_DIGIT]: $localize`Has at least one digit`,
    [NameRequirements.START_WITH_LOWERCASE]: $localize`Begins with small letter`,
    [NameRequirements.NO_SEQUENTIAL_DOTS]: $localize`Must not have subsequent dots`,
    [NameRequirements.NO_TRAILING_DOTS]: $localize`Must not end with a dot`,
};

@Component({
    selector: "message-list-creation",
    template: `
        <tim-dialog-frame class="form-horizontal">
            <ng-container header i18n>
                Create message list
            </ng-container>
            <ng-container body>
                <div *ngIf="errorMessage.length > 0" class="alert alert-danger">
                    <ul>
                        <li *ngFor="let error of errorMessage">{{error}}</li>
                    </ul>
                </div>
                <form>
                    <fieldset [disabled]="creatingList">
                        <section>
                            <label for="list-name" class="list-name-label" i18n>List name:</label>
                            <div class="list-name-group" [class.has-error]="!nameValid">
                                <div class="input-group">
                                    <input type="text" class="form-control" name="list-name" id="list-name"
                                           [(ngModel)]="listName"
                                           (ngModelChange)="nameTyped.next($event)"/>
                                    <div class="input-group-addon">@</div>
                                    <select id="domain-select" class="form-control" name="domain-select"
                                            [(ngModel)]="domain">
                                        <option [disabled]="domains.length"
                                                *ngFor="let domain of domains">{{domain}}</option>
                                    </select>
                                </div>
                                <tim-loading
                                        [style.visibility]="pollingAvailability ? 'visible' : 'hidden'"></tim-loading>
                            </div>
                            <div class="name-requirements">
                                <p i18n>Name requirements:</p>
                                <ul>
                                    <li>
                                        <span class="req-check" [class.ok]="!nameExists">
                                            <i class="glyphicon glyphicon-unchecked"></i>
                                            <i class="glyphicon glyphicon-ok"></i>
                                        </span>
                                        <span i18n>Must be unique</span>
                                    </li>
                                    <li *ngFor="let rule of rules">
                                        <span class="req-check" [class.ok]="!failedRequirements.has(rule)">
                                            <i class="glyphicon glyphicon-unchecked"></i>
                                            <i class="glyphicon glyphicon-ok"></i>
                                        </span>
                                        <span>{{rules_names[rule]}}</span>
                                    </li>
                                </ul>
                            </div>
                            <span id="archives-label" i18n>Who can read the archives:</span>
                            <ul class="archive-list" role="radiogroup" aria-labelledby="archives-label">
                                <li *ngFor="let option of archiveOptions">
                                    <label class="radio" for="archive-{{option.archiveType}}">
                                        <input
                                                name="items-radio"
                                                type="radio"
                                                id="archive-{{option.archiveType}}"
                                                [value]="option.archiveType"
                                                [(ngModel)]="archive"
                                        />
                                        {{option.policyName}}
                                    </label>
                                </li>
                            </ul>
                        </section>
                    </fieldset>
                </form>
            </ng-container>
            <ng-container footer>
                <tim-loading *ngIf="creatingList"></tim-loading>
                <button [disabled]="!canCreate || creatingList" class="timButton" type="button" (click)="newList()"
                        i18n>
                    Create
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["message-list-create-dialog.component.scss"],
})
export class MessageListCreateDialogComponent extends AngularDialogComponent<
    unknown,
    unknown
> {
    creatingList: boolean = false;
    listName: string = "";
    errorMessage: string[] = [];
    urlPrefix: string = "/messagelist";
    domains: string[] = [];
    domain: string = "";
    rules_names = NAME_RULES;
    rules = Object.keys(NAME_RULES).map(
        (k) => Number.parseInt(k, 10) as AnnotatedNameRequirements
    );
    // List has a public archive by default.
    archive: ArchiveType = ArchiveType.PUBLIC;
    archiveOptions = archivePolicyNames;
    pollingAvailability: boolean = false;
    nameTyped: Subject<string> = new Subject<string>();
    nameTypedSubscription!: Subscription;
    nameValid: boolean = true;
    nameExists: boolean = true;
    canCreate: boolean = false;
    failedRequirements = new Set<AnnotatedNameRequirements>(this.rules);
    protected dialogName = "MessageList";

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit(): void {
        if (Users.isLoggedIn()) {
            void this.getDomains();
        }

        const allAnnotated = (
            reqs: NameRequirements[]
        ): reqs is AnnotatedNameRequirements[] =>
            reqs.every((r) => r != NameRequirements.ERROR);

        this.nameTypedSubscription = this.nameTyped
            .pipe(
                tap((n) => {
                    if (!n) {
                        this.nameExists = true;
                        this.failedRequirements = new Set<
                            AnnotatedNameRequirements
                        >(this.rules);
                    }
                    this.canCreate = false;
                    this.nameValid = true;
                }),
                debounceTime(500),
                distinctUntilChanged(),
                filter((s) => s.length != 0),
                tap(() => (this.pollingAvailability = true)),
                switchMap((name) =>
                    this.http.post<{
                        rule_fails: NameRequirements[];
                        exists: boolean;
                    }>(`${this.urlPrefix}/checkname`, {name})
                ),
                tap(() => (this.pollingAvailability = false)),
                catchError(() =>
                    of({
                        rule_fails: [NameRequirements.ERROR],
                        exists: false,
                    })
                )
            )
            .subscribe((res) => {
                this.nameValid = !res.exists && res.rule_fails.length == 0;
                this.canCreate = this.nameValid;
                this.nameExists = res.exists;
                if (!allAnnotated(res.rule_fails)) {
                    this.errorMessage = [
                        $localize`Failed to check name requirements, please try again`,
                    ];
                    return;
                }
                this.errorMessage = [];
                this.failedRequirements = new Set<AnnotatedNameRequirements>(
                    res.rule_fails
                );
            });
    }

    ngOnDestroy() {
        super.ngOnDestroy();
        this.nameTypedSubscription.unsubscribe();
    }

    /**
     * Launching the creation of a new list. Verifies the basic name rules in the client before involving the server.
     */
    async newList() {
        this.errorMessage = [];
        this.creatingList = true;
        const result = await this.createList({
            name: this.listName,
            domain: this.domain,
            archive: this.archive,
        });
        if (!result.ok) {
            this.errorMessage = [result.result.error.error];
            this.creatingList = false;
        } else {
            redirectToItem(result.result);
        }
    }

    /**
     * Fetch possible domains to be used for email lists.
     * @private
     */
    private async getDomains() {
        const result = await to2(
            this.http.get<string[]>(`${this.urlPrefix}/domains`).toPromise()
        );
        if (result.ok) {
            this.domains = result.result;

            // Set default domain.
            this.domain = this.domains[0];
        } else {
            this.errorMessage = [
                $localize`Failed to load email domains. The following error provides details: ${result.result.error.error}`,
            ];
            // Creating a message list isn't possible at this time if domains are not given. Therefore disable creation
            // button.
            this.creatingList = true;
        }
    }

    /**
     * The call to create new list.
     * @param options Required amount of options to create a new message list. Here the necessary arguments are list's
     * name and archive policy.
     * @private
     */
    private createList(options: ListOptions) {
        return to2(
            this.http
                .post<IDocument>(`${this.urlPrefix}/createlist`, {options})
                .toPromise()
        );
    }
}

@NgModule({
    declarations: [MessageListCreateDialogComponent],
    imports: [BrowserModule, DialogModule, FormsModule, TimUtilityModule],
})
export class MessageListModule {}
