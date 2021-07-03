import {BrowserModule} from "@angular/platform-browser";
import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {of, Subject, Subscription} from "rxjs";
import {
    catchError,
    debounceTime,
    distinctUntilChanged,
    map,
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
                <section>
                    <label for="list-name" class="list-name-label" i18n>List name:</label>
                    <div class="list-name-group has-error">
                        <div class="input-group">
                            <input type="text" class="form-control" name="list-name" id="list-name"
                                   [(ngModel)]="listName"
                                   (ngModelChange)="nameTyped.next($event)"
                                   (keyup)="checkNameRequirementsLocally()"/>
                            <div class="input-group-addon">@</div>
                            <select id="domain-select" class="form-control" name="domain-select"
                                    [(ngModel)]="domain">
                                <option [disabled]="domains.length"
                                        *ngFor="let domain of domains">{{domain}}</option>
                            </select>
                        </div>
                        <tim-loading [style.visibility]="pollingAvailability ? 'visible' : 'hidden'"></tim-loading>
                    </div>
                    <div class="name-requirements">
                        <p>Name requirements:</p>
                        <ul>
                            <li><i class="glyphicon glyphicon-ok"></i> A name must be unique</li>
                            <li>Name length: 8-36 characters</li>
                        </ul>
                    </div>
                    <span id="archives-label">Who can read the archives:</span>
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
            </ng-container>
            <ng-container footer>
                <tim-loading *ngIf="disableCreate"></tim-loading>
                <button [disabled]="disableCreate" class="timButton" type="button" (click)="newList()" i18n>Create
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
    disableCreate: boolean = false;
    protected dialogName = "MessageList";
    listName: string = "";
    errorMessage: string[] = [];

    urlPrefix: string = "/messagelist";

    domains: string[] = [];
    domain: string = "";

    // List has a public archive by default.
    archive: ArchiveType = ArchiveType.PUBLIC;
    archiveOptions = archivePolicyNames;

    pollingAvailability: boolean = false;
    nameTyped: Subject<string> = new Subject<string>();
    nameTypedSubscription!: Subscription;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit(): void {
        if (Users.isLoggedIn()) {
            void this.getDomains();
        }

        this.nameTypedSubscription = this.nameTyped
            .pipe(
                debounceTime(1000),
                distinctUntilChanged(),
                tap(() => (this.pollingAvailability = true)),
                switchMap((name) =>
                    this.http.post<{rule_fails: string[]; exists: boolean}>(
                        `${this.urlPrefix}/checkname`,
                        {name}
                    )
                ),
                tap(() => (this.pollingAvailability = false)),
                catchError((e) => of({exists: false, rule_fails: []}))
            )
            .subscribe((res) => {
                console.log(res);
            });
    }

    ngOnDestroy() {
        super.ngOnDestroy();
        this.nameTypedSubscription.unsubscribe();
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
                $localize`Failed to load domains, list creation can't continue. The following error provides details: ${result.result.error.error}`,
            ];
            // Creating a message list isn't possible at this time if domains are not given. Therefore disable creation
            // button.
            this.disableCreate = true;
        }
    }

    /**
     * Launching the creation of a new list. Verifies the basic name rules in the client before involving the server.
     */
    async newList() {
        if (!this.checkNameRequirementsLocally()) {
            return;
        }

        this.errorMessage = [];
        this.disableCreate = true;
        const result = await this.createList({
            name: this.listName,
            domain: this.domain,
            archive: this.archive,
        });
        if (!result.ok) {
            this.errorMessage = [result.result.error.error];
            this.disableCreate = false;
        } else {
            redirectToItem(result.result);
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

    /**
     * Check list name requirements locally.
     *
     * If you make changes here, make sure to check that the server checks the same things. Otherwise there will
     * inconsistant name checking and a confused user.
     *
     * @returns {boolean} Returns true if name requirements are met. Otherwise returns false.
     */
    checkNameRequirementsLocally(): boolean {
        // Clear old error messages.
        this.errorMessage = [];

        // Name length is within length boundaries.
        if (this.listName.length <= 5 || 36 <= this.listName.length) {
            this.errorMessage.push("Name not in length boundaries");
        }

        // Name starts with a character that is a letter a - z.
        const regExpStartCharacter = /^[a-z]/;
        if (!regExpStartCharacter.test(this.listName)) {
            this.errorMessage.push("Name should start with a lowercase letter");
        }

        // Name contains at least one digit.
        const regExpAtLeastOneDigit = /\d/;
        if (!regExpAtLeastOneDigit.test(this.listName)) {
            this.errorMessage.push("Name should contain at least one digit");
        }

        // Name can't contain sequential dots.
        const regExpMultipleDots = /\.\.+/;
        if (regExpMultipleDots.test(this.listName)) {
            this.errorMessage.push("Name shouldn´t contain multiple dots");
        }

        // Name doesn't end in a dot.
        // ESLint prefers to not use regex for this. And by "prefer" we mean this won't transpile with a regular
        // expression.
        if (this.listName.endsWith(".")) {
            this.errorMessage.push("Name shouldn´t end in a dot");
        }

        // Name contains only acceptable characters, which are:
        //     letters                  a - z
        //     numbers                  0 - 9
        //     dot                      '.'
        //     underscore               '_'
        //     hyphen (or "minus sign") '-'
        // The following regular expression searches for characters that are *not* one of the above. If those are not
        // found the name is of correct form. Notice that hyphen is in two different roles and one hyphen has
        // to be escaped. The dot does not have to be escaped here.
        const regExpNonAllowedCharacters = /[^a-z0-9.\-_]/;
        if (regExpNonAllowedCharacters.test(this.listName)) {
            this.errorMessage.push("Name has forbidden characters");
        }
        return this.errorMessage.length == 0;
    }
}

@NgModule({
    declarations: [MessageListCreateDialogComponent],
    imports: [BrowserModule, DialogModule, FormsModule, TimUtilityModule],
})
export class MessageListModule {}
