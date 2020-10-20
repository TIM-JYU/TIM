import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule, NgZone} from "@angular/core";
import {to2} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {LectureWallContentModule} from "tim/lecture/lecture-wall-content.component";
import {DropdownCheckboxComponent} from "tim/lecture/dropdown-checkbox.component";
import {FormsModule} from "@angular/forms";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {ILectureMessage} from "./lecturetypes";

@Component({
    selector: "tim-lecture-wall-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <tim-lecture-wall-content
                        [messages]="messages"
                        [showName]="messageName"
                        [showTime]="messageTime">
                </tim-lecture-wall-content>
            </ng-container>
            <ng-container footer>
                <div class="row">
                    <div class="col-sm-8">
                        <div class="input-group">
                            <input class="form-control"
                                   [(ngModel)]="newMsg"
                                   (keydown.enter)="sendMessage()"
                                   placeholder="Type message...">
                            <span class="input-group-btn">
                       <button [disabled]="!newMsg"
                               title="Send message"
                               class="timButton"
                               (click)="sendMessage()">
                           <i class="glyphicon glyphicon-send"></i>
                       </button>
                    </span>
                        </div>
                    </div>
                    <div class="col-sm-4">
                        <div class="btn-group" dropdown>
                            <button id="single-button"
                                    type="button"
                                    class="btn btn-primary dropdown-toggle"
                                    dropdownToggle>
                                <i class="glyphicon glyphicon-wrench"></i>&ngsp;<span class="caret"></span>
                            </button>
                            <ul class="dropdown-menu" *dropdownMenu role="menu" aria-labelledby="single-button">
                                <tim-dropdown-checkbox [(for)]="messageName">Show names</tim-dropdown-checkbox>
                                <tim-dropdown-checkbox [(for)]="messageTime">Show times</tim-dropdown-checkbox>
                            </ul>
                        </div>
                    </div>
                </div>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class LectureWallDialogComponent extends AngularDialogComponent<
    {messages: ILectureMessage[]},
    void
> {
    protected dialogName = "LectureWall";
    messageName = true;
    messageTime = true;
    newMsg: string = "";
    messages: ILectureMessage[] = [];
    private msgCount = 0;

    constructor(private http: HttpClient, private zone: NgZone) {
        super();
    }

    public ngOnInit() {
        this.messages = this.data.messages;
    }

    public ngAfterViewChecked() {
        if (this.msgCount === this.messages.length) {
            return;
        }
        const e = this.frame.modalBody.nativeElement;
        this.msgCount = this.messages.length;
        e.scrollTop = e.scrollHeight;
    }

    checkChanges() {
        this.zone.run(() => {});
    }

    /**
     * Sends a message.
     */
    public async sendMessage() {
        const message = this.newMsg;
        if (message.trim() === "") {
            return;
        }
        await to2(
            this.http
                .post("/sendMessage", {
                    message,
                })
                .toPromise()
        );
        this.newMsg = "";
    }

    getTitle() {
        return "Lecture wall";
    }
}

@NgModule({
    declarations: [LectureWallDialogComponent, DropdownCheckboxComponent],
    imports: [
        BrowserModule,
        FormsModule,
        TimUtilityModule,
        DialogModule,
        LectureWallContentModule,
        BsDropdownModule.forRoot(),
    ],
    exports: [LectureWallDialogComponent],
})
export class LectureWallDialogModule {}
