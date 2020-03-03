import {IScope} from "angular";
import {to} from "tim/util/utils";
import {DialogController} from "tim/ui/dialogController";
import {timApp} from "../app";
import {registerDialogComponent, showDialog} from "../ui/dialog";
import {KEY_ENTER} from "../util/keycodes";
import {$http, $timeout} from "../util/ngimport";
import {ILectureMessage} from "./lecturetypes";

export class LectureWallController extends DialogController<{params: {messages: ILectureMessage[]}}, {}> {
    static component = "timLectureWall";
    static $inject = ["$element", "$scope"] as const;
    private messageName = true;
    private messageTime = true;
    private newMsg: string = "";
    private messages: ILectureMessage[] = [];
    private modalBody: HTMLElement | undefined;
    private msgCount = 0;

    constructor(element: JQLite, protected scope: IScope) {
        super(element, scope);
    }

    public $onInit() {
        super.$onInit();
        this.messages = this.resolve.params.messages;
    }

    public $postLink() {
        this.modalBody = this.element.find(".modal-body")[0];
    }

    public async $doCheck() {
        if (this.msgCount === this.messages.length) {
            return;
        }
        const e = this.modalBody;
        if (!e) {
            return;
        }
        this.msgCount = this.messages.length;
        await $timeout();
        e.scrollTop = e.scrollHeight;
    }

    /**
     * Event for pressing enter while writing message. Sends message.
     * @param event They key press.
     */
    public chatEnterPressed(event: KeyboardEvent) {
        if (event.which === KEY_ENTER) {
            this.sendMessage();
        }
    }

    /**
     * Sends http request to send a message.
     * @param message The message to be sent.
     * @returns {boolean} Whether the message was sent successfully.
     */
    public async sendMessage() {
        const message = this.newMsg;
        if (message.trim() === "") {
            return false;
        }
        await to($http({
            url: "/sendMessage",
            method: "POST",
            params: {message},
        }));
        this.newMsg = "";
    }

    protected getTitle() {
        return "Lecture wall";
    }
}

timApp.component("timDropdownCheckbox", {
    bindings: {
        for: "=",
    },
    template: `
<li role="menuitem"><a ng-click="$ctrl.for = !$ctrl.for; $event.stopPropagation()">
    <i class="glyphicon"
       ng-class="{'glyphicon-check': $ctrl.for, 'glyphicon-unchecked': !$ctrl.for}"></i>
    <span ng-transclude></span></a>
</li>
    `,
    transclude: true,
});

timApp.component("timLectureWallContent", {
    bindings: {
        messages: "<",
        showName: "<",
        showTime: "<",
    },
    template: `
<ul class="list-unstyled">
    <li ng-repeat="m in $ctrl.messages">
        <span ng-if="$ctrl.showName">{{m.user.name}}</span>
        <span ng-if="$ctrl.showTime">&lt;{{ m.timestamp | timtim }}&gt;</span>
        <span ng-if="$ctrl.showTime || $ctrl.showName">:</span>
        {{m.message}}
    </li>
</ul>
    `,
});

registerDialogComponent(
    LectureWallController,
    {
        template: `
<tim-dialog>
    <dialog-header>
        Lecture wall
    </dialog-header>
    <dialog-body>
        <tim-lecture-wall-content
                messages="$ctrl.messages"
                show-name="$ctrl.messageName"
                show-time="$ctrl.messageTime">
        </tim-lecture-wall-content>
    </dialog-body>
    <dialog-footer>
        <div class="row">
            <div class="col-sm-8">
                <div class="input-group">
                    <input class="form-control"
                           ng-model="$ctrl.newMsg"
                           ng-keypress="$ctrl.chatEnterPressed($event)"
                           placeholder="Type message...">
                    <span class="input-group-btn">
                       <button ng-disabled="!$ctrl.newMsg"
                               title="Send message"
                               class="timButton"
                               ng-click="$ctrl.sendMessage()">
                           <i class="glyphicon glyphicon-send"></i>
                       </button>
                    </span>
                </div>
            </div>
            <div class="col-sm-4">
                <div class="btn-group" uib-dropdown is-open="status.isopen">
                    <button id="single-button" type="button" class="btn btn-primary" uib-dropdown-toggle
                            ng-disabled="disabled">
                        <i class="glyphicon glyphicon-wrench"></i> <span class="caret"></span>
                    </button>
                    <ul class="dropdown-menu" uib-dropdown-menu role="menu" aria-labelledby="single-button">
                        <tim-dropdown-checkbox for="$ctrl.messageName">Show names</tim-dropdown-checkbox>
                        <tim-dropdown-checkbox for="$ctrl.messageTime">Show times</tim-dropdown-checkbox>
                    </ul>
                </div>
            </div>
        </div>
    </dialog-footer>
</tim-dialog>
        `,
    });

export function showLectureWall(messages: ILectureMessage[]) {
    return showDialog(LectureWallController,
        {params: () => ({messages})},
        {size: "sm"});
}
