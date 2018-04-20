import {IRootElementService, IScope} from "angular";
import {timApp} from "../app";
import {DialogController, IModalInstance, registerDialogComponent, showDialog, showMessageDialog} from "../dialog";
import {ILectureMessage} from "../lecturetypes";
import {$http, $timeout} from "../ngimport";

class LectureWallController extends DialogController<{params: {lectureId: number, messages: ILectureMessage[]}}, {}, "timLectureWall"> {
    private static $inject = ["$element", "$scope"];
    private messageName = true;
    private messageTime = true;
    private newMsg: string;
    private messages: ILectureMessage[];
    private lectureId: number;
    private modalBody: HTMLElement | undefined;
    private msgCount = 0;

    constructor(element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    public $onInit() {
        super.$onInit();
        this.lectureId = this.resolve.params.lectureId;
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
        if (event.which === 13) {
            this.sendMessageEvent(this.newMsg);
        }
    }

    /**
     * Sends http request to send a message.
     * @param message The message to be sent.
     * @returns {boolean} Whether the message was sent successfully.
     */
    public async sendMessageEvent(message: string) {
        if (message.trim() === "") {
            await showMessageDialog("Can't send empty messages");
            return false;
        }
        await $http({
            url: "/sendMessage",
            method: "POST",
            params: {message},
        });
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
    "timLectureWall",
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
                <input class="form-control"
                       ng-model="$ctrl.newMsg"
                       ng-keypress="$ctrl.chatEnterPressed($event)"
                       placeholder="Type message...">
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

export function showLectureWall(lid: number, messages: ILectureMessage[]): IModalInstance<{}> {
    return showDialog<LectureWallController>("timLectureWall",
        {params: () => ({lectureId: lid, messages})},
        {size: "sm"});
}
