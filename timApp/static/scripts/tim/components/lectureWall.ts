import {IController, IScope} from "angular";
import {timApp} from "../app";
import {IMessage} from "../lecturetypes";
import {showDialog} from "../dialog";
import {$http} from "../ngimport";

class LectureWallController implements IController {
    private static $inject = ["$scope"];
    private messageName: boolean;
    private messageTime: boolean;
    private msg: string;
    private newMsg: string;
    private readonly messages: IMessage[];
    private readonly lectureId: number;
    private scope: IScope;

    constructor(scope: IScope) {
        this.scope = scope;
        this.messageName = true;
        this.messageTime = true;
    }

    $onInit() {
        this.scope.$watch(() => this.messages, () => this.showInfo());
    }

    /**
     * Depending on what users wants to see on the wall, makes the msg to correct form. Able to show the
     * name of the sender, time and message. Sender and time are optional.
     */
    showInfo() {
        this.msg = "";
        if (this.messageName && this.messageTime) {
            for (const msg of this.messages) {
                this.msg += msg.sender;
                this.msg += " <" + msg.time + ">: ";
                this.msg += msg.message + "\r\n";
            }
        } else if (!this.messageName && this.messageTime) {
            for (const msg of this.messages) {
                this.msg += " <" + msg.time + ">: ";
                this.msg += msg.message + "\r\n";
            }
        } else if (this.messageName && !this.messageTime) {
            for (const msg of this.messages) {
                this.msg += msg.sender + ": ";
                this.msg += msg.message + "\r\n";
            }
        } else if (!this.messageName && !this.messageTime) {
            for (const msg of this.messages) {
                this.msg += ">" + msg.message + "\r\n";
            }
        }
    }

    /**
     * Event for pressing enter while writing message. Sends message.
     * @param event They key press.
     * @memberof module:lectureController
     */
    chatEnterPressed(event: KeyboardEvent) {
        if (event.which === 13) {
            this.sendMessageEvent(this.newMsg);
        }
    }

    /**
     * Sends http request to send a message.
     * @param message The message to be sent.
     * @returns {boolean} Whether the message was sent successfully.
     * @memberof module:lectureController
     */
    async sendMessageEvent(message: string) {
        if (message.trim() === "") {
            showDialog("Can't send empty messages");
            return false;
        }
        await $http({
            url: "/sendMessage",
            method: "POST",
            params: {message, lecture_id: this.lectureId},
        });
        this.newMsg = "";
        // TODO: Fix this to scroll bottom without cheating.
        const wallArea = $("#wallArea");
        wallArea.animate({scrollTop: wallArea[0].scrollHeight * 10}, 1000);
    }
}

timApp.component("timLectureWall", {
    bindings: {
        lectureId: "<",
        messages: "<",
    },
    controller: LectureWallController,
    template: `
<div class="wall-base" id="wallBase">
    <div class="wall-message-area">
        <textarea id="wallArea" readonly cols="100">{{$ctrl.msg}}</textarea>
    </div>
    <div class="wall-footer">
        <div class="wall-buttons">
            <input id="messagebox" ng-model="$ctrl.newMsg" width="200"
                   ng-keypress="$ctrl.chatEnterPressed($event)">
        </div>
        <div class="wall-info-buttons">
            <label title="Shows name of the sender">
                <input type="checkbox" ng-change="$ctrl.showInfo()" ng-model="$ctrl.messageName"/>Name
            </label>
            <label title="Shows sending time">
                <input type="checkbox" ng-change="$ctrl.showInfo()" ng-model="$ctrl.messageTime"/>Time
            </label>
        </div>
    </div>
</div>
`,
});
