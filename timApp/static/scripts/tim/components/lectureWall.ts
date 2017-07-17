import {IController, IScope} from "angular";
import {timApp} from "../app";
import {IMessage} from "../lecturetypes";
import {showDialog} from "../dialog";
import {$http} from "../ngimport";

class LectureWallController implements IController {
    private messageName: boolean;
    private messageTime: boolean;
    private newMsg: string;
    private readonly messages: IMessage[];
    private readonly lectureId: number;

    constructor() {
        this.messageName = true;
        this.messageTime = true;
    }

    $onInit() {
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
    template: `<div class="wall-message-area">
    <ul class="list-unstyled" id="wallArea">
        <li ng-repeat="m in $ctrl.messages">
            <span ng-if="$ctrl.messageName">{{m.sender}}</span>
            <span ng-if="$ctrl.messageTime">&lt;{{m.time}}&gt;</span>
            <span ng-if="$ctrl.messageTime || $ctrl.messageName">:</span>
            {{m.message}}
        </li>
    </ul>
</div>
<div class="wall-footer">
    <div class="wall-buttons">
        <input id="messagebox" ng-model="$ctrl.newMsg" width="200"
               ng-keypress="$ctrl.chatEnterPressed($event)">
    </div>
    <div class="wall-info-buttons">
        <label title="Shows name of the sender">
            <input type="checkbox" ng-model="$ctrl.messageName"/>Name
        </label>
        <label title="Shows sending time">
            <input type="checkbox" ng-model="$ctrl.messageTime"/>Time
        </label>
    </div>
</div>
`,
});
