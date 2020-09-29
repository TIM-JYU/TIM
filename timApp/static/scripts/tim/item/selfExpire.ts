import {to} from "tim/util/utils";
import {$http} from "tim/util/ngimport";
import {showMessageDialog} from "tim/ui/dialog";
import {timApp} from "tim/app";

export class SelfExpireController {
    private confirm?: string;
    private buttonText?: string;
    private itemId!: number;

    $onInit() {
        if (!this.buttonText) {
            this.buttonText = "Remove your rights";
        }
    }

    async clicked() {
        if (!this.confirm || window.confirm(this.confirm)) {
            const r = await to(
                $http.post<unknown>("/permissions/selfExpire", {
                    id: this.itemId,
                })
            );
            if (r.ok) {
                location.reload();
            } else {
                await showMessageDialog(
                    `Error expiring right: ${r.result.data.error}`
                );
            }
        }
    }
}

timApp.component("timSelfExpire", {
    bindings: {
        buttonText: "<?",
        confirm: "<?",
        itemId: "<",
    },
    controller: SelfExpireController,
    template: `
<button class="timButton" ng-click="$ctrl.clicked()">{{ ::$ctrl.buttonText }}</button>
    `,
});
