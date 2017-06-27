import angular from "angular";
import {timApp} from "tim/app";
import * as formErrorMessage from "tim/directives/formErrorMessage";
import * as shortNameValidator from "tim/directives/shortNameValidator";
import {markAsUsed} from "tim/utils";
import {$http, $window} from "../ngimport";
import {slugify} from "../services/slugify";

markAsUsed(formErrorMessage, shortNameValidator);

class CreateItemController {
    private fullPath: string;
    private automaticShortName: boolean;
    private itemLocation: string;
    private itemTitle: string;
    private itemName: string;
    private alerts: {}[];
    private itemType: string;
    private params: {};

    constructor() {
        this.automaticShortName = true;

        if (this.fullPath) {
            const str = this.fullPath;
            this.itemLocation = str.substring(0, str.lastIndexOf("/"));
            this.itemTitle = str.substring(str.lastIndexOf("/") + 1, str.length);
        }
        if (this.itemTitle) {
            this.itemName = slugify(this.itemTitle);
        }

        this.alerts = [];
    }

    createItem() {
        $http.post<{path}>("/createItem", angular.extend({
            item_path: this.itemLocation + "/" + this.itemName,
            item_type: this.itemType,
            item_title: this.itemTitle,
        }, this.params)).then((response) => {
            $window.location.href = "/view/" + response.data.path;
        }, (response) => {
            this.alerts = [];
            this.alerts.push({msg: response.data.error, type: "danger"});
        });
    }

    closeAlert(index) {
        this.alerts.splice(index, 1);
    }

    titleChanged() {
        if (!this.automaticShortName) {
            return;
        }
        this.itemName = slugify(this.itemTitle);
    }

    nameChanged() {
        this.automaticShortName = (this.itemName || []).length === 0;
    }
}

timApp.component("createItem", {
    bindings: {
        itemType: "@", // folder or document
        itemTitle: "@?",
        itemName: "@?",
        itemLocation: "@?",
        fullPath: "@?",
        params: "=?", // any additional parameters to be sent to server
    },
    controller: CreateItemController,
    templateUrl: "/static/templates/createItem.html",
});
