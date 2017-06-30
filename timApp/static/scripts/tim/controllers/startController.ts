import {timApp} from "tim/app";
import * as createItem from "tim/directives/createItem";
import {markAsUsed} from "tim/utils";

markAsUsed(createItem);

export class StartCtrl {
    private creatingNew: boolean;

    constructor() {
        this.creatingNew = false;
    }

    cancelCreate() {
        this.creatingNew = false;
    }

    enableCreate() {
        this.creatingNew = true;
    }
}

timApp.controller("StartCtrl", StartCtrl);
