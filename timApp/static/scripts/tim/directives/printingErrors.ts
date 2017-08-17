import {timApp} from "../app";

timApp.component("printingErrors", {
    bindings: {
        errorMsg: "<",
    },
    template: `<div><h4>An error occurred during printing!</h4><p>{{$ctrl.errorMsg}}</p></div>`,
});
