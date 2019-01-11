import {timApp} from "../app";

timApp.component("timMarkupError", {
    bindings: {
        data: "<",
    },
    template: `
<div class="pluginError">
    Plugin has invalid values for these markup fields:
    <ul>
        <li ng-repeat="e in ::$ctrl.data">
            {{ ::e.name }}: expected {{ ::e.type }}
        </li>
    </ul>
</div>
`,
});
