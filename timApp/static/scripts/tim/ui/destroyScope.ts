import {IScope} from "angular";

export abstract class DestroyScope {
    constructor(scope: IScope, element: JQLite) {
        element.on("$destroy", () => {
            this.$onDestroy();
            scope.$destroy();
        });
    }

    abstract $onDestroy(): void;
}
