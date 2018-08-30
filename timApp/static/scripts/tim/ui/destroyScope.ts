import {IRootElementService, IScope} from "angular";

export abstract class DestroyScope {
    constructor(scope: IScope, element: IRootElementService) {
        element.on("$destroy", () => {
            this.$onDestroy();
            scope.$destroy();
        });
    }

    abstract $onDestroy(): void;
}
