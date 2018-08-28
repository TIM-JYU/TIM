import {IRootElementService, IScope} from "angular";

export class DestroyScope {
    constructor(scope: IScope, element: IRootElementService) {
        element.on("$destroy", () => {
            scope.$destroy();
        });
    }
}
