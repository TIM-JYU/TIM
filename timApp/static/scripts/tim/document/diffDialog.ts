import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {Pos} from "../ui/draggable";
import {$injector} from "../util/ngimport";
import {fixDefExport} from "../util/utils";

export interface IDiffParams {
    left: string;
    right: string;
    title: string;
    pos?: Pos;
}

export class DiffController extends DialogController<{params: IDiffParams}, {}> {
    static component = "timDiff";
    static $inject = ["$element", "$scope"] as const;
    private options = {editCost: 4};

    async $onInit() {
        super.$onInit();
        await this.draggable.makeHeightAutomatic();
        if (this.resolve.params.pos) {
            this.moveTo(this.resolve.params.pos);
        }
    }

    close() {
        super.close({});
    }

    protected getTitle(): string {
        return this.resolve.params.title;
    }

    left() {
        return this.resolve.params.left;
    }

    right() {
        return this.resolve.params.right;
    }
}

registerDialogComponent(DiffController,
    {
        template: `
<tim-dialog>
    <dialog-body>
        <div class="diff border">
            <pre processing-diff options="$ctrl.options" left-obj="$ctrl.left()" right-obj="$ctrl.right()"></pre>
        </div>
    </dialog-body>
</tim-dialog>
    `,
    },
);

export async function showDiffDialog(p: IDiffParams) {
    const module = fixDefExport(await import("angular-diff-match-patch"));
    $injector.loadNewModules([module]);
    return showDialog(DiffController, {params: () => p},
        {
            absolute: false,
            showMinimizeButton: false,
            size: "xs",
        });
}
