import angular, {IController, IFormController, IHttpPromise, IPromise, IRootElementService, IScope} from "angular";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../ui/dialog";
import {ILectureFormParams} from "../lecture/lecturetypes";
import {IEditorParams, IEditorResult, PareditorController} from "../editor/pareditor";

export interface TimTableToolbarCallbacks {
    setTextAlign: (value: string) => void,
    setCellBackgroundColor: (value: string) => void,
}

export interface timTableEditorToolbarParams {
    callbacks: TimTableToolbarCallbacks;
    activeTable: object;
}

let instance: TimTableEditorToolbarController | null = null;

export class TimTableEditorToolbarController extends DialogController<{params: timTableEditorToolbarParams},
    { }, "timTableEditorToolbar" > {
    private static $inject = ["$scope", "$element"];

    readonly DEFAULT_CELL_BGCOLOR = "#EEEEEE";

    constructor(protected scope: IScope, protected element: IRootElementService) {
        super(element, scope);
        instance = this;
        this.callbacks = this.resolve.params.callbacks;
        this.activeTable = this.resolve.params.activeTable;
    }

    $onInit() {
        super.$onInit();
        this.draggable.setCloseFn(undefined); // Hides the close button
    }

    /**
     * Checks for changes in the cell background color selector.
     */
    $doCheck() {
        if (this.cellBackgroundColor !== this.previousBackgroundColor) {
            this.previousBackgroundColor = this.cellBackgroundColor;
            this.callbacks.setCellBackgroundColor(this.cellBackgroundColor);
        }
    }

    public callbacks: TimTableToolbarCallbacks;
    private activeTable?: object;
    private visible: boolean = true;

    private previousBackgroundColor: string = this.DEFAULT_CELL_BGCOLOR;
    private cellBackgroundColor: string = this.DEFAULT_CELL_BGCOLOR;

    public getTitle() {
        return "Edit table";
    }

    dismiss() {
        this.hide();
    }

    public hide() {
        this.close("");
        this.visible = false;
        this.scope.$apply();
        instance = null;
    }

    public hideIfActiveTable(table: object) {
        if (table == this.activeTable) {
            this.hide();
        }
    }

    public show(callbacks: TimTableToolbarCallbacks, activeTable: object) {
        this.visible = true;
        this.activeTable = activeTable;
        this.callbacks = callbacks;
    }

    private setTextAlign(value: string) {
        this.callbacks.setTextAlign(value);
    }
}

// : IPromise< { } >
export function openTableEditorToolbar(p: timTableEditorToolbarParams) {
    if (instance) {
        instance.show(p.callbacks, p.activeTable);
    } else {
        showDialog<TimTableEditorToolbarController>(
        "timTableEditorToolbar",
        {params: () => p},
        {forceMaximized: false});
    }
}

export function hideToolbar(closingTable: object) {
    if (instance) {
        instance.hideIfActiveTable(closingTable);
    }
}

registerDialogComponent("timTableEditorToolbar",
    TimTableEditorToolbarController,
    {
        template: `
  <div >
    <div class="timTableEditorToolbar">
        <input type="color" title="Change cell background color" class="colorchange-button"
               ng-model="$ctrl.cellBackgroundColor"/>
        <button class="glyphicon glyphicon-align-left" title="Align left" ng-click="$ctrl.setTextAlign('left')"></button>
        <button class="glyphicon glyphicon-align-center" title="Align center" ng-click="$ctrl.setTextAlign('center')"></button>
        <button class="glyphicon glyphicon-align-right" title="Align right" ng-click="$ctrl.setTextAlign('right')"></button>
        <!--- <button class="editorButton" title="Align left" ng-click="$ctrl.alignLeft()"><span
                class="glyphicon glyphicon-align-left"></span></button>
        <button class="editorButton" title="Align center" ng-click="$ctrl.alignCenter()"><span
                class="glyphicon glyphicon-align-center"></span></button>
        <button class="editorButton" title="Align right" ng-click="$ctrl.alignRight()"><span
                class="glyphicon glyphicon-align-right"></span></button> --->
    </div>
  </div>

<!--- <tim-dialog>
    <dialog-header>aaa
        Lecture ends in
        <timer interval="1000"
               max-time-unit="'day'"
               end-time="$ctrl.resolve.lecture.end_time">
            {{ days > 0 ? days + ' day' + daysS + ' +' : '' }} {{ hhours }}:{{ mminutes }}:{{ sseconds }}
        </timer>
    </dialog-header>
    <dialog-body>
        <form>
            <label> Extend by
                <select ng-model="$ctrl.selectedTime" ng-options="choice for choice in $ctrl.extendTimes">
                </select>
                minutes
            </label>
        </form>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" autofocus ng-click="$ctrl.extend()">Extend</button>
        <button class="timButton" ng-show="!$ctrl.hasLectureEnded()" ng-click="$ctrl.end()">End</button>
        <button
                class="timButton"
                ng-show="$ctrl.hasLectureEnded()"
                ng-click="$ctrl.noExtend()">Don't extend
        </button>
    </dialog-footer>
</tim-dialog> --->
`,
    });