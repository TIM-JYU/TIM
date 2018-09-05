import {timApp} from "../app";
import {IController, IRootElementService, IScope} from "angular";


export class TapeController implements IController {
    private static $inject = ["$scope", "$element"];

    constructor(protected scope: IScope, protected element: IRootElementService) {
    }

    $onInit() {

    }

    public input: number[] = [];
    public hand: number | null = null;
    public output: number[] = [];
    public memory: number[] = []


// aaaa
}

timApp.component("tape", {
    controller: TapeController,
    template: `
    <div>
    <p>Liukuhihna!</p>
        <div>
            <span class="output">
                <span ng-repeat="n in $ctrl.output">{{n}}</span>
            </span>
            <span class="hand" ng-bind="$ctrl.hand">
            </span>
            <span class="input">
                <span ng-repeat="n in $ctrl.input">{{n}}</span>
            </span>
        </div>
        <div class="memory">
            <span ng-repeat="n in $ctrl.memory" ng-init="memindex = $index">
                <span ng-bind="{{memindex}}"></span>
                <span ng-bind="{{n}}"></span>
            </span>
        </div>
        <div class="allowed-commands">
        <!--- listalaatikko --->
        </div>
        <div class="program">
        <!--- listalaatikkaao --->
        </div>
        <div class="commandAddArea" ng-show="true">
             <input ng-model="$ctrl.newCommandParameter">
             <button class="timButton" ng-show="$ctrl.isSomeCellBeingEdited()"
                       ng-click="$ctrl.addCommand()"><span>Add command</span>
        </div>
        <div class="commandRemoveArea" ng-show="true">
            <button class="timButton"><span>Remove command</span></button>
        </div>
        <div>
             <button class="timButton"
                        ng-click="$ctrl.run()"><span>Run</span>
              <button class="timButton"
                        ng-click="$ctrl.step()"><span>Step</span>
        </div>
    </div>
    `
});