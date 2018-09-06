import {timApp} from "../app";
import {IController, IRootElementService, IScope} from "angular";

abstract class Command {
    protected constructor(name: string) {
        this.name = name;
    }

    protected function: ((params: CommandParameters) => void) | undefined;
    public name: string;

    public execute(params: CommandParameters) {
        if (this.function) {
            this.function(params);
        }
    }
}

// Commands

class CommandParameters {
    constructor(state: TapeState, mainParam: number) {
        this.state = state;
        this.mainParam = mainParam;
    }

    state: TapeState;
    mainParam: number;
}

class Add extends Command {
    constructor() {
         super("ADD");
         this.function = this.addFunc;
    }

    private addFunc(params: CommandParameters) {
        const n = params.mainParam;
        // TODO continue
    }
}

class CommandInstance {
    constructor(cmd: Command, param: number) {
        this.command = cmd;
        this.parameter = param;
    }

    public command: Command;
    public parameter: number;

    public getName() {
        return this.command.name + "(" + this.parameter + ")";
    }
}

class TapeState {
    public input: number[] = [];
    public hand: number | null = null;
    public output: number[] = [];
    public memory: number[] = [];
    public instructionPointer: number = 0;
}

export class TapeController implements IController {
    private static $inject = ["$scope", "$element"];

    constructor(protected scope: IScope, protected element: IRootElementService) {
        this.state = new TapeState();
    }

    $onInit() {

    }

    public possibleCommandList: Command[] = [];
    public commandList: CommandInstance[] = [];
    public state: TapeState;

}

timApp.component("tape", {
    controller: TapeController,
    template: `
    <div>
    <p>Liukuhihna!</p>
        <div>
            <span class="output">
                Output:
                <span ng-repeat="n in $ctrl.state.output">{{n}}</span>
            </span>
            Hand:
            <span class="hand" ng-bind="$ctrl.state.hand">
            </span>
            <span class="input">
                Input:
                <span ng-repeat="n in $ctrl.state.input">{{n}}</span>
            </span>
        </div>
        <div class="memory">
            Memory:
            <span ng-repeat="n in $ctrl.state.memory" ng-init="memindex = $index">
                <span ng-bind="{{memindex}}"></span>
                <span ng-bind="{{n}}"></span>
            </span>
        </div>
        <div>
        <span>Add command:</span>
        <span>Program</span>
        </div>
        <span class="allowed-commands">
            <select size="10">
            </select>
        </span>
        <span class="program">
            <select size="10">
            </select>
        </span>
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