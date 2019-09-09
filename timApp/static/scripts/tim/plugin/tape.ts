import {IController, IRootElementService, IScope} from "angular";
import {timApp} from "../app";
import {Binding} from "../util/utils";

export enum ParameterType {
    NUMBER,
    STRING,
}

/**
 * Generic base class for all tape machine commands.
 */
export abstract class Command {
    protected constructor(name: string, abbreviation: string) {
        this.name = name;
        this.abbreviation = abbreviation;
    }

    public name: string;
    public abbreviation: string;
    public usesParameter: boolean = true;

    public abstract execute(params: CommandParameters<unknown>): void;

    public getParameterName(): string {
        return "Parameter";
    }

    public toString(param: string | number | undefined): string {
        if ( this.usesParameter ) {
            return this.name + "(" + param + ")";
        }
        return this.name;
    }

    public isLabel(): boolean { return false; }

    public getParameterType(): ParameterType { return ParameterType.STRING; }
}

/**
 * The parameters given to a command when it is executed.
 */
export class CommandParameters<T> {
    constructor(state: TapeState, mainParam: T, commandList: CommandInstance[]) {
        this.state = state;
        this.mainParam = mainParam;
        this.commandList = commandList;
    }

    public state: TapeState;
    public mainParam: T;
    public commandList: CommandInstance[] = [];
}

// <editor-fold desc="Commands">

class Input extends Command {
    constructor() {
        super("INPUT", "i");
        this.usesParameter = false;
    }

    public execute(params: CommandParameters<unknown>) {
        if (params.state.input.length === 0) {
            params.state.stopped = true;
            return;
        }

        params.state.hand = params.state.input[0];
        params.state.input.splice(0, 1);
    }
}

class Output extends Command {
    constructor() {
        super("OUTPUT", "o");
        this.usesParameter = false;
    }

    public execute(params: CommandParameters<unknown>) {
        if (params.state.hand != null) {
            params.state.output.push(params.state.hand);
            while (params.state.output.length > 6 ) {
                params.state.output.splice(0, 1);
            }
            params.state.hand = undefined;
        }
    }
}

class MemoryCommand extends Command {
    constructor(name: string, abbr: string) {
        super(name, abbr);
    }

    public execute(params: CommandParameters<unknown>) { }

    public getParameterName() {
        return "Memory index";
    }

    public getParameterType(): ParameterType { return ParameterType.NUMBER; }
}

class Add extends MemoryCommand {
    constructor() {
         super("ADD", "a");
    }

    public execute(params: CommandParameters<number>) {
        const memoryIndex = params.mainParam;
        if (memoryIndex >= params.state.memory.length || params.state.hand == null) {
            return;
        }

        params.state.hand = params.state.hand + params.state.memory[memoryIndex];
    }
}

class Sub extends MemoryCommand {
    constructor() {
        super("SUB", "s");
    }

    public execute(params: CommandParameters<number>) {
        const memoryIndex = params.mainParam;
        if (memoryIndex >= params.state.memory.length || params.state.hand == null) {
            return;
        }

        params.state.hand = params.state.hand - params.state.memory[memoryIndex];
    }
}

class CopyTo extends MemoryCommand {
    constructor() {
        super("COPYTO", "t");
    }

    public execute(params: CommandParameters<number>) {
        if (params.state.hand != null) {
            params.state.memory[params.mainParam] = params.state.hand;
        }
    }
}

class CopyFrom extends MemoryCommand {
    constructor() {
        super("COPYFROM", "f");
    }

    public execute(params: CommandParameters<number>) {
        const memoryIndex = params.mainParam;
        if (memoryIndex < params.state.memory.length) {
            params.state.hand = params.state.memory[memoryIndex];
        }
    }
}

class DefineLabel extends Command {
    constructor() {
        super("Define Label", ".");
    }

    public execute(params: CommandParameters<unknown>) { }

    public getParameterName() { return "Label"; }

    public isLabel() { return true; }

    public toString(param: string): string {
        return param + ":";
    }
}

class Jump extends Command {
    constructor(name: string, abbr: string) {
        super(name, abbr);
    }

    public execute(params: CommandParameters<unknown>) {
        this.jumpToLabel(params);
    }

    protected jumpToLabel(params: CommandParameters<unknown>) {
        const index = params.commandList.findIndex((c) => c.command.isLabel() && c.parameter === params.mainParam);
        if (index > -1) {
            params.state.instructionPointer = index + 1;
        }
    }

    public getParameterName() { return "Label"; }
}

class JumpIfZero extends Jump {
    constructor() {
        super("JUMPIFZERO", "Z");
    }

    public execute(params: CommandParameters<unknown>) {
        if (params.state.hand !== null && params.state.hand === 0) {
            this.jumpToLabel(params);
        }
    }
}

class JumpIfNeg extends Jump {
    constructor() {
        super("JUMPIFNEG", "N");
    }

    public execute(params: CommandParameters<unknown>) {
        if (params.state.hand != null && params.state.hand < 0) {
            this.jumpToLabel(params);
        }
    }

    public getParameterName() { return "Label"; }
}

// </editor-fold>

/**
 * An instance of a command to be executed. Includes the command and its parameters.
 */
export class CommandInstance {
    constructor(cmd: Command, param: string | number | undefined) {
        this.command = cmd;
        this.parameter = param;
    }

    public command: Command;
    public parameter: string | number | undefined;

    public getName() {
        if (!this.command.usesParameter) {
            return this.command.name;
        }

        if (this.command.isLabel()) {
            return `${this.parameter}:`;
        }

        return `${this.command.name}(${this.parameter})`;
    }
}

/**
 * The state of the tape machine.
 */
export class TapeState {
    public input: number[] = [];
    public hand: number | undefined;
    public output: number[] = [];
    public memory: number[] = [];
    public instructionPointer: number = 0;
    public stopped: boolean = false;
}

/**
 * Attributes parsed from the paragraph YAML.
 */
export interface TapeAttrs {
    presetInput?: number[];
    presetHand?: number;
    presetOutput?: number[];
    presetMemoryState?: number[];
    presetCode?: string;
    hideCommands: boolean;
    hidePresetInput: boolean;
    hidePresetMem: boolean;
    hideTextMode: boolean;
    hideCopyAll: boolean;
}

function isiOS(): boolean {
      const iDevices = [
        "iPad Simulator",
        "iPhone Simulator",
        "iPod Simulator",
        "iPad",
        "iPhone",
        "iPod",
      ];

      if (!!navigator.platform) {
        while (iDevices.length) {
          if (navigator.platform === iDevices.pop()) { return true; }
        }
      }
      return false;
}

function scrollElementVisibleInParent(el: Element, par: Element, extraY: number) {
    // Scroll par window so that el comes visible if it is not visible
    // it is ensured that at least extraY times el hight is over or under el
    const rect = el.getBoundingClientRect();
    const prect = par.getBoundingClientRect();

    let dy = prect.top - rect.top;
    if ( dy > 0 ) { // el too high
        par.scrollTop -= dy + extraY * rect.height;
        return false;
    }

    dy = (prect.top + prect.height) - (rect.top + rect.height);
    if ( dy < 0 ) { // el too low
        par.scrollTop -= dy - extraY * rect.height;
        return false;
    }
    return true;
}

/**
 * The tape machine controller.
 */
export class TapeController implements IController {
    static $inject = ["$scope", "$element"]; // do not remove this even PyCharm says it is not used

    constructor(protected scope: IScope, protected element: IRootElementService) {
        this.state = new TapeState();
        this.possibleCommandList = [new Input(), new Output(), new Add(), new Sub(),
            new CopyTo(), new CopyFrom(), new DefineLabel(), new Jump("JUMP", "j"), new JumpIfZero(), new JumpIfNeg()];
    }

    $onInit() {
        this.iOS = isiOS();
        this.inputString = "";
        if ( this.data.presetInput ) {
            this.inputString = String(this.data.presetInput);
        }
        this.memString = "";
        if ( this.data.presetMemoryState ) {
            this.memString = String(this.data.presetMemoryState);
        }
        this.reset();
        this.selectedCommandIndex = this.commandList.length;
    }

    // List of commands supported by the tape machine
    public possibleCommandList: Command[] = [];

    // List of commands in the current program
    public commandList: CommandInstance[] = [];

    public state: TapeState;

    private newCommandIndex: number = -1;
    private newCommandParameter: string = "";
    private newCommandParameterText: string = "Parameter:";

    private showNewCommandParameter: boolean = false;

    // The index of the selected command of the current program, if any
    private selectedCommandIndex: number = -1;

    private data!: Binding<TapeAttrs, "<">;

    private timer?: NodeJS.Timer;

    private inputString: string = "";
    private memString: string = "";
    private programAsText: string = "";
    private textmode: boolean = false; // do not use as ng-model, because ipad changes it different time than PC
    private textmodeCB: boolean = false;
    private iOS: boolean = false;

    /**
     * Handles clicks on the "Insert command" button.
     */
    private insertCommandButtonClick() {
        if (this.newCommandIndex == -1) {
            return;
        }
        if (this.textmode) {
            const n = this.element.find(".textAreaRobotProgram").getSelection().start;
            let r = 0;
            const text = this.programAsText;
            for (let i = 0; i < n; i++) {
                if (text[i] === "\n") { r++; }
            }
            this.selectedCommandIndex = r;
        }

        const commandToAdd = this.possibleCommandList[this.newCommandIndex];
        this.addCommand(commandToAdd, this.newCommandParameter, this.selectedCommandIndex);
        if (this.selectedCommandIndex !== -1) {
            this.selectedCommandIndex++;
        }
        if (this.textmode) { this.textAll(); }

        this.checkCommandInView(this.selectedCommandIndex);

    }

    /**
     * Inserts a command into the program.
     * Returns true if succesful, otherwise false.
     * @param commandToAdd The command.
     * @param parameterString The parameter of the command given as a string.
     * @param index (Optional) The place where the command is inserted in the program, if not to the end.
     */
    private addCommand(commandToAdd: Command, parameterString: string, index: number = -1): boolean {
        let parameter;

        // TODO maybe move parameter validation to the commands themselves?
        if (commandToAdd.usesParameter) {
            if (commandToAdd.getParameterType() === ParameterType.NUMBER) {
                parameter = parseInt(parameterString, 10);
                // for now we assume all numbers are memory indexes
                if (isNaN(parameter) || parameter < 0 || parameter >= this.state.memory.length) {
                    return false;
                }
            } else {
                if (parameterString === "" ||
                    parameterString.includes(" ")) { // don't allow spaces to make parsing easier
                    // TODO show error?
                    return false;
                }
                parameter = parameterString;
            }
        }

        const commandInstance = new CommandInstance(commandToAdd, parameter);
        if (index === -1) {
            this.commandList.push(commandInstance);
        } else {
            this.commandList.splice(index, 0, commandInstance);
        }
        return true;
    }

    private checkCommandInView(n: number) {
        if (this.commandList.length > 10) { // long program, ensure command is visible
            if ( n >= this.commandList.length ) { n = this.commandList.length - 1; }
            if ( n < 0 ) { return; }
            const cmdul = this.element.find(".cmditems");
            const cmdli = cmdul.children()[n];
            scrollElementVisibleInParent(cmdli, cmdul[0], 1);
        }
    }

    private checkCurrentCommandInView() {
        this.checkCommandInView(this.state.instructionPointer);
    }

    /**
     * Steps the program.
     */
    private step() {
        this.changeList();
        if (this.state.instructionPointer >= this.commandList.length || this.state.stopped) {
            if (this.timer) {
                this.stop();
                this.scope.$apply();
            }
            return;
        }

        const command = this.commandList[this.state.instructionPointer];
        this.state.instructionPointer++;
        command.command.execute(new CommandParameters(this.state, command.parameter, this.commandList));

        this.checkCurrentCommandInView();
    }

    private automaticStep() {
        this.step();
        this.scope.$apply();
    }

    /**
     * Handles clicks on the Run / Stop button.
     */
    private run() {
        if (this.state.instructionPointer >= this.commandList.length || this.state.stopped) {
            return;
        }

        if (this.timer) {
            this.stop();
        } else {
            this.timer = setInterval(() => this.automaticStep(), 500);
        }
    }

    private static arrayFromString(s?: string): number[] {
        if ( !s ) { return []; }
        s = s.trim();
        if ( s === "" ) { return []; }

        const result = [];
        const pieces = s.split(",");
        for (const n of pieces) {
            let val = Number(n.trim());
            if ( !val ) { val = 0; }
            result.push(val);
        }
        return result;
    }

    /**
     * Resets the tape machine to its original state.
     * Clears output, sets input to match the original input,
     * sets the instruction pointer to zero and clears memory.
     */
    private reset() {
        if (this.timer) {
            stop();
        }

        this.state.stopped = false;

        this.state.input = TapeController.arrayFromString(this.inputString);  // Array.from(this.data.presetInput);

        if (this.data.presetHand) {
            this.state.hand = this.data.presetHand;
        } else {
            this.state.hand = undefined;
        }

        if (this.data.presetOutput) {
            this.state.output = Array.from(this.data.presetOutput);
        } else {
            this.state.output = [];
        }

        this.state.memory = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

        const memState = TapeController.arrayFromString(this.memString);  // Array.from(this.data.presetInput);

        for (let i = 0; i < memState.length && i < this.state.memory.length; i++) {
            this.state.memory[i] = memState[i];
        }

        this.state.instructionPointer = 0;
        if (this.data.presetCode && this.commandList.length === 0) {
            // assign pre-defined code only if we have no code right now,
            // so we don't clear the user's code on reset
            this.fromText(this.data.presetCode);
            this.selectedCommandIndex = this.commandList.length;
        }

        this.checkCurrentCommandInView();
    }

    /**
     * Stops the program if it's running.
     */
    private stop() {
        if (this.timer) {
            clearInterval(this.timer);
            this.timer = undefined;
        }
    }

    /**
     * Handles clicks on the "Remove command" button.
     */
    private removeCommand() {
        if (this.selectedCommandIndex > -1 && this.selectedCommandIndex < this.commandList.length) {
            this.commandList.splice(this.selectedCommandIndex, 1);
        }
        // this.textAll();
    }

    private textAll() {
        this.programAsText = this.toText();
    }

    private selectAllText(name: string, newtext: string) {
        const jh = this.element.find(name);
        if ( newtext ) { jh.val(newtext); }
        const h = jh[0] as HTMLTextAreaElement;
        if ( !h ) { return; }
        if ( this.iOS ) {
            h.focus();
            h.setSelectionRange(0, 99999);  // select is not working in iOS
        } else {
            jh.select();
        }

    }

    private copyAll() {
        // this.changeText();
        if ( this.textmode ) {
            this.selectAllText(".textAreaRobotProgram", "");
        } else {
            this.selectAllText(".hiddenRobotProgram", this.toText());
        }
        document.execCommand("copy");
    }

    private paste() {
        if ( this.programAsText === "" ) { return; }
        this.commandList = [];
        this.fromText(this.programAsText);
    }

    private changeList() {
        if ( !this.textmode ) { return; }
        this.textmode = false;
        this.textmodeCB = false;
        this.paste();
    }

    private changeText() {
        if ( this.textmode ) { return; }
        this.textmode = true;
        this.textmodeCB = true;
        this.textAll();
    }

    private changeMode() {
        if ( this.textmode ) { this.changeList(); } else { this.changeText(); }
    }

    /**
     * Converts the current program into a textual representation.
     */
    private toText(): string {
        let result: string = "";
        // this.commandList.forEach(c => result += `${c.command.name} ${c.parameter}\n`);
        for (const c of this.commandList) {
             result += c.command.toString(c.parameter) + "\n";
        }
        return result;
    }

    /**
     * Parses a program from a textual representation.
     * @param text The textual representation of a program.
     */
    private fromText(text: string) {
        text = text.replace("\r\n", "\n").replace("\r", "\n");
        const lines = text.split("\n");
        for (let line of lines) {
            line = line.replace(";", "").trim();
            if (line.length == 0) {
                continue;
            }

            const colonIndex = line.indexOf(":");
            if (colonIndex == line.length - 1 && line.length > 1) {
                // label found
                const defineLabelCommand = this.possibleCommandList.find((c) => c.name === "Define Label");
                if (defineLabelCommand) {
                    this.addCommand(defineLabelCommand, line.slice(0, line.length - 1));
                }

                continue;
            }

            let components: string[] = [];
            const pIndex = line.indexOf("(");
            let separator = "(";

            if (pIndex === -1) {
                // Check for "Python syntax"
                const spaceIndex = line.indexOf(" ");
                if (spaceIndex > -1) {
                    separator = " ";
                }
            } else {
                // C syntax

                const closingIndex = line.indexOf(")");
                // We wouldn't need to check the following for parsing,
                // but check it anyway to force correct syntax
                if (closingIndex < pIndex) {
                    // Syntax error
                    continue;
                }

                line = line.replace(")", "");
                separator = "(";
            }

            components = line.split(separator);
            if (components.length === 0) {
                continue;
            }

            const name = components[0].trim().toUpperCase();
            const command = this.possibleCommandList.find((c) => c.name === name);
            if (!command) {
                continue;
            }

            if (components.length === 1) {
                this.addCommand(command, "");
            } else {
                this.addCommand(command, components[1].trim());
            }
        }
    }

    /**
     * Handles clicks on the "add command" command list.
     * @param index The index of the command that was clicked.
     */
    private onCommandClick(index: number) {
        this.newCommandIndex = index;

        if (this.newCommandIndex > -1) {
            const command = this.possibleCommandList[this.newCommandIndex];
            if (command) {
                this.showNewCommandParameter = command.usesParameter;
                this.newCommandParameterText = command.getParameterName() + ":";
            }
        }
    }

    // Rendering functions below

    /**
     * Gets the color of an item in the "add command" supported command list.
     * @param index The index of the command.
     */
    private getNewCommandColor(index: number) {
        if (index === this.newCommandIndex) {
            return "red";
        }

        return "black";
    }

    /**
     * Gets the color of an item in the current program command list.
     * @param index The index of the program command.
     */
    private getCommandColor(index: number) {
        if (index == this.selectedCommandIndex) {
            return "red";
        }

        if (index > -1 && index < this.commandList.length) {
            if (this.commandList[index].command.isLabel()) {
                return "blue";
            }
        }

        return "black";
    }

    private getCommandBackgroundColor(index: number) {
        if (index == this.state.instructionPointer) {
            return "yellow";
        }

        return "white";
    }

    private getRunButtonText() {
        if (this.timer) {
            return "Stop";
        }

        return "Run";
    }

    private getHand() {
        if (this.state.hand != null) {
            return this.state.hand;
        } else {
            return " ";
        }
    }

    /**
     * Gets the text for a specific index in the memory index (not memory content!) display.
     * @param index The memory location index.
     */
    private getMemoryText(index: number) {
        return /* "#" + */ String(index);
    }
}

timApp.component("timTape", {
    controller: TapeController,
    bindings: {
        data: "<",
    },
    template: `
<style>

tim-tape .robotMainDiv {
    text-align: left;

}

tim-tape .outputvalues, tim-tape .inputvalues, tim-tape .handItem {
    height: 40px;
}
tim-tape .outputbelt, tim-tape .inputbelt {
    width: 150px;
    overflow: hidden;
}
tim-tape .outputvalues {
    width: 150px;
    overflow: hidden;
    text-align: right;
}
tim-tape .inputvalues {
    width: 150px;
    overflow: hidden;
    text-align: left;
}

tim-tape .robotRunButtons {
    display: table;
    position: relative;
    left: 180px;
    top: -70px;
}
tim-tape .inputbelt, tim-tape .outputbelt, tim-tape .robotDiv {
    display: inline-block;
    text-align: center;
    vertical-align: top;
}
tim-tape .robotDiv {
}
tim-tape .tapeAndRobotDiv {
    display: inline-table;
    width: 700px;
}
tim-tape .memoryArea {
    width: 700px;
}
tim-tape .memoryValue {
    display: inline-table;
    width: 30px;
}
tim-tape .programArea {
    display: flex;
    width: 700px;
}
tim-tape .commandAddArea .timButton, tim-tape .commandAddArea input {
    display: grid;
    width: 100px;
}
tim-tape .commandButtons {
}
tim-tape .commandAddArea {
    margin-top: 30px;
    width: 120px;
}
tim-tape .commandParamArea {
    height: 80px;
}
tim-tape .presetInput {
    text-align: left;
    width: 200px;
}
tim-tape .robotEditDiv {

}
tim-tape .robotEditArea {
   height: calc(100% - 40px);
   width: 150px;
   white-space: pre;
   overflow-wrap: normal;
   overflow-x: scroll;
}

tim-tape .robotPresets {
    margin-top: 30px;
    display: inline-block;
    vertical-align: top;
    margin-left: 2em;
}

tim-tape .commandListContainer {
   width: 150px;
}

</style>
    <div class="no-highlight robotMainDiv">
        <div class="tapeAndRobotDiv">
            <div class="outputbelt" >
                <div class="outputvalues">
                    <span class="output">
                    <div ng-repeat="n in $ctrl.state.output track by $index" class="tapeItem">{{n}}</div>
                    </span>
               </div>
                <img src="/static/images/tape/output.png" />
                <span>Output</span>
            </div>
            <div class="robotDiv" >
                <div class="handItem">
                    <div class="tapeItem"  ng-bind="$ctrl.getHand()"></div>
                </div>
                <div class="robotImage">
                    <img src="/static/images/tape/robot.png" />
                    <div class="robotRunButtons">
                         <button class="timButton" ng-click="$ctrl.step()"><span>Step</span>
                         <button class="timButton" ng-click="$ctrl.run()"><span ng-bind="$ctrl.getRunButtonText()"></span>
                         <button class="timButton" ng-click="$ctrl.reset()"><span>Reset</span>
                    </div>
                </div>
            </div>
            <div class="inputbelt">
                <div class="inputvalues">
                    <span class="input">
                        <div ng-repeat="n in $ctrl.state.input track by $index" class="tapeItem">{{n}}</div>
                    </span>
                </div>
                <img src="/static/images/tape/input.png" />
                <span>Input</span>
            </div>
        </div>
        <div class="memoryArea">
            <div>Memory:</div>
            <div ng-repeat="n in $ctrl.state.memory track by $index" class="memoryContainer">
                <div ng-bind="n" class="memoryValue"></div>
                <div ng-bind="$ctrl.getMemoryText($index)" class="memoryIndex"></div>
            </div>
        </div>
        <div class="programArea">
            <span class="commandListContainer newCommandList" ng-hide="$ctrl.data.hideCommands">
                Commands:
                <ul class="list-unstyled listBox">
                <li ng-repeat="c in $ctrl.possibleCommandList" class="command" ng-style="{'color': $ctrl.getNewCommandColor($index)}"
                    ng-click="$ctrl.onCommandClick($index)">{{c.name}}</li>
                </ul>
            </span>
            <div class="commandAddArea" ng-hide="$ctrl.data.hideCommands" >
                 <div class="commandParamArea">
                     <span class="commandParameterArea" ng-show="$ctrl.showNewCommandParameter">
                        <span>{{$ctrl.newCommandParameterText}}</span>
                        <input size="5em" ng-model="$ctrl.newCommandParameter">
                     </span>
                 </div>
                 <div class="commandButtons">
                     <button class="timButton" ng-click="$ctrl.insertCommandButtonClick()"><span>Insert -&gt;</span>
                     <button class="timButton" ng-click="$ctrl.removeCommand()"><span>&lt;- Remove</span></button>
                     <button class="timButton" ng-click="$ctrl.copyAll()" ng-hide="$ctrl.data.hideCopyAll"><span>Copy all</span></button>
                     <!-- <button class="timButton" ng-click="$ctrl.paste()"><span>Paste</span></button> -->
                 </div>
            </div>
            <span class="commandListContainer">
                Program:
                <textarea class= "robotEditArea textAreaRobotProgram" ng-model="$ctrl.programAsText" ng-hide="!$ctrl.textmode"></textarea>
                <ul class="cmditems list-unstyled listBox programCommandList" ng-hide="$ctrl.textmode">
                <li  ng-repeat="c in $ctrl.commandList" class="command" ng-click="$ctrl.selectedCommandIndex = $index"
                ng-style="{'color': $ctrl.getCommandColor($index), 'background-color': $ctrl.getCommandBackgroundColor($index)}">{{c.getName()}}</li>
                <li class="command" ng-style="{'color': $ctrl.getCommandColor($ctrl.commandList.length + 1)}"
                    ng-click="$ctrl.selectedCommandIndex = ($ctrl.commandList.length + 1)">-</li>
                </ul>
                <!--- <select ng-model="$ctrl.selected" size="10">
                <option ng-repeat="c in $ctrl.commandList" ng-style="{'color': $ctrl.getCommandColor($index)}">{{c.getName()}}</option>
                </select> --->
            </span>
            <div class="robotPresets">
                <div class="presetInput" ng-hide="$ctrl.data.hidePresetInput" >
                     <div class="commandParamArea">
                         <span>
                            <span>Preset input:</span>
                            <input size="15em" ng-model="$ctrl.inputString">
                         </span>
                     </div>
                </div>
                <div class="presetInput" ng-hide="$ctrl.data.hidePresetMem" >
                     <div class="commandParamArea">
                         <span>
                            <span>Preset memory:</span>
                            <input size="15em" ng-model="$ctrl.memString">
                         </span>
                     </div>
                </div>
                <span ng-hide="$ctrl.data.hideTextMode"><input type="checkbox" ng-model="$ctrl.textmodeCB" class="belttextbox" name="textmode" value="textmode"  ng-click="$ctrl.changeMode()" /><label for="belttextbox">&nbsp;Text&nbsp;mode</label></div></span>
            </div>
        </div>
        <textarea style= "height: 1px; width: 1px; position: unset;" class="hiddenRobotProgram"></textarea>
    </div>
    `,
});
