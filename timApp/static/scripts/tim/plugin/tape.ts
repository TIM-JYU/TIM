import {timApp} from "../app";
import {IController, IRootElementService, IScope} from "angular";
import {Binding} from "../util/utils";

enum ParameterType {
    NUMBER,
    STRING
}

/**
 * Generic base class for all tape machine commands.
 */
abstract class Command {
    protected constructor(name: string, abbreviation: string) {
        this.name = name;
        this.abbreviation = abbreviation;
    }

    public name: string;
    public abbreviation: string;
    public usesParameter: boolean = true;

    public abstract execute(params: CommandParameters): void;

    public getParameterName(): string {
        return "Parameter";
    }

    public isLabel(): boolean { return false; }

    public getParameterType() : ParameterType { return ParameterType.STRING; }
}

/**
 * The parameters given to a command when it is executed.
 */
class CommandParameters {
    constructor(state: TapeState, mainParam: any, commandList: CommandInstance[]) {
        this.state = state;
        this.mainParam = mainParam;
        this.commandList = commandList;
    }

    public state: TapeState;
    public mainParam: any;
    public commandList: CommandInstance[] = [];
}

//<editor-fold desc="Commands">

class Input extends Command {
    constructor() {
        super("INPUT", "i");
        this.usesParameter = false;
    }

    public execute(params: CommandParameters) {
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

    public execute(params: CommandParameters) {
        if (params.state.hand != null) {
            params.state.output.push(params.state.hand);
            params.state.hand = undefined;
        }
    }
}

class MemoryCommand extends Command {
    constructor(name: string, abbr: string) {
        super(name, abbr);
    }

    public execute(params: CommandParameters) { }

    public getParameterName() {
        return "Memory index";
    }

    public getParameterType() : ParameterType { return ParameterType.NUMBER; }
}

class Add extends MemoryCommand {
    constructor() {
         super("ADD", "a");
    }

    public execute(params: CommandParameters) {
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

    public execute(params: CommandParameters) {
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

    public execute(params: CommandParameters) {
        if (params.state.hand) {
            params.state.memory[params.mainParam] = params.state.hand;
        }
    }
}

class CopyFrom extends MemoryCommand {
    constructor() {
        super("COPYFROM", "f");
    }

    public execute(params: CommandParameters) {
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

    public execute(params: CommandParameters) { }

    public getParameterName() { return "Label"; }

    public isLabel() { return true; }
}

class Jump extends Command {
    constructor(name: string, abbr: string) {
        super(name, abbr);
    }

    public execute(params: CommandParameters) {
        this.jumpToLabel(params);
    }

    protected jumpToLabel(params: CommandParameters) {
        const index = params.commandList.findIndex(c => c.command.isLabel() && c.parameter === params.mainParam);
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

    public execute(params: CommandParameters) {
        if (params.state.hand !== null && params.state.hand === 0) {
            this.jumpToLabel(params);
        }
    }
}

class JumpIfNeg extends Jump {
    constructor() {
        super("JUMPIFNEG", "N");
    }

    public execute(params: CommandParameters) {
        if (params.state.hand != null && params.state.hand < 0) {
            this.jumpToLabel(params);
        }
    }

    public getParameterName() { return "Label"; }
}

//</editor-fold>

/**
 * An instance of a command to be executed. Includes the command and its parameters.
 */
class CommandInstance {
    constructor(cmd: Command, param: any) {
        this.command = cmd;
        this.parameter = param;
    }

    public command: Command;
    public parameter: any;

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
class TapeState {
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
}

/**
 * The tape machine controller.
 */
export class TapeController implements IController {
    private static $inject = ["$scope", "$element"];

    constructor(protected scope: IScope, protected element: IRootElementService) {
        this.state = new TapeState();
        this.possibleCommandList = [new Input(), new Output(), new Add(), new Sub(),
            new CopyTo(), new CopyFrom(), new DefineLabel(), new Jump("JUMP", "j"), new JumpIfZero(), new JumpIfNeg()];
    }

    $onInit() {
        this.step = this.step.bind(this);
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

    private showNewCommandParameter: boolean = true;

    // The index of the selected command of the current program, if any
    private selectedCommandIndex: number = -1;

    private data!: Binding<TapeAttrs, "<">;

    private timer: any;


    /**
     * Handles clicks on the "Insert command" button.
     */
    private insertCommandButtonClick() {
        if (this.newCommandIndex == -1) {
            return;
        }

        const commandToAdd = this.possibleCommandList[this.newCommandIndex];
        this.addCommand(commandToAdd, this.newCommandParameter, this.selectedCommandIndex);
        if (this.selectedCommandIndex !== -1)
            this.selectedCommandIndex++;
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
                parameter = parseInt(parameterString);
                // for now we assume all numbers are memory indexes
                if (isNaN(parameter) || parameter < 0 || parameter >= this.state.memory.length) {
                    return false;
                }
            } else {
                if (parameterString === "" ||
                    parameterString.includes(' ')) { // don't allow spaces to make parsing easier
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

    /**
     * Steps the program.
     */
    private step() {
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
            this.timer = setInterval(this.automaticStep(), 500);
        }
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

        if (this.data.presetInput) {
            this.state.input = Array.from(this.data.presetInput);
        } else {
            this.state.input = [];
        }

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
        if (this.data.presetMemoryState) {
            for (let i = 0; i < this.data.presetMemoryState.length && this.state.memory.length; i++) {
                this.state.memory[i] = this.data.presetMemoryState[i];
            }
        }

        this.state.instructionPointer = 0;
        if (this.data.presetCode && this.commandList.length === 0) {
            // assign pre-defined code only if we have no code right now,
            // so we don't clear the user's code on reset
            this.fromText(this.data.presetCode);
            this.selectedCommandIndex = this.commandList.length;
        }
    }

    /**
     * Stops the program if it's running.
     */
    private stop() {
        if (this.timer) {
            clearInterval(this.timer);
            this.timer = null;
        }
    }

    /**
     * Handles clicks on the "Remove command" button.
     */
    private removeCommand() {
        if (this.selectedCommandIndex > -1 && this.selectedCommandIndex < this.commandList.length) {
            this.commandList.splice(this.selectedCommandIndex, 1);
        }
    }

    /**
     * Converts the current program into a textual representation.
     */
    private toText(): string {
        let result: string = "";
        this.commandList.forEach(c => result += `${c.command.name} ${c.parameter}\n`);
        return result;
    }

    /**
     * Parses a program from a textual representation.
     * @param text The textual representation of a program.
     */
    private fromText(text: string) {
        text = text.replace("\r\n", "\n").replace("\r", "\n");
        const lines = text.split('\n');
        for (let line of lines) {
            line = line.replace(';', '').trim();
            let components: string[] = [];
            const pIndex = line.indexOf('(');
            let separator = '(';

            if (pIndex === -1) {
                // Check for "Python syntax"
                const spaceIndex = line.indexOf(' ');
                if (spaceIndex > -1) {
                    separator = ' ';
                }
            } else {
                // C syntax

                const closingIndex = line.indexOf(')');
                // We wouldn't need to check the following for parsing,
                // but check it anyway to force correct syntax
                if (closingIndex < pIndex) {
                    // Syntax error
                    continue;
                }

                line = line.replace(')', '');
                separator = '(';
            }

            components = line.split(separator);
            if (components.length === 0) {
                continue;
            }

            const command = this.possibleCommandList.find(c => c.name === components[0].trim());
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
            if (this.commandList[index].command.isLabel())
                return "blue";
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
        if (this.state.hand) {
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
        return "#" + String(index);
    }
}

timApp.component("timTape", {
    controller: TapeController,
    bindings: {
        data: "<",
    },
    template: `
    <div class="no-highlight">
        <div>
            <div ng-style="{'display': 'inline-block', 'text-align': 'right'}">
                <span class="output">
                <div ng-repeat="n in $ctrl.state.output track by $index" ng-style="{'border': '1px solid black', 
                    'display': 'inline-block', 'margin': '0.2em', 'padding': '0.2em'}">{{n}}</div>
                </span>
                <img src="/static/images/tape/output.png" />
                <span>Output</span>
            </div>
            <div ng-style="{'display': 'inline-block', 'text-align': 'center'}">
                <div ng-style="{'border': '1px solid black', 
                    'display': 'inline-block', 'margin': 'auto', 'padding': '0.2em'}"
                     class="hand" ng-bind="$ctrl.getHand()"></div>
                <img src="/static/images/tape/robot.png" />
            </div>
            <div ng-style="{'display': 'inline-block'}">
                <span class="input">
                    <div ng-repeat="n in $ctrl.state.input track by $index" ng-style="{'border': '1px solid black', 
                    'display': 'inline-block', 'margin': '0.2em', 'padding': '0.2em'}">{{n}}</div>
                </span>
                <img src="/static/images/tape/input.png" />
                <span>Input</span>
            </div>
        </div>
        <div class="memory">
            <div>Memory:</div>
            <div ng-repeat="n in $ctrl.state.memory track by $index" ng-style="{'display': 'inline-block', 'text-align': 'center', 'padding': '0.2em', 'margin': '0.1em', 'margin-bottom': '1em'}">
                <div ng-bind="n" ng-style="{'border': '1px solid black'}"></div>
                <div ng-bind="$ctrl.getMemoryText($index)" ng-style="{'font-size': '0.8em'}"></div>
            </div>
        </div>
        <span class="allowed-commands" ng-style="{'display': 'inline-block', 'vertical-align': 'top', 'margin-right': '2em'}">
            Add command:
            <ul class="list-unstyled" ng-style="{'border': '1px solid black'}">
            <li ng-repeat="c in $ctrl.possibleCommandList" ng-style="{'color': $ctrl.getNewCommandColor($index), 'cursor': 'pointer'}" 
                ng-click="$ctrl.onCommandClick($index)">{{c.name}}</li>
            </ul>
        </span>
        <span class="program" ng-style="{'display': 'inline-block', 'vertical-align': 'top'}">
            Program:
            <ul class="list-unstyled" ng-style="{'border': '1px solid black'}">
            <li ng-repeat="c in $ctrl.commandList" ng-click="$ctrl.selectedCommandIndex = $index" 
            ng-style="{'color': $ctrl.getCommandColor($index), 'background-color': $ctrl.getCommandBackgroundColor($index),
            'cursor': 'pointer'}">{{c.getName()}}</li>
            <li ng-style="{'color': $ctrl.getCommandColor($ctrl.commandList.length + 1), 'cursor': 'pointer'}" 
                ng-click="$ctrl.selectedCommandIndex = ($ctrl.commandList.length + 1)">-</li>
            </ul>
            <!--- <select ng-model="$ctrl.selected" size="10">
            <option ng-repeat="c in $ctrl.commandList" ng-style="{'color': $ctrl.getCommandColor($index)}">{{c.getName()}}</option>
            </select> --->
        </span>
        <div class="commandAddArea" ng-show="true" ng-style="{'margin-bottom': '0.5em'}">
             <span class="commandParameterArea" ng-show="$ctrl.showNewCommandParameter">
                <span>{{$ctrl.newCommandParameterText}}</span>
                <input ng-model="$ctrl.newCommandParameter">
             </span>
             <button class="timButton" ng-click="$ctrl.insertCommandButtonClick()"><span>Insert command</span>
        </div>
        <div class="commandRemoveArea" ng-show="true" ng-style="{'margin-bottom': '1em'}">
            <button class="timButton" ng-click="$ctrl.removeCommand()"><span>Remove command</span></button>
        </div>
        <div>
             <button class="timButton" ng-click="$ctrl.run()"><span ng-bind="$ctrl.getRunButtonText()"></span>
             <button class="timButton" ng-click="$ctrl.step()"><span>Step</span>
             <button class="timButton" ng-click="$ctrl.reset()"><span>Reset</span>
        </div>
    </div>
    `
});