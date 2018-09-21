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
        if (params.state.hand !== null) {
            params.state.output.push(params.state.hand);
            params.state.hand = null;
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
        if (params.state.hand !== null && params.state.hand < 0) {
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
            return this.parameter + ":";
        }

        return this.command.name + "(" + this.parameter + ")";
    }
}

/**
 * The state of the tape machine.
 */
class TapeState {
    public input: number[] = [];
    public hand: number | null = null;
    public output: number[] = [];
    public memory: number[] = [];
    public instructionPointer: number = 0;
    public stopped: boolean = false;
}

/**
 * Attributes parsed from the paragraph YAML.
 */
export interface TapeAttrs {
    originalInput?: number[];
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
    }

    public possibleCommandList: Command[] = [];
    public commandList: CommandInstance[] = [];
    public state: TapeState;

    private newCommandIndex: number = -1;
    private newCommandParameter: string = "";
    private newCommandParameterText: string = "Parameter:";

    private showNewCommandParameter: boolean = true;

    private selectedCommandIndex: number = -1;

    private data!: Binding<TapeAttrs, "<">;

    private timer: any;


    /**
     * Handles clicks on the "Add command" button.
     */
    private addCommandButtonClick() {
        if (this.newCommandIndex == -1) {
            return;
        }

        const commandToAdd = this.possibleCommandList[this.newCommandIndex];
        this.addCommand(commandToAdd, this.newCommandParameter);
    }

    /**
     * Adds a command into the program.
     * Returns true if succesful, otherwise false.
     * @param commandToAdd The command.
     * @param parameterString The parameter of the command given as a string.
     */
    private addCommand(commandToAdd: Command, parameterString: string): boolean {
        let parameter;

        if (commandToAdd.usesParameter) {
            if (commandToAdd.getParameterType() === ParameterType.NUMBER) {
                parameter = parseInt(parameterString);
                if (isNaN(parameter)) {
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
        this.commandList.push(commandInstance);
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
     * Resets the tape machine.
     * Clears output, sets input to match the original input,
     * sets the instruction pointer to zero and clears memory.
     */
    private reset() {
        if (this.timer) {
            stop();
        }

        this.state.stopped = false;
        this.state.instructionPointer = 0;
        this.state.output = [];
        this.state.memory = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
        if (this.data.originalInput) {
            this.state.input = Array.from(this.data.originalInput);
        } else {
            this.state.input = [];
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
        this.commandList.forEach(c => result += c.command.name + " " + c.parameter + "\n");
        return result;
    }

    /**
     * Parses a program from a textual representation.
     * @param text The textual representation of a program.
     */
    private fromText(text: string) {
        text = text.replace("\r\n", "\n").replace("\r", "\n");
        const lines = text.split('\n');
        for (let i = 0; i < lines.length; i++) {
            const components = lines[i].split(' ');
            if (components.length === 0) {
                continue;
            }

            const command = this.possibleCommandList.find(c => c.name === components[0]);
            if (!command) {
                continue;
            }

            if (components.length === 1) {
                this.addCommand(command, "");
            } else {
                this.addCommand(command, components[1]);
            }
        }
    }

    private onCommandClick(index: number) {
        this.newCommandIndex = index;

        if (this.newCommandIndex > 0) {
            const command = this.possibleCommandList[this.newCommandIndex];
            if (command) {
                this.showNewCommandParameter = command.usesParameter;
                this.newCommandParameterText = command.getParameterName() + ":";
            }
        }
    }

    // Rendering functions below

    private getNewCommandColor(index: number) {
        if (index === this.newCommandIndex) {
            return "red";
        }

        return "black";
    }

    private getCommandColor(index: number) {
        if (index == this.selectedCommandIndex) {
            return "red";
        }

        if (index == this.state.instructionPointer) {
            return "green";
        }

        if (index > -1 && index < this.commandList.length) {
            if (this.commandList[index].command.isLabel())
                return "blue";
        }

        return "black";
    }

    private getRunButtonText() {
        if (this.timer) {
            return "Stop";
        }

        return "Run";
    }
}

timApp.component("tape", {
    controller: TapeController,
    bindings: {
        data: "<",
    },
    template: `
    <div class="no-highlight">
        <div>
            <span class="output">
                Output:
                <span ng-repeat="n in $ctrl.state.output track by $index">{{n}}</span>
            </span>
            Hand:
            <span class="hand" ng-bind="$ctrl.state.hand">
            </span>
            <span class="input">
                Input:
                <span ng-repeat="n in $ctrl.state.input track by $index">{{n}}</span>
            </span>
        </div>
        <div class="memory">
            <div>Memory:</div>
            <span ng-repeat="n in $ctrl.state.memory track by $index">
                <span ng-bind="n"></span>
                <span ng-bind="$index"></span>
            </span>
        </div>
        <div>
        </div>
        <span class="allowed-commands" ng-style="{'display': 'inline-block', 'vertical-align': 'top'}">
            Add command:
            <ul>
            <li ng-repeat="c in $ctrl.possibleCommandList" ng-style="{'color': $ctrl.getNewCommandColor($index)}" 
                ng-click="$ctrl.onCommandClick($index)">{{c.name}}</li>
            </ul>
        </span>
        <span class="program" ng-style="{'display': 'inline-block', 'vertical-align': 'top'}">
            Program
            <ul>
            <li ng-repeat="c in $ctrl.commandList" ng-click="$ctrl.selectedCommandIndex = $index" 
            ng-style="{'color': $ctrl.getCommandColor($index)}">{{c.getName()}}</li>
            </ul>
            <!--- <select ng-model="$ctrl.selected" size="10">
            <option ng-repeat="c in $ctrl.commandList" ng-style="{'color': $ctrl.getCommandColor($index)}">{{c.getName()}}</option>
            </select> --->
        </span>
        <div class="commandAddArea" ng-show="true">
             <span class="commandParameterArea" ng-show="$ctrl.showNewCommandParameter">
                <span>{{$ctrl.newCommandParameterText}}</span>
                <input ng-model="$ctrl.newCommandParameter">
             </span>
             <button class="timButton" ng-click="$ctrl.addCommandButtonClick()"><span>Add command</span>
        </div>
        <div class="commandRemoveArea" ng-show="true">
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