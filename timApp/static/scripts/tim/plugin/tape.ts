import {timApp} from "../app";
import {IController, IRootElementService, IScope} from "angular";
import {Binding} from "../util/utils";

/**
 * Generic base class for all tape machine commands.
 */
abstract class Command {
    protected constructor(name: string) {
        this.name = name;
    }

    public name: string;
    public usesParameter: boolean = true;

    public abstract execute(params: CommandParameters): void;
}

/**
 * The parameters given to a command when it is executed.
 */
class CommandParameters {
    constructor(state: TapeState, mainParam: number) {
        this.state = state;
        this.mainParam = mainParam;
    }

    state: TapeState;
    mainParam: number;
}

// Commands

class Input extends Command {
    constructor() {
        super("INPUT");
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
        super("OUTPUT");
        this.usesParameter = false;
    }

    public execute(params: CommandParameters) {
        if (params.state.hand !== null) {
            params.state.output.push(params.state.hand);
            params.state.hand = null;
        }
    }
}

class Add extends Command {
    constructor() {
         super("ADD");
    }

    public execute(params: CommandParameters) {
        const memoryIndex = params.mainParam;
        if (memoryIndex >= params.state.memory.length || params.state.hand == null) {
            return;
        }

        params.state.hand = params.state.hand + params.state.memory[memoryIndex];
    }
}

class Sub extends Command {
    constructor() {
        super("SUB");
    }

    public execute(params: CommandParameters) {
        const memoryIndex = params.mainParam;
        if (memoryIndex >= params.state.memory.length || params.state.hand == null) {
            return;
        }

        params.state.hand = params.state.hand - params.state.memory[memoryIndex];
    }
}

class CopyTo extends Command {
    constructor() {
        super("COPYTO");
    }

    public execute(params: CommandParameters) {
        if (params.state.hand) {
            params.state.memory[params.mainParam] = params.state.hand;
        }
    }
}

class CopyFrom extends Command {
    constructor() {
        super("COPYFROM");
    }

    public execute(params: CommandParameters) {
        const memoryIndex = params.mainParam;
        if (memoryIndex < params.state.memory.length) {
            params.state.hand = params.state.memory[memoryIndex];
        }
    }
}

class Jump extends Command {
    constructor() {
        super("JUMP");
    }

    public execute(params: CommandParameters) {
        params.state.instructionPointer = params.mainParam;
    }
}

class JumpIfZero extends Command {
    constructor() {
        super("JUMPIFZERO");
    }

    public execute(params: CommandParameters) {
        if (params.state.hand !== null && params.state.hand === 0) {
            params.state.instructionPointer = params.mainParam;
        }
    }
}

class JumpIfNeg extends Command {
    constructor() {
        super("JUMPIFNEG");
    }

    public execute(params: CommandParameters) {
        if (params.state.hand !== null && params.state.hand < 0) {
            params.state.instructionPointer = params.mainParam;
        }
    }
}

/**
 * An instance of a command to be executed. Includes the command and its parameters.
 */
class CommandInstance {
    constructor(cmd: Command, param: number) {
        this.command = cmd;
        this.parameter = param;
    }

    public command: Command;
    public parameter: number;

    public getName() {
        if (!this.command.usesParameter) {
            return this.command.name;
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
            new CopyTo(), new CopyFrom(), new Jump(), new JumpIfZero(), new JumpIfNeg()];
    }

    $onInit() {
        this.step = this.step.bind(this);
        this.reset();
    }

    public possibleCommandList: Command[] = [];
    public commandList: CommandInstance[] = [];
    public state: TapeState;

    public newCommandName: string = "";
    public newCommandParameter: string = "";

    private data!: Binding<TapeAttrs, "<">;

    private timer: any;


    /**
     * Adds a command to the program.
     */
    private addCommand() {
        const commandToAdd = this.possibleCommandList.find(c => c.name === this.newCommandName);
        if (!commandToAdd) {
            return;
        }

        let parameter = 0;

        if (commandToAdd.usesParameter) {
            parameter = parseInt(this.newCommandParameter);
            if (isNaN(parameter)) {
                return;
            }
        }

        const commandInstance = new CommandInstance(commandToAdd, parameter);
        this.commandList.push(commandInstance);
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
        command.command.execute(new CommandParameters(this.state, command.parameter));

        this.scope.$apply();
    }

    /**
     * Runs the program.
     */
    private run() {
        if (this.state.instructionPointer >= this.commandList.length || this.state.stopped) {
            return;
        }

        if (this.timer) {
            this.stop();
        } else {
            this.timer = setInterval(this.step, 500);
        }
    }

    /**
     * Resets the tape machine.
     * Clears output, sets input to match the original input, sets the instruction pointer to zero
     * and clears memory.
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
            this.state.input = this.data.originalInput;
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

    // Rendering functions below

    private getCommandColor(index: number) {
        if (index == this.state.instructionPointer) {
            return "red";
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
        <span>Add command:</span>
        <span>Program</span>
        </div>
        <span class="allowed-commands">
            <select ng-model="$ctrl.newCommandName" size="10">
            <option ng-repeat="c in $ctrl.possibleCommandList">{{c.name}}</option>
            </select>
        </span>
        <span class="program">
            <select size="10">
            <option ng-repeat="c in $ctrl.commandList" ng-style="{'color': $ctrl.getCommandColor($index)}">{{c.getName()}}</option>
            </select>
        </span>
        <div class="commandAddArea" ng-show="true">
             <span>Parameter:</span>
             <input ng-model="$ctrl.newCommandParameter">
             <button class="timButton" ng-click="$ctrl.addCommand()"><span>Add command</span>
        </div>
        <div class="commandRemoveArea" ng-show="true">
            <button class="timButton"><span>Remove command</span></button>
        </div>
        <div>
             <button class="timButton" ng-click="$ctrl.run()"><span ng-bind="$ctrl.getRunButtonText()"></span>
             <button class="timButton" ng-click="$ctrl.step()"><span>Step</span>
             <button class="timButton" ng-click="$ctrl.reset()"><span>Reset</span>
        </div>
    </div>
    `
});