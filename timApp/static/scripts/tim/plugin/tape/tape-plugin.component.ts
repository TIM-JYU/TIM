import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, ElementRef, Input, NgModule, ViewChild} from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {AnswerSheetModule} from "tim/document/question/answer-sheet.component";
import {PurifyModule} from "tim/util/purify.module";
import {copyToClipboard, isIOS} from "tim/util/utils";
import type {PluginJson} from "tim/plugin/angular-plugin-base.directive";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";

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
        if (this.usesParameter) {
            return this.name + "(" + param + ")";
        }
        return this.name;
    }

    public isLabel(): boolean {
        return false;
    }

    public getParameterType(): ParameterType {
        return ParameterType.STRING;
    }
}

/**
 * The parameters given to a command when it is executed.
 */
export class CommandParameters<T> {
    constructor(
        state: TapeState,
        mainParam: T,
        commandList: CommandInstance[]
    ) {
        this.state = state;
        this.mainParam = mainParam;
        this.commandList = commandList;
    }

    public state: TapeState;
    public mainParam: T;
    public commandList: CommandInstance[] = [];
}

// <editor-fold desc="Commands">

class InputC extends Command {
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
            params.state.output.unshift(params.state.hand);
            while (params.state.output.length > 6) {
                params.state.output.pop();
            }
            params.state.hand = undefined;
        }
    }
}

class MemoryCommand extends Command {
    constructor(name: string, abbr: string) {
        super(name, abbr);
    }

    public execute(params: CommandParameters<unknown>) {}

    public getParameterName() {
        return "Memory index";
    }

    public getParameterType(): ParameterType {
        return ParameterType.NUMBER;
    }
}

class Add extends MemoryCommand {
    constructor() {
        super("ADD", "a");
    }

    public execute(params: CommandParameters<number>) {
        const memoryIndex = params.mainParam;
        if (
            memoryIndex >= params.state.memory.length ||
            params.state.hand == null
        ) {
            return;
        }

        params.state.hand =
            params.state.hand + params.state.memory[memoryIndex];
    }
}

class Sub extends MemoryCommand {
    constructor() {
        super("SUB", "s");
    }

    public execute(params: CommandParameters<number>) {
        const memoryIndex = params.mainParam;
        if (
            memoryIndex >= params.state.memory.length ||
            params.state.hand == null
        ) {
            return;
        }

        params.state.hand =
            params.state.hand - params.state.memory[memoryIndex];
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

    public execute(params: CommandParameters<unknown>) {}

    public getParameterName() {
        return "Label";
    }

    public isLabel() {
        return true;
    }

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
        const index = params.commandList.findIndex(
            (c) => c.command.isLabel() && c.parameter === params.mainParam
        );
        if (index > -1) {
            params.state.instructionPointer = index + 1;
        }
    }

    public getParameterName() {
        return "Label";
    }
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

    public getParameterName() {
        return "Label";
    }
}

class JumpIfEmpty extends Jump {
    constructor() {
        super("JUMPIFEMPTY", "E");
    }

    public execute(params: CommandParameters<unknown>) {
        if (params.state.input.length === 0) {
            this.jumpToLabel(params);
        }
    }

    public getParameterName() {
        return "Label";
    }
}

class InvalidCommand extends Command {
    static Instance = new InvalidCommand();

    constructor() {
        super("INVALID", "!");
    }

    public execute(params: CommandParameters<unknown>) {
        // Skip on purpose
    }

    toString(param: string | number | undefined): string {
        if (param === undefined) {
            return "";
        }
        return `${param} // INVALID`;
    }
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

        // TODO: if this.parameter is undefined, what should be the default?
        if (this.command.isLabel()) {
            return `${this.parameter ?? "undefined"}:`;
        }

        return `${this.command.name}(${this.parameter ?? "undefined"})`;
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
    useJumpIfEmpty: boolean;
}

function scrollElementVisibleInParent(
    el: Element,
    par: Element,
    extraY: number
) {
    // Scroll par window so that el comes visible if it is not visible
    // it is ensured that at least extraY times el height is over or under el
    const rect = el.getBoundingClientRect();
    const prect = par.getBoundingClientRect();

    let dy = prect.top - rect.top;
    if (dy > 0) {
        // el too high
        par.scrollTop -= dy + extraY * rect.height;
        return false;
    }

    dy = prect.top + prect.height - (rect.top + rect.height);
    if (dy < 0) {
        // el too low
        par.scrollTop -= dy - extraY * rect.height;
        return false;
    }
    return true;
}

/**
 * The tape machine controller.
 */
@Component({
    selector: "tim-tape",
    template: `
        <div class="no-highlight robotMainDiv fullinv">
            <div class="tapeAndRobotDiv">
                <div class="outputbelt">
                    <div class="outputvalues">
                        <div class="output">
                            <div *ngFor="let n of state.output" class="tapeItem">{{n}}</div>
                        </div>
                    </div>
                    <img alt="output" src="/static/images/tape/output.png"/>
                    <span>Output</span>
                </div>
                <div class="robotDiv">
                    <div class="handItem">
                        <div class="tapeItem" [innerText]="getHand()"></div>
                    </div>
                    <div class="robotImage">
                        <img alt="robot" src="/static/images/tape/robot.png"/>
                        <div class="robotRunButtons">
                            <button class="timButton" (click)="step()"><span>Step</span></button>
                            <button class="timButton" (click)="run()"><span [innerText]="getRunButtonText()"></span>
                            </button>
                            <button class="timButton" (click)="reset()"><span>Reset</span></button>
                        </div>
                    </div>
                </div>
                <div class="inputbelt">
                    <div class="inputvalues">
                    <span class="input">
                        <div *ngFor="let n of state.input" class="tapeItem">{{n}}</div>
                    </span>
                    </div>
                    <img alt="input" src="/static/images/tape/input.png"/>
                    <span>Input</span>
                </div>
            </div>
            <div class="memoryArea">
                <div>Memory:</div>
                <div *ngFor="let n of state.memory; let i = index" class="memoryContainer">
                    <div [innerText]="n" class="memoryValue"></div>
                    <div [innerText]="getMemoryText(i)" class="memoryIndex"></div>
                </div>
            </div>
            <div class="programArea">
            <span class="commandListContainer newCommandList" *ngIf="!data.hideCommands">
                Commands:
                <ul class="list-unstyled listBox">
                <li *ngFor="let c of possibleCommandList; let i = index" class="command"
                    [ngStyle]="{'color': getNewCommandColor(i)}"
                    (click)="onCommandClick(i)">{{c.name}}</li>
                </ul>
            </span>
                <div class="commandAddArea" *ngIf="!data.hideCommands">
                    <div class="commandParamArea">
                     <span class="commandParameterArea" *ngIf="showNewCommandParameter">
                        <span>{{newCommandParameterText}}</span>
                        <input size="5em" [(ngModel)]="newCommandParameter">
                     </span>
                    </div>
                    <div class="commandButtons">
                        <button class="timButton" (click)="insertCommandButtonClick()">
                            <span>
                                Insert above 
                                <i class="glyphicon glyphicon-triangle-right"></i>
                            </span>
                        </button>
                        <button class="timButton" (click)="removeCommand()"
                                [disabled]="selectedCommandIndex == (commandList.length + 1)">
                            <span>
                                Remove selected
                            </span>
                        </button>
                        <button class="timButton" (click)="copyAll()" *ngIf="!data.hideCopyAll"><span>Copy all</span>
                        </button>
                        <!-- <button class="timButton" (click)="paste()"><span>Paste</span></button> -->
                    </div>
                </div>
                <span class="commandListContainer">
                Program:
                <textarea #textAreaRobotProgram class="robotEditArea textAreaRobotProgram" [(ngModel)]="programAsText"
                          *ngIf="textmode"></textarea>
                <ul class="cmditems list-unstyled listBox programCommandList" *ngIf="!textmode">
                <li *ngFor="let c of commandList; let i = index" class="command" [attr.data-command]="c.command.name"
                    [class.activeCommand]="i == selectedCommandIndex"
                    (click)="selectedCommandIndex = i"
                    [ngStyle]="{'color': getCommandColor(i), 'background-color': getCommandBackgroundColor(i)}">{{c.getName()}}</li>
                <li class="command lastCommand" [ngStyle]="{'color': getCommandColor(commandList.length + 1), 'background-color': getCommandBackgroundColor(commandList.length + 1)}"
                    [class.activeCommand]="(commandList.length + 1) == selectedCommandIndex"
                    (click)="selectedCommandIndex = (commandList.length + 1)">// end</li>
                </ul>
                <p class="red" *ngIf="hasInvalidCommands">Some commands are invalid, they will be ignored</p>
                    <!--- <select [(ngModel)]="selected" size="10">
                    <option *ngFor="let c of commandList" [ngStyle]="{'color': getCommandColor($index)}">{{c.getName()}}</option>
                    </select> --->
            </span>
                <div class="robotPresets">
                    <div class="presetInput" *ngIf="!data.hidePresetInput">
                        <div class="commandParamArea">
                         <span>
                            <span>Preset input:</span>
                            <input size="15em" [(ngModel)]="inputString">
                         </span>
                        </div>
                    </div>
                    <div class="presetInput" *ngIf="!data.hidePresetMem">
                        <div class="commandParamArea">
                         <span>
                            <span>Preset memory:</span>
                            <input size="15em" [(ngModel)]="memString">
                         </span>
                        </div>
                    </div>
                    <span *ngIf="!data.hideTextMode">
                    <label>
                        <input type="checkbox" [(ngModel)]="textmodeCB" class="belttextbox" name="textmode"
                               value="textmode"
                               (click)="changeMode()"/>
                    Text mode
                    </label>
                </span>
                </div>
            </div>
        </div>
    `,
    styleUrls: ["tape-plugin.component.scss"],
})
export class TapePluginContent implements PluginJson {
    public data!: TapeAttrs;
    @Input() json!: string;
    @ViewChild("textAreaRobotProgram")
    textAreaRobotProgram?: ElementRef<HTMLTextAreaElement>;
    private element: JQuery<HTMLElement>;

    constructor(hostElement: ElementRef<HTMLElement>) {
        this.element = $(hostElement.nativeElement);
        this.state = new TapeState();
        this.possibleCommandList = [
            new InputC(),
            new Output(),
            new Add(),
            new Sub(),
            new CopyTo(),
            new CopyFrom(),
            new DefineLabel(),
            new Jump("JUMP", "j"),
            new JumpIfZero(),
            new JumpIfNeg(),
        ];
    }

    ngOnInit() {
        // TODO: Convert to proper Angular plugin and use base64 encoding
        if (this.json) {
            this.data = JSON.parse(this.json);
        }

        this.iOS = isIOS();
        if (this.data.useJumpIfEmpty) {
            this.possibleCommandList.push(new JumpIfEmpty());
        }

        this.inputString = "";
        if (this.data.presetInput) {
            this.inputString = this.data.presetInput.toString();
        }
        this.memString = "";
        if (this.data.presetMemoryState) {
            this.memString = this.data.presetMemoryState.toString();
        }
        this.reset();
        this.selectedCommandIndex = this.commandList.length + 1;
    }

    // List of commands supported by the tape machine
    public possibleCommandList: Command[] = [];

    // List of commands in the current program
    public commandList: CommandInstance[] = [];

    public state: TapeState;

    private newCommandIndex: number = -1;
    newCommandParameter: string = "";
    newCommandParameterText: string = "Parameter:";

    showNewCommandParameter: boolean = false;

    // The index of the selected command of the current program, if any
    selectedCommandIndex: number = -1;

    private timer?: number;

    inputString: string = "";
    memString: string = "";
    programAsText: string = "";
    textmode: boolean = false; // do not use as [(ngModel)], because ipad changes it different time than PC
    textmodeCB: boolean = false;
    private iOS: boolean = false;

    /**
     * Handles clicks on the "Insert command" button.
     */
    insertCommandButtonClick() {
        if (this.newCommandIndex == -1) {
            return;
        }
        if (this.textmode) {
            const n = this.element
                .find(".textAreaRobotProgram")
                .getSelection().start;
            let r = 0;
            const text = this.programAsText;
            for (let i = 0; i < n; i++) {
                if (text[i] === "\n") {
                    r++;
                }
            }
            this.selectedCommandIndex = r;
        }

        const commandToAdd = this.possibleCommandList[this.newCommandIndex];
        const commandAdded = this.addCommand(
            commandToAdd,
            this.newCommandParameter,
            this.selectedCommandIndex
        );
        if (!commandAdded) {
            return;
        }
        if (this.selectedCommandIndex !== -1) {
            this.selectedCommandIndex++;
        }
        if (this.textmode) {
            this.textAll();
        }

        this.checkCommandInView(this.selectedCommandIndex);
    }

    hasInvalidCommands = false;

    private addInvalidCommand(line: string) {
        // Strip any comments
        line = line.split("//")[0].trim();
        const ins = new CommandInstance(InvalidCommand.Instance, line);
        this.commandList.push(ins);
        this.hasInvalidCommands = true;
    }

    /**
     * Inserts a command into the program.
     * Returns true if successful, otherwise false.
     * @param commandToAdd The command.
     * @param parameterString The parameter of the command given as a string.
     * @param index (Optional) The place where the command is inserted in the program, if not to the end.
     */
    private addCommand(
        commandToAdd: Command,
        parameterString: string,
        index: number = -1
    ): boolean {
        let parameter;

        // TODO maybe move parameter validation to the commands themselves?
        if (commandToAdd.usesParameter) {
            if (commandToAdd.getParameterType() === ParameterType.NUMBER) {
                parameter = parseInt(parameterString, 10);
                // for now we assume all numbers are memory indexes
                if (
                    isNaN(parameter) ||
                    parameter < 0 ||
                    parameter >= this.state.memory.length
                ) {
                    return false;
                }
            } else {
                if (parameterString === "" || parameterString.includes(" ")) {
                    // don't allow spaces to make parsing easier
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
        if (this.commandList.length > 10) {
            // long program, ensure command is visible
            if (n >= this.commandList.length) {
                n = this.commandList.length - 1;
            }
            if (n < 0) {
                return;
            }
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
    step() {
        this.changeList();
        if (
            this.state.instructionPointer >= this.commandList.length ||
            this.state.stopped
        ) {
            if (this.timer) {
                this.stop();
            }
            return;
        }

        const command = this.commandList[this.state.instructionPointer];
        this.state.instructionPointer++;
        command.command.execute(
            new CommandParameters(
                this.state,
                command.parameter,
                this.commandList
            )
        );

        this.checkCurrentCommandInView();
    }

    private automaticStep() {
        this.step();
    }

    /**
     * Handles clicks on the Run / Stop button.
     */
    run() {
        if (
            this.state.instructionPointer >= this.commandList.length ||
            this.state.stopped
        ) {
            return;
        }

        if (this.timer) {
            this.stop();
        } else {
            this.timer = window.setInterval(() => this.automaticStep(), 500);
        }
    }

    private static arrayFromString(s?: string): number[] {
        if (!s) {
            return [];
        }
        s = s.trim();
        if (s === "") {
            return [];
        }

        const result = [];
        const pieces = s.split(",");
        for (const n of pieces) {
            let val = Number(n.trim());
            if (!val) {
                val = 0;
            }
            result.push(val);
        }
        return result;
    }

    /**
     * Resets the tape machine to its original state.
     * Clears output, sets input to match the original input,
     * sets the instruction pointer to zero and clears memory.
     */
    reset() {
        if (this.timer) {
            stop();
        }

        this.state.stopped = false;

        this.state.input = TapePluginContent.arrayFromString(this.inputString); // Array.from(this.data.presetInput);

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

        const memState = TapePluginContent.arrayFromString(this.memString); // Array.from(this.data.presetInput);

        for (
            let i = 0;
            i < memState.length && i < this.state.memory.length;
            i++
        ) {
            this.state.memory[i] = memState[i];
        }

        this.state.instructionPointer = 0;
        console.log(this.data.presetCode);
        if (this.data.presetCode && this.commandList.length === 0) {
            // assign pre-defined code only if we have no code right now,
            // so we don't clear the user's code on reset
            this.fromText(this.data.presetCode);
            this.selectedCommandIndex = this.commandList.length + 1;
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
    removeCommand() {
        if (
            this.selectedCommandIndex > -1 &&
            this.selectedCommandIndex < this.commandList.length
        ) {
            this.commandList.splice(this.selectedCommandIndex, 1);

            // If removed command was at the last line, move one up if possible
            if (this.selectedCommandIndex == this.commandList.length) {
                this.selectedCommandIndex = this.commandList.length - 1;
            }
            // If no commands are available, select the stop mark
            if (this.commandList.length == 0) {
                this.selectedCommandIndex = 1;
            }
        }

        this.hasInvalidCommands = this.commandList.some(
            (c) => c.command instanceof InvalidCommand
        );
        // this.textAll();
    }

    private textAll() {
        this.programAsText = this.toText();
    }

    copyAll() {
        if (this.textmode && this.textAreaRobotProgram) {
            copyToClipboard(this.textAreaRobotProgram.nativeElement.value);
        } else {
            copyToClipboard(this.toText());
        }
    }

    paste() {
        if (this.programAsText === "") {
            return;
        }
        this.commandList = [];
        this.hasInvalidCommands = false;
        this.fromText(this.programAsText);
        this.selectedCommandIndex = this.commandList.length + 1;
    }

    private changeList() {
        if (!this.textmode) {
            return;
        }
        this.textmode = false;
        this.textmodeCB = false;
        this.paste();
    }

    private changeText() {
        if (this.textmode) {
            return;
        }
        this.textmode = true;
        this.textmodeCB = true;
        this.textAll();
    }

    changeMode() {
        if (this.textmode) {
            this.changeList();
        } else {
            this.changeText();
        }
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
                const defineLabelCommand = this.possibleCommandList.find(
                    (c) => c.name === "Define Label"
                );
                if (defineLabelCommand) {
                    this.addCommand(
                        defineLabelCommand,
                        line.slice(0, line.length - 1)
                    );
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
            const command = this.possibleCommandList.find(
                (c) => c.name === name
            );
            console.log(name, components, command);
            if (!command) {
                this.addInvalidCommand(line);
                continue;
            }

            let ok = true;
            if (components.length === 1) {
                ok = this.addCommand(command, "");
            } else {
                ok = this.addCommand(command, components[1].trim());
            }

            if (!ok) {
                this.addInvalidCommand(line);
            }
        }
    }

    /**
     * Handles clicks on the "add command" command list.
     * @param index The index of the command that was clicked.
     */
    onCommandClick(index: number) {
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
    getNewCommandColor(index: number) {
        if (index === this.newCommandIndex) {
            return "red";
        }

        return "unset";
    }

    private isCommandInvalid(index: number) {
        return (
            index > -1 &&
            index < this.commandList.length &&
            this.commandList[index].command instanceof InvalidCommand
        );
    }

    /**
     * Gets the color of an item in the current program command list.
     * @param index The index of the program command.
     */
    getCommandColor(index: number) {
        const isInvalid = this.isCommandInvalid(index);

        if (index == this.selectedCommandIndex) {
            if (isInvalid) {
                return "black";
            }
            return "red";
        }

        if (isInvalid) {
            return "white";
        }

        if (index > -1 && index < this.commandList.length) {
            if (this.commandList[index].command.isLabel()) {
                return "blue";
            }
        }

        return "black";
    }

    getCommandBackgroundColor(index: number) {
        if (
            index == this.state.instructionPointer ||
            (index > this.commandList.length &&
                this.state.instructionPointer >= this.commandList.length)
        ) {
            return "yellow";
        }

        if (this.isCommandInvalid(index)) {
            return "red";
        }

        return "white";
    }

    getRunButtonText() {
        if (this.timer) {
            return "Stop";
        }

        return "Run";
    }

    getHand() {
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
    getMemoryText(index: number) {
        return /* "#" + */ index.toString();
    }
}

@NgModule({
    declarations: [TapePluginContent],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        AnswerSheetModule,
        PurifyModule,
    ],
})
export class TapePluginModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("tim-tape", TapePluginModule, TapePluginContent);
