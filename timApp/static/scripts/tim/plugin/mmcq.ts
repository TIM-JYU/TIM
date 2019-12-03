import angular, {IController} from "angular";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";

const mcqMod = angular.module("MCQ", []);

interface MMCQContent<State> {
    state?: State,
    question: {
        headerText: string | null,
        buttonText: string | null,
        falseText: string | null,
        trueText: string | null,
        wrongText: string | null,
        correctText: string | null,
        choices: Array<{
            text: string,
            reason?: string,
            correct?: boolean,
        }>,
    },
}

class MCQBase<State> implements IController {
    private static $inject = ["$element"];
    protected headerText: string = "Check your understanding";
    protected buttonText: string = "Submit";
    protected content!: MMCQContent<State>;

    constructor(protected element: JQLite) {

    }

    $onInit() {
        this.content = JSON.parse(this.element.attr("data-content")!) as MMCQContent<State>;
    }

    protected getId() {
        return this.element.parent().attr("id");
    }
}

class MMCQ extends MCQBase<null | boolean[]> {
    private active: boolean = false;
    private checked: boolean = true;
    private trueText: string = "True";
    private falseText: string = "False";
    private correctText: string = "Correct!";
    private wrongText: string = "Wrong!";
    private answer!: Array<boolean | "false" | "true">;

    $onInit() {
        super.$onInit();
        const fields = ["headerText", "buttonText", "falseText", "trueText", "correctText", "wrongText"] as const;
        fields.forEach((opt) => {
            const o = this.content.question[opt];
            if (o !== null) {
                this[opt] = o;
            }
        });
        const s = this.content.state;
        if (s == null) {
            this.checked = false;
            this.active = true;
            this.answer = new Array(this.content.question.choices.length);
        } else {
            this.answer = s;
        }
    }

    extract() {
        for (let j = 0; j < this.answer.length; j++) {
            if (this.answer[j] == "false") {
                this.answer[j] = false;
            }
            if (this.answer[j] == "true") {
                this.answer[j] = true;
            }
        }
        this.active = false;
        return this.answer;
    }

    async submit() {
        const message = {
            input: this.extract(),
        };
        const ident = this.getId();

        const r = await to($http<{web: MMCQ["content"]}>({
            method: "PUT",
            url: `/mmcq/${ident}/answer/`,
            data: message,
        }));
        if (r.ok) {
            this.content = r.result.data.web;
            this.checked = true;
        }
    }
}

mcqMod.component("mmcq", {
    bindings: {},
    template: `
<div class="mcq">
    <p class="header" style="font-weight:bold" ng-bind-html="$ctrl.headerText"></p>
    <p class="stem" ng-bind-html="$ctrl.content.question.stem"></p>
    <table>
        <tr>
            <th></th>
            <th ng-bind-html="$ctrl.trueText"></th>
            <th ng-bind-html="$ctrl.falseText"></th>
            <th ng-if="$ctrl.checked"></th>
            <th ng-if="$ctrl.checked"></th>
        </tr>
        <tr ng-repeat="choice in $ctrl.content.question.choices">
            <td><span class="MCQItem" ng-bind-html="choice.text"></span></td>
            <td class="text-center">
                <input type="checkbox" ng-model="$ctrl.answer[$index]" ng-true-value="true" ng-false-value="false"/>
            </td>
            <td class="text-center">
                <input type="checkbox" ng-model="$ctrl.answer[$index]" ng-false-value="true" ng-true-value="false"/>
            </td>
            <td ng-if="$ctrl.checked">
                <span ng-bind-html="$ctrl.correctText"
                      ng-if="!($ctrl.answer[$index] == null) && !(choice.correct == null) && (''+$ctrl.answer[$index] == ''+choice.correct)"
                      class="correct"></span>
                <span ng-bind-html="$ctrl.wrongText"
                      ng-if="!($ctrl.answer[$index] == null) && !(choice.correct == null) && (''+$ctrl.answer[$index] !==''+choice.correct)"
                      class="wrong"></span>
            </td>
            <td ng-if="choice.reason">
                <span class="MCQExpl" ng-bind-html="choice.reason"></span></td>
        </tr>
    </table>
    <div class="text-center">
        <button ng-click="$ctrl.submit()" ng-bind-html="$ctrl.buttonText"></button>
    </div>
</div>
`,
    controller: MMCQ,
});

class MCQ extends MCQBase<number | null> {
    private userSelection: number | undefined;

    $onInit() {
        super.$onInit();
        this.userSelection = this.content.state || undefined;
        const fields = ["headerText", "buttonText"] as const;
        fields.forEach((opt) => {
            const o = this.content.question[opt];
            if (o !== null) {
                this[opt] = o;
            }
        });
    }

    async submit() {
        const message = {
            input: this.userSelection,
        };
        const ident = this.getId();

        const r = await to($http<{web: MCQ["content"]}>({
            method: "PUT",
            url: `/mcq/${ident}/answer/`,
            data: message,
        }));
        if (r.ok) {
            this.content = r.result.data.web;
        }
    }
}

mcqMod.component("mcq", {
    bindings: {},
    template: `
<div class="mcq">
    <p class="header" style="font-weight:bold" ng-bind-html="$ctrl.headerText"></p>
    <p class="stem" ng-bind-html="$ctrl.content.question.stem"></p>
    <table>
        <tr ng-repeat="choice in $ctrl.content.question.choices">
            <td>
                <input type="radio" ng-model="$ctrl.userSelection" ng-value="$index"/>
                <span ng-if="$ctrl.content.state==$index&&choice.correct">✓</span>
                <span ng-if="$ctrl.content.state==$index&&!choice.correct">✗</span>
            </td>
            <td><span class="MCQItem" ng-bind-html="choice.text"></span>
                <span ng-if="choice.reason" class="MCQExpl" ng-bind-html="choice.reason"></span></td>
        </tr>
    </table>
    <div class="text-center">
        <button ng-click="$ctrl.submit()" ng-bind-html="$ctrl.buttonText"></button>
    </div>
</div>
`,
    controller: MCQ,
});

export const moduleDefs = [mcqMod];
