import {IScope} from "angular";
import $ from "jquery";
import {showMessageDialog} from "../../ui/dialog";
import {documentglobals} from "../../util/globals";
import {$timeout} from "../../util/ngimport";
import {EditPosition, EditType} from "../editing/editing";
import {getParId, Paragraph} from "../parhelpers";
import {ViewCtrl} from "../viewctrl";
import {fetchAndEditQuestion, showQuestionEditDialog} from "./questionController";

export class QuestionHandler {
    public sc: IScope;
    public noQuestionAutoNumbering: boolean = false;
    public viewctrl: ViewCtrl;

    constructor(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        if (view.lectureCtrl.lectureSettings.lectureMode) {
            this.noQuestionAutoNumbering = documentglobals().noQuestionAutoNumbering;
        }
    }

    // Opens pop-up window to create question.
    async addQuestionQst(e: JQuery.Event, par: Paragraph) {
        const parNextId = getParId(par);
        if (!parNextId) {
            showMessageDialog("Not a valid paragraph.");
            return;
        }
        const result = await showQuestionEditDialog({par_id_next: parNextId, qst: true, docId: this.viewctrl.docId});
        if (result.type === "points") {
            throw new Error("unexpected result type from dialog");
        }
        this.viewctrl.editingHandler.addSavedParToDom(result.data, {type: EditType.AddAbove, par: par});
    }

    async editQst(e: JQuery.Event, par: Paragraph) {
        const parId = getParId(par);
        if (!parId) {
            showMessageDialog("Not a valid paragraph.");
            return;
        }
        const result = await fetchAndEditQuestion(this.viewctrl.docId, parId);
        if (result.type === "points") {
            throw new Error("unexpected result type from dialog");
        }
        const position: EditPosition = {type: EditType.Edit, pars: par};
        if (result.deleted) {
            this.viewctrl.editingHandler.handleDelete(position);
        } else {
            this.viewctrl.editingHandler.addSavedParToDom(result.data, position);
        }
    }

    // Event handler for "Add question below"
    // Opens pop-up window to create question.
    async addQuestion(e: JQuery.Event, par: Paragraph) {
        const parNextId = getParId(par.next());
        if (!parNextId) {
            showMessageDialog("Not a valid paragraph.");
            return;
        }
        const result = await showQuestionEditDialog({par_id_next: parNextId, qst: false, docId: this.viewctrl.docId});
        if (result.type === "points") {
            throw new Error("unexpected result type from dialog");
        }
        this.viewctrl.editingHandler.addSavedParToDom(result.data, {type: EditType.AddBelow, par: par});
    }

    async processQuestions() {
        const questions = $(".questionPar");
        if (this.showQuestions()) {
            let n = 1;
            let separator = ")";
            await $timeout();
            for (let i = 0; i < questions.length; i++) {
                const par = questions.eq(i);
                const questionChildren = par.children();
                const questionNumber = questionChildren.find(".questionNumber");
                if (questionNumber.length === 0) {
                    continue;
                }
                let questionTitle = questionNumber[0].innerHTML;
                if (questionTitle.length > 10) {
                    questionTitle = questionTitle.substr(0, 10) + "\r\n...";
                }
                const nt = parseInt(questionTitle, 10);
                let nr = "";
                if (isNaN(nt)) {
                    nr = (n) + "" + separator + "\r\n";
                } else {
                    n = nt;
                    const nrt = "" + n;
                    if (questionTitle.length > nrt.length) {
                        separator = questionTitle[nrt.length];
                    }
                }
                if (questionNumber[0] && questionNumber[0].innerHTML) {
                    if (this.noQuestionAutoNumbering) {
                        questionNumber[0].innerHTML = questionTitle;
                    } else {
                        questionNumber[0].innerHTML = nr + questionTitle;
                        n++;
                    }
                }
            }
        } else {
            questions.hide();
        }
    }

    showQuestions() {
        return (this.viewctrl.item.rights.teacher && this.viewctrl.lectureCtrl.lectureViewOrInLecture()) ||
            (documentglobals().editMode && this.viewctrl.item.rights.editable);
    }
}
