import $ from "jquery";
import {$http, $log, $window} from "../../ngimport";
import {createNewPar, getParId} from "./parhelpers";
import {onClick} from "./eventhandlers";
import {IScope} from "angular";
import {ViewCtrl} from "./viewctrl";
import {IAskedJsonJson} from "../../lecturetypes";
import {showQuestionEditDialog} from "../questionController";

export class QuestionHandler {
    public sc: IScope;
    public noQuestionAutoNumbering: boolean;
    public par: JQuery;
    public viewctrl: ViewCtrl;

    public initQuestions(sc: IScope, view: ViewCtrl): void {
        this.sc = sc;
        this.viewctrl = view;
        if (view.lectureMode) {
            this.noQuestionAutoNumbering = $window.noQuestionAutoNumbering;
        }

        onClick(".questionAddedNew", ($this, e) => {
            const question = $this;
            const $par = $(question).parent().parent();
            const parId = $($par)[0].getAttribute("id");
            const parNextId = getParId($par.next());
            this.showQuestionNew(parId, parNextId);
            this.par = ($(question).parent());
        });
    }

    async showQuestionNew(parId: string, parIdNext: string) {
        const response = await $http<{markup: IAskedJsonJson}>({
            url: "/getQuestionByParId",
            method: "GET",
            params: {
                par_id: parId,
                doc_id: this.viewctrl.docId,
            },
        });
        // TODO show question preview
        this.viewctrl.inLecture = false;
    }

    // Event handler for "Add question below"
    // Opens pop-up window to create question.
    async addQuestionQst(e, $par) {
        const parId = getParId($par);
        const parNextId = parId; // getParId($par.next());
        this.par = $par;
        const result = await showQuestionEditDialog({par_id_next: parNextId, qst: true, docId: this.viewctrl.docId});
        const $newpar = createNewPar();
        $par.before($newpar);
        this.viewctrl.addSavedParToDom(result.data, {par: getParId($newpar)});
    }

    async editQst(e, $par) {
        const parId = getParId($par);
        const parNextId = getParId($par.next());
        this.par = $par;
        try {
            var response = await
                $http<{markup: IAskedJsonJson}>({
                    url: "/getQuestionByParId",
                    method: "GET",
                    params: {par_id: parId, doc_id: this.viewctrl.docId, edit: true},
                });
        } catch (e) {
            $log.error("Could not get question.");
            return;
        }
        const data = response.data;
        if (!data.markup) {
            return; // not a question
        }
        // data.markup.qst = true;
        const result = await showQuestionEditDialog({
            markup: data.markup,
            par_id: parId,
            par_id_next: parNextId,
            docId: this.viewctrl.docId,
        });
        if (result.deleted) {
            this.viewctrl.handleDelete(result.data, {par: parId});
        } else {
            this.viewctrl.addSavedParToDom(result.data, {par: parId});
        }
    }

    // Event handler for "Add question below"
    // Opens pop-up window to create question.
    async addQuestion(e, $par) {
        const parNextId = getParId($par.next());
        this.par = $par;
        const result = await showQuestionEditDialog({par_id_next: parNextId, qst: false, docId: this.viewctrl.docId});
        const $newpar = createNewPar();
        $par.after($newpar);
        this.viewctrl.addSavedParToDom(result.data, {par: getParId($newpar)});
    }

    processQuestions() {
        const questions = $(".questionPar");
        let n = 1;
        let separator = ")";
        if (this.showQuestions()) {
            for (let i = 0; i < questions.length; i++) {
                const $par = questions.eq(i);
                const questionChildren = $par.children();
                const questionNumber = questionChildren.find(".questionNumber");
                // var questionTitle = getParAttributes($par).question;
                let questionTitle = questionNumber[0].innerHTML;
                if (questionTitle.length > 10) {
                    questionTitle = questionTitle.substr(0, 10) + "\r\n...";
                }
                const nt = parseInt(questionTitle);
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
        return (this.viewctrl.item.rights.teacher && (this.viewctrl.lectureMode || this.viewctrl.inLecture)) ||
            ($window.editMode && this.viewctrl.item.rights.editable);
    }
}
