import $ from "jquery";
import {$http, $log, $rootScope, $window} from "../../ngimport";
import {createNewPar, getParId} from "./parhelpers";
import {onClick} from "./eventhandlers";
import {IScope} from "angular";
import {ViewCtrl} from "./viewctrl";
import {IAskedJsonJson} from "../../lecturetypes";

export class QuestionHandler {
    public sc: IScope;
    public noQuestionAutoNumbering: boolean;
    public par: JQuery;
    public viewctrl: ViewCtrl;
    public showQuestionPreview: boolean;
    public questionParIdNext: string;

    public initQuestions(sc: IScope, view: ViewCtrl): void {
        this.sc = sc;
        this.viewctrl = view;
        if (view.lectureMode) {
            this.noQuestionAutoNumbering = $window.noQuestionAutoNumbering;
        }

        this.viewctrl.questionShown = false;

        onClick(".questionAddedNew", ($this, e) => {
            const question = $this;
            const $par = $(question).parent().parent();
            const parId = $($par)[0].getAttribute("id");
            const parNextId = getParId($par.next());
            this.showQuestionNew(parId, parNextId);
            this.par = ($(question).parent());
        });

        this.sc.$on("closeQuestionPreview", () => {
                this.showQuestionPreview = false;
            },
        );
    }

    async showQuestionNew(parId: string, parIdNext: string) {
        this.viewctrl.questionParId = parId;
        this.questionParIdNext = parIdNext;

        const response = await $http<{markup: IAskedJsonJson}>({
            url: "/getQuestionByParId",
            method: "GET",
            params: {
                par_id: this.viewctrl.questionParId,
                doc_id: this.viewctrl.docId,
            },
        });
        this.viewctrl.markup = response.data.markup;
        $rootScope.$broadcast("changeQuestionTitle", {questionTitle: this.viewctrl.markup.json.questionTitle});
        $rootScope.$broadcast("setPreviewJson", {
            markup: this.viewctrl.markup,
            questionParId: this.viewctrl.questionParId,
            questionParIdNext: this.questionParIdNext,
            isLecturer: this.viewctrl.isLecturer,
        });

        this.viewctrl.inLecture = false;
        this.showQuestionPreview = true;
    }

    // Event handler for "Add question below"
    // Opens pop-up window to create question.
    addQuestionQst(e, $par) {
        const parId = getParId($par);
        const parNextId = parId; // getParId($par.next());
        const $newpar = createNewPar();
        $par.before($newpar);
        $rootScope.$broadcast("toggleQuestion");
        $rootScope.$broadcast("newQuestion", {par_id: parId, par_id_next: parNextId, qst: true});
        this.par = $par;
    }

    async editQst(e, $par) {
        const parId = getParId($par);
        const parNextId = getParId($par.next());
        this.par = $par;
        // $rootScope.$broadcast('toggleQuestion');
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
        this.viewctrl.json = data.markup.json;  // TODO: näistä pitäisi päästä eroon, kaikki markupin kautta!
        this.viewctrl.markup = data.markup;
        // data.markup.qst = true;
        $rootScope.$broadcast("changeQuestionTitle", {questionTitle: this.viewctrl.json.questionTitle});
        $rootScope.$broadcast("editQuestion", {
            par_id: parId,
            par_id_next: parNextId,
            markup: data.markup,
        });
    }

    // Event handler for "Add question below"
    // Opens pop-up window to create question.
    addQuestion(e, $par) {
        const parId = getParId($par);
        const parNextId = getParId($par.next());
        const $newpar = createNewPar();
        $par.after($newpar);
        $rootScope.$broadcast("toggleQuestion");
        $rootScope.$broadcast("newQuestion", {par_id: parId, par_id_next: parNextId});
        this.par = $par;
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
