import $ from "jquery";
import {$http, $log, $rootScope, $window} from "../../ngimport";
import {createNewPar, getElementByParId, getParId} from "./parhelpers";
import {onClick} from "./eventhandlers";
import {IScope} from "angular";
import {ViewCtrl} from "./viewctrl";

export class QuestionHandler {
    public sc: IScope;
    public questionShown: boolean;
    public firstTimeQuestions: boolean;
    public noQuestionAutoNumbering: boolean;
    public par: JQuery;
    public markup: {json?, points?, expl?};
    public questionParId: string;
    public qId: string;
    public json: {questionTitle};
    public viewctrl: ViewCtrl;
    public showQuestionPreview: boolean;
    public questionParIdNext: string;

    public initQuestions(sc: IScope, view: ViewCtrl): void {
        this.sc = sc;
        this.viewctrl = view;
        if (view.lectureMode) {
            this.noQuestionAutoNumbering = $window.noQuestionAutoNumbering;
        }

        this.questionShown = false;
        this.firstTimeQuestions = true;

        onClick(".questionAdded", ($this, e) => {
            const question = $this;
            const questionId = question[0].getAttribute("id");
            this.showQuestion(questionId);
            this.par = ($(question).parent().parent());
        });

        onClick(".questionAddedNew", ($this, e) => {
            const question = $this;
            const $par = $(question).parent().parent();
            const parId = $($par)[0].getAttribute("id");
            const parNextId = getParId($par.next());
            this.showQuestionNew(parId, parNextId);
            this.par = ($(question).parent());
        });

        this.sc.$on("getQuestions", () => {
                if (this.firstTimeQuestions) {
                    this.getQuestions();
                    this.firstTimeQuestions = false;
                }
            },
        );

        this.sc.$on("closeQuestionPreview", () => {
                this.showQuestionPreview = false;
            },
        );
    }

    showQuestion(questionId: string) {
        this.markup = {};
        this.markup.json = "No data";
        this.qId = questionId;

        $http<{questionjson: string, points: any, expl: string}>({
            url: "/getQuestionById",
            method: "GET",
            params: {question_id: this.qId},
        }).then((response) => {
            this.markup.json = JSON.parse(response.data.questionjson);
            this.markup.points = response.data.points;
            this.markup.expl = response.data.expl;
            $rootScope.$broadcast("changeQuestionTitle", {questionTitle: this.json.questionTitle});
            $rootScope.$broadcast("setPreviewJson", {
                markup: this.markup,
                questionId: this.qId,
                isLecturer: this.viewctrl.isLecturer,
            });
        }, () => {
            $log.error("There was some error creating question to database.");
        });

        this.viewctrl.lectureId = -1;
        this.viewctrl.inLecture = false;

        this.sc.$on("postLectureId", (event, response) => {
            this.viewctrl.lectureId = response;
        });

        this.sc.$on("postInLecture", (event, response) => {
            this.viewctrl.inLecture = response;
        });

        $rootScope.$broadcast("getLectureId");
        $rootScope.$broadcast("getInLecture");
        this.showQuestionPreview = true;
    }

    showQuestionNew(parId: string, parIdNext: string) {
        this.markup = {};
        this.markup.json = "No data";
        this.questionParId = parId;
        this.questionParIdNext = parIdNext;

        $http<{markup: string}>({
            url: "/getQuestionByParId",
            method: "GET",
            params: {par_id: this.questionParId, doc_id: this.viewctrl.docId},
        }).then((response) => {
            this.markup = response.data.markup;
            $rootScope.$broadcast("changeQuestionTitle", {questionTitle: this.markup.json.questionTitle});
            $rootScope.$broadcast("setPreviewJson", {
                markup: this.markup,
                questionParId: this.questionParId,
                questionParIdNext: this.questionParIdNext,
                isLecturer: this.viewctrl.isLecturer,
            });
        }, () => {
            $log.error("Could not get question.");
        });

        this.viewctrl.lectureId = -1;
        this.viewctrl.inLecture = false;

        this.sc.$on("postLectureId", (event, response) => {
            this.viewctrl.lectureId = response;
        });

        this.sc.$on("postInLecture", (event, response) => {
            this.viewctrl.inLecture = response;
        });

        $rootScope.$broadcast("getLectureId");
        $rootScope.$broadcast("getInLecture");
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
                $http<{markup: {json: {questionTitle}}}>({
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
        this.json = data.markup.json;  // TODO: näistä pitäisi päästä eroon, kaikki markupin kautta!
        this.markup = data.markup;
        // data.markup.qst = true;
        $rootScope.$broadcast("changeQuestionTitle", {questionTitle: this.json.questionTitle});
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

    getQuestionHtml(questions) {
        const questionImage = "/static/images/show-question-icon.png";
        const $questionsDiv = $("<div>", {class: "questions"});

        // TODO: Think better way to get the ID of question.
        for (let i = 0; i < questions.length; i++) {

            const img = new Image(30, 30);
            img.src = questionImage;
            img.title = questions[i].question_title;
            const $questionDiv = $("<span>", {
                class: "questionAdded", html: img, id: questions[i].question_id,
            });
            /*
             var img = new Span(30, 30); // TODO: katso toimiiko
             // img.src = questionImage;
             img.title = questions[i].question_title;
             img.class = "glyphicon-question-sign";
             var $questionDiv = $("<span>", {
             class: 'questionAdded', html: img, id: questions[i].question_id
             });
             */
            $questionsDiv.append($questionDiv);
        }
        return $questionsDiv;
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

    async getQuestions() {
        const response = await $http.get<Array<{par_id: string}>>("/questions/" + this.viewctrl.docId);
        const data = response.data;
        const pars = {};
        const questionCount = data.length;
        for (let i = 0; i < questionCount; i++) {
            const pi = data[i].par_id;
            if (!(pi in pars)) {
                pars[pi] = {questions: []};
            }

            pars[pi].questions.push(data[i]);
        }

        Object.keys(pars).forEach((parId, index) => {
            const $par = getElementByParId(parId);
            $par.find(".questions").remove();
            const $questionsDiv = this.getQuestionHtml(pars[parId].questions);
            $par.append($questionsDiv);
        });
    }

    showQuestions() {
        return (this.viewctrl.item.rights.teacher && (this.viewctrl.lectureMode || this.viewctrl.inLecture)) ||
            ($window.editMode && this.viewctrl.item.rights.editable);
    }
}
