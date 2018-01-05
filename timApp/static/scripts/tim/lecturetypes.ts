import moment from "moment";

export interface IExplCollection {
    [idString: string]: string;
}

export interface IAskedJsonJsonBase {
    userpoints?: string;
    points?: string;
    json: IAskedJsonJsonJson;
    expl?: IExplCollection;
}

export interface IAskedJsonJson extends IAskedJsonJsonBase {
    button?: string;
    buttonText?: string;
    taskId: string;
    lazy?: boolean;
    qst?: boolean;
}

export type QuestionType =
    "checkbox-vertical"
    | "matrix"
    | "radio-vertical"
    | "true-false"
    | "textarea"
    | "likert"
    | "";

export type MatrixType = "textArea" | "" | "radiobutton-horizontal" | "radiobutton-vertical" | "checkbox";

export type AnswerFieldType = "radio" | "checkbox" | "matrix" | "text"; // TODO matrix seems wrong

export interface IHeader {
    text: string;
    type: string;
    id: number;
}

export interface IColumn {
    text: string;
    type: string;
    answerFieldType: AnswerFieldType; // TODO useless field?
}

export interface IRow extends IHeader {
    columns: IColumn[];
}

export interface IUnprocessedHeaders {
    headers: string | string[];
    rows: string | string[];
    answerFieldType: AnswerFieldType;
    questionType: QuestionType;
}

export interface IProcessedHeaders {
    headers: IHeader[];
    rows: IRow[];
    answerFieldType: AnswerFieldType;
    questionType: QuestionType;
}

export interface IUnprocessedHeadersCompat extends IUnprocessedHeaders {
    data?: IUnprocessedHeaders;
}

export interface IAskedJsonJsonJson extends IUnprocessedHeadersCompat {
    timeLimit?: number;
    questionTitle: string;
    questionText: string;
    matrixType: MatrixType;
}

export interface IQuestionUI {
    endTimeSelected: boolean;
    timeLimitFields: {hours: number, minutes: number, seconds: number};
}

export interface IAskedJson {
    hash: string;
    json: string; // IAskedJsonJson as a string
}

export interface IAskedQuestion extends IAskedJson {
    asked_id: number;
    lecture_id: number;
    doc_id: number;
    par_id: string;
    asked_time: string; // TODO
    points: string;
    expl: string;
}

export interface ILectureMessage {
    sender: string;
    time: string;
    message: string;
}

export interface ILectureFormParams extends ILecture {
}

export interface ILectureOptions {
    max_students: number;
    poll_interval: number;
    poll_interval_t: number;
    long_poll: boolean;
    long_poll_t: boolean;
}

export interface ILecture {
    doc_id: number;
    lecture_id: number;
    lecture_code: string;
    start_time: moment.Moment;
    end_time: moment.Moment;
    password: string;
    options: ILectureOptions;
    is_full: boolean;
}

export interface IQuestionAnswer {
    user_id: number;
    question_id: number;
    points: number;
    answer: string;
}

export interface IMessage {
    time: string;
    sender: string;
    message: string;
}

export interface ILecturePerson {
    name: string;
    active: boolean;
    user_id: number;
}

export function isLectureListResponse(response: any): response is ILectureListResponse {
    return response.lectures !== undefined && response.futureLectures !== undefined;
}

export interface ILectureListResponse2 {
    currentLectures: ILecture[];
    futureLectures: ILecture[];
    pastLectures: ILecture[];
}

export interface ILectureListResponse {
    isLecturer: boolean;
    lectures: ILecture[];
    futureLectures: ILecture[];
}

export interface ILectureResponse {
    isInLecture: boolean;
    isLecturer: boolean;
    lecture: ILecture;
    students: ILecturePerson[];
    lecturers: ILecturePerson[];
    useWall: boolean;
    useQuestions: boolean;
    correctPassword?: boolean;
}

export interface ILectureSettings {
    inLecture: boolean;
    lectureMode: boolean;
    useAnswers: boolean;
    useQuestions: boolean;
    useWall: boolean;
}

export function hasLectureEnded(lecture: ILecture) {
    return lecture.end_time < moment();
}

export interface IUniqueParId {
    parId: string;
    docId: number;
}
