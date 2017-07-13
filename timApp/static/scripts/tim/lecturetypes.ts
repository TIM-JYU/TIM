import moment from "moment";

export interface IExplCollection {
    [idString: string]: string;
}

export interface IAskedJsonJson {
    taskId: string;
    lazy?: boolean;
    points?: string;
    json: IAskedJsonJsonJson;
    expl: IExplCollection;
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

export interface IAskedJsonJsonJson {
    timeLimit: number;
    questionTitle: string;
    questionText: string;
    matrixType: "textArea" | "" | "radiobutton-horizontal" | "radiobutton-vertical" | "checkbox";
    answerFieldType: string;
    questionType: QuestionType;
    headers: string[];
    rows: string[];
}

export interface IQuestionUI {
    endTimeSelected: boolean;
    showPreview: boolean;
    timeLimitFields: {hours: number, minutes: number, seconds: number};
}

export interface IAskedJson {
    hash: string;
    json: string; // IAskedJsonJson
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

export interface ILecture {
    doc_id: number;
    lecture_id: number;
    lecture_code: string;
    start_time: moment.Moment;
    end_time: moment.Moment;
    password: string;
    max_students: number;
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
    useNotPollingDialog: boolean;
    useQuestions: boolean;
    useWall: boolean;
}
