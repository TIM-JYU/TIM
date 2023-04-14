import type {Overwrite} from "type-zoo";
import type {IAnswerWithUsers} from "tim/answer/IAnswer";
import {IUser} from "tim/user/IUser";
import {Moment} from "moment";
import {
    JsonObject,
    JsonProperty,
    JsonSerializer,
} from "typescript-json-serializer";
import type {DrawItem} from "tim/plugin/draw-canvas/draw-canvas.components";

export const jsonSerializer = new JsonSerializer();

export type VelpGroupSelectionType = "show" | "default";

export type IUIFields = {
    show?: boolean;
    selected?: boolean;
    edit?: boolean;
    valid?: boolean;
};

export type IAnnotationCoordinate = {
    par_id: string;
    t?: string;
    offset?: number;
    el_path?: number[];
    node?: number;
    depth?: number;
};

export function isFullCoord(
    c: IAnnotationCoordinate
): c is Required<IAnnotationCoordinate> {
    return (
        c.offset != null &&
        c.el_path != null &&
        c.node != null &&
        c.depth != null &&
        c.t != null
    );
}

export type IAnnotationInterval = {
    start: IAnnotationCoordinate;
    end: IAnnotationCoordinate;
};

export interface IAnnotationComment {
    id: number;
    comment_time: Moment;
    content: string;
    commenter: IUser;
}

@JsonObject()
export class PeerReview {
    @JsonProperty() public id!: number;
    @JsonProperty() public answer_id!: number;
    @JsonProperty() public task_name!: string;
    @JsonProperty() public block_id!: number;
    @JsonProperty() public reviewer!: IUser;
    @JsonProperty() public reviewable!: IUser;
    @JsonProperty() public points?: number;
    @JsonProperty() public comment?: string;
    @JsonProperty() public reviewed?: boolean;
    // @JsonProperty() public start_time!: number;
    // @JsonProperty() public end_time!: number;
}

@JsonObject()
export class Annotation implements IAnnotation {
    @JsonProperty() public id!: number;
    @JsonProperty() public annotator!: IUser;
    @JsonProperty() public answer!: IAnswerWithUsers | null;
    @JsonProperty() public color!: string | null;
    @JsonProperty() public comments!: IAnnotationComment[];
    @JsonProperty() public content!: string;
    @JsonProperty() public coord!: IAnnotationInterval;
    @JsonProperty() public creation_time!: Moment;
    @JsonProperty() public points!: number | null;
    @JsonProperty() public velp!: number;
    @JsonProperty() public visible_to!: number;
    @JsonProperty() public draw_data?: DrawItem[];
    @JsonProperty() public style?: number;

    constructor() {}

    getAnswerId(): number | undefined {
        return this.answer?.id;
    }

    getColor(): string | null {
        return this.color;
    }

    getContent(): string {
        return this.content;
    }

    getVelpId(): number {
        return this.velp;
    }

    getDrawing(): DrawItem[] | undefined {
        return this.draw_data;
    }

    getEditableValues(): Readonly<IAnnotationEditableValues> {
        return {
            color: this.color,
            visible_to: this.visible_to,
            points: this.points,
            style: this.style,
        };
    }
}

export type IAnnotationEditableValues = {
    points: number | null;
    color: string | null;
    visible_to: number;
    style?: number;
};

export class NewAnnotation implements IAnnotation {
    constructor(
        public velp: IVelp,
        public annotator: IUser,
        public answer_id: number | null
    ) {}

    getAnswerId(): number | undefined {
        return this.answer_id ?? undefined;
    }

    getColor(): string | null {
        return this.velp.color;
    }

    getContent(): string {
        return this.velp.content;
    }

    getVelpId(): number {
        return this.velp.id;
    }

    getEditableValues(): Readonly<IAnnotationEditableValues> {
        return {
            points: this.velp.points,
            visible_to: this.velp.visible_to,
            color: this.velp.color,
            style: this.velp.style,
        };
    }
}

export interface IAnnotation {
    annotator: IUser;

    getContent(): string;

    getColor(): string | null;

    getAnswerId(): number | undefined;

    getVelpId(): number;

    getEditableValues(): Readonly<IAnnotationEditableValues>;
}

export type IVelp = {
    content: string;
    default_comment: string;
    points: number | null;
    labels: number[];
    id: number;
    velp_groups: number[];
    visible_to: number;
    color: string | null;
    language_id: string;
    valid_until: string | null;
    style?: number;
};

export type ILabel = {
    content: string;
    id: number;
    language_id?: string;
};

export type IVelpGroup = {
    name: string;
    target_type: number | null;
    id: number;
    default: boolean;
    edit_access?: boolean;
};

export type INewLabel = Overwrite<ILabel, {id: null | number}> & IUIFields;
export type INewVelp = Overwrite<
    IVelp,
    {id: null | number; visible_to: number | null}
> &
    IUIFields;
export type INewVelpGroup = Overwrite<IVelpGroup, {id: null | number}> &
    IUIFields;

export type ILabelUI = ILabel & IUIFields;
export type IVelpUI = IVelp & IUIFields;
export type IVelpGroupUI = IVelpGroup & IUIFields;

export type IVelpGroupCollection = Record<
    string,
    Array<{id: number; selected?: boolean}>
>;
