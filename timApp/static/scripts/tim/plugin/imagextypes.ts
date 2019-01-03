import * as t from "io-ts";
import {GenericPluginMarkup, withDefault} from "./util";

export interface IPoint {
    x: number;
    y: number;
}

const TuplePointR = t.clean(t.union([t.tuple([t.number, t.number]), t.tuple([t.number, t.number, t.number, t.number])]));
const LineSegment = t.clean(t.intersection([
    t.type({lines: t.array(TuplePointR)}),
    t.partial({
        color: t.string,
        w: t.union([t.number, t.string]), // TODO should convert w to number only in database
    }),
]));
export type TuplePoint = t.TypeOf<typeof TuplePointR>;

export interface ILineSegment extends t.TypeOf<typeof LineSegment> {
}

export interface IFakeVideo {
    currentTime: number;
    fakeVideo: true;
}

export type VideoPlayer = IFakeVideo | HTMLVideoElement;
export type MouseOrTouch = MouseEvent | Touch;

// pin begin (non-dot) position = align place + start
// pin end (dot) position = begin + coord
// in YAML, "position" means pin end, so everything else should be computed based on that
export interface IPinPosition {
    align?: PinAlign;
    start?: IPoint;
    coord?: IPoint;
}

export interface ISized {
    width: number;
    height: number;
}

export interface ISizedPartial {
    width: number;
    height?: number;
}

// These two are from type-zoo: https://github.com/pelotom/type-zoo
export type Omit<T, K extends keyof any> = T extends any ? Pick<T, Exclude<keyof T, K>> : never;
export type Overwrite<T, U> = Omit<T, keyof T & keyof U> & U;

type MakeOptional<T, U extends keyof T> = Overwrite<T, { [P in U]?: T[P] }>;
export type RequireExcept<T, U extends keyof T> = MakeOptional<Required<T>, U>;
export type RequiredNonNull<T> = {
    [P in keyof T]-?: NonNullable<T[P]>;
};
export type RequireNonNullExcept<T, U extends keyof T> = MakeOptional<RequiredNonNull<T>, U>;

const ObjectType = t.clean(t.keyof({
    ellipse: null,
    img: null,
    rectangle: null,
    textbox: null,
    vector: null,
}));

const PinAlignment = t.clean(t.keyof({
    center: null,
    east: null,
    north: null,
    northeast: null,
    northwest: null,
    south: null,
    southeast: null,
    southwest: null,
    west: null,
}));

export type PinAlign = t.TypeOf<typeof PinAlignment>;

const ImgProps = t.clean(t.type({
    src: t.string,
    textbox: withDefault(t.boolean, true),
}));

export const ValidCoord = t.clean(t.tuple([t.number, t.number]));

export const SingleSize = t.clean(t.tuple([t.number]));

const Size = t.clean(t.union([t.null, ValidCoord, SingleSize]));

export type SizeT = t.TypeOf<typeof Size>;

const TextboxProps = t.clean(t.intersection([
    t.partial({
        a: t.number,
        position: ValidCoord,
        size: Size,
        borderColor: t.union([t.null, t.string]),
        borderWidth: t.union([t.null, t.number]),
        cornerradius: t.number,
        fillColor: t.string,
        font: t.union([t.null, t.string]),
        text: t.union([t.null, t.string]),
        textColor: t.union([t.null, t.string]),
    })]));

const VectorProps = t.clean(t.partial({
    arrowheadlength: t.number,
    arrowheadwidth: t.number,
    color: t.union([t.null, t.string]),
}));

const CommonProps = t.clean(t.partial({
    a: t.number,
    color: t.union([t.null, t.string]),
    id: t.string,
    position: ValidCoord,
    size: Size,
    type: t.union([t.null, ObjectType]),

    imgproperties: t.union([t.null, ImgProps]),
    textboxproperties: t.union([t.null, TextboxProps]),
    vectorproperties: t.union([t.null, VectorProps]),
}));

const PinProps = t.clean(t.partial({
    color: t.union([t.null, t.string]),
    dotRadius: t.union([t.null, t.number]),
    length: t.union([t.null, t.number]),
    linewidth: t.union([t.null, t.number]),
    position: t.partial({
        align: PinAlignment,
        coord: ValidCoord,
        start: ValidCoord,
    }),
    visible: t.boolean,
}));

export interface PinPropsT extends t.TypeOf<typeof PinProps> {
}

const FixedObjectProps = CommonProps;

const TargetProps = t.clean(t.intersection([
    CommonProps,
    t.partial({
        dropColor: t.string,
        points: t.dictionary(t.string, t.number),
        snap: t.boolean,
        snapColor: t.string,
        snapOffset: ValidCoord,
    })]));

const Lock = t.clean(t.keyof({
    x: null,
    y: null,
}));

const DragObjectProps = t.clean(t.intersection([
    t.partial({
        lock: Lock,
        pin: PinProps,
        xlimits: ValidCoord,
        ylimits: ValidCoord,
    }),
    FixedObjectProps,
]));

export interface CommonPropsT extends t.TypeOf<typeof CommonProps> {
}

export interface DragObjectPropsT extends t.TypeOf<typeof DragObjectProps> {
}

export interface TargetPropsT extends t.TypeOf<typeof TargetProps> {
}

export interface FixedObjectPropsT extends t.TypeOf<typeof FixedObjectProps> {
}

export interface TextboxPropsT extends t.TypeOf<typeof TextboxProps> {
}

export interface ImgPropsT extends t.TypeOf<typeof ImgProps> {
}

export interface VectorPropsT extends t.TypeOf<typeof VectorProps> {
}

export type ObjectTypeT = t.TypeOf<typeof ObjectType>;

const BackgroundProps = t.clean(t.intersection([
    t.partial({
        a: t.number,
        size: Size,
    }),
    t.type({
        src: t.string,
    }),
]));

const DefaultProps = t.clean(t.intersection([DragObjectProps, TargetProps]));

export interface DefaultPropsT extends t.TypeOf<typeof DefaultProps> {
}

export const ImageXMarkup = t.clean(t.intersection([
    t.partial({
        background: BackgroundProps,
        buttonPlay: t.string,
        buttonRevert: t.string,
        defaults: DefaultProps,
        fixedobjects: t.readonlyArray(FixedObjectProps),
        followid: t.string,
        freeHand: t.union([t.boolean, t.literal("use")]),
        freeHandColor: t.string,
        freeHandShortCut: t.boolean,
        freeHandVisible: t.boolean,
        freeHandWidth: t.number,
        objects: t.readonlyArray(DragObjectProps),
        targets: t.readonlyArray(TargetProps),
    }),
    GenericPluginMarkup,
    t.type({
        analyzeDot: withDefault(t.boolean, false),
        autosave: withDefault(t.boolean, false),
        autoupdate: withDefault(t.number, 500),
        canvasheight: withDefault(t.number, 600),
        canvaswidth: withDefault(t.number, 800),
        cols: withDefault(t.number, 20),
        dotVisibleTime: withDefault(t.number, 1),
        drawLast: withDefault(t.boolean, false),
        emotion: withDefault(t.boolean, false),
        extraGrabAreaHeight: withDefault(t.number, 30),
        finalanswer: withDefault(t.boolean, false),
        freeHandColor: withDefault(t.string, "red"),
        freeHandLine: withDefault(t.boolean, false),
        freeHandLineVisible: withDefault(t.boolean, true),
        freeHandShortCuts: withDefault(t.boolean, true),
        freeHandToolbar: withDefault(t.boolean, true),
        freeHandWidth: withDefault(t.number, 2),
        showTimes: withDefault(t.boolean, false),
        showVideoTime: withDefault(t.boolean, true),
    }),
]));

export const RightAnswer = t.clean(t.type({
    id: t.string,
    position: ValidCoord,
}));

export interface RightAnswerT extends t.TypeOf<typeof RightAnswer> {
}

export const ImageXAll = t.clean(t.intersection([
    t.partial({
        preview: t.boolean,
        state: t.union([
            t.null,
            t.partial({
                freeHandData: t.array(LineSegment),
                userAnswer: t.partial({
                    drags: t.array(t.type({
                        did: t.string,
                        id: t.string,
                        position: ValidCoord,
                    })),
                }),
            })]),
    }),
    t.type({
        info: t.union([t.null, t.type({earlier_answers: t.Integer})]),
        markup: ImageXMarkup,
    }),
]));

// By default, all properties are required when passing data to ObjBase constructors.
// Here we make some properties optional.
export type OptionalCommonPropNames =
    | "id"
    | "size"
    | "imgproperties"
    | "textboxproperties"
    | "vectorproperties"
    | "color";
export type OptionalFixedObjPropNames = OptionalCommonPropNames;
export type OptionalDragObjectPropNames = OptionalFixedObjPropNames | "xlimits" | "ylimits" | "lock";
export type OptionalTargetPropNames = "points" | OptionalFixedObjPropNames;
export type OptionalPropNames = OptionalFixedObjPropNames | OptionalDragObjectPropNames | OptionalTargetPropNames;
