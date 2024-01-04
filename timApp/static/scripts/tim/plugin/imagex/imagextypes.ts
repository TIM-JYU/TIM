import * as t from "io-ts";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";

const Point = t.type({x: t.number, y: t.number});

const TuplePointR = t.union([
    t.tuple([t.number, t.number]),
    t.tuple([t.number, t.number, t.number, t.number]),
]);
const LineSegment = t.intersection([
    t.type({lines: t.array(TuplePointR)}),
    t.partial({
        color: t.string,
        w: t.union([t.number, t.string]), // TODO should convert w to number only in database
        opacity: t.number,
        eraser: t.boolean,
    }),
]);

const Line = t.intersection([
    t.type({start: Point, end: Point, w: t.number}),
    t.partial({
        color: t.string,
        opacity: t.number,
        eraser: t.boolean,
    }),
]);

const RectangleOrEllipse = t.intersection([
    t.type({x: t.number, y: t.number, w: t.number, h: t.number}),
    t.partial({
        color: t.string,
        fillColor: t.string,
        lineWidth: t.number,
        opacity: t.number,
        eraser: t.boolean,
    }),
]);

const DrawData = t.union([
    t.type({type: t.literal("freehand"), drawData: LineSegment}),
    t.type({type: t.literal("rectangle"), drawData: RectangleOrEllipse}),
    t.type({type: t.literal("ellipse"), drawData: RectangleOrEllipse}),
    t.type({type: t.literal("arrow"), drawData: Line}),
]);

export type IPoint = t.TypeOf<typeof Point>;

export type TuplePoint = t.TypeOf<typeof TuplePointR>;

export type ILine = t.TypeOf<typeof Line>;

export type ILineSegment = t.TypeOf<typeof LineSegment>;

export type IRectangleOrEllipse = t.TypeOf<typeof RectangleOrEllipse>;

export type IDrawData = t.TypeOf<typeof DrawData>;

export interface IFakeVideo {
    currentTime: number;
    fakeVideo: true;
}

export type VideoPlayer = IFakeVideo | HTMLVideoElement;

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
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type Omit<T, K extends keyof any> = T extends any
    ? Pick<T, Exclude<keyof T, K>>
    : never;
export type Overwrite<T, U> = Omit<T, keyof T & keyof U> & U;

type MakeOptional<T, U extends keyof T> = Overwrite<T, {[P in U]?: T[P]}>;
export type RequireExcept<T, U extends keyof T> = MakeOptional<Required<T>, U>;
export type RequiredNonNull<T> = {
    [P in keyof T]-?: NonNullable<T[P]>;
};
export type RequireNonNullExcept<T, U extends keyof T> = MakeOptional<
    RequiredNonNull<T>,
    U
>;

const ObjectType = t.keyof({
    ellipse: null,
    img: null,
    rectangle: null,
    textbox: null,
    vector: null,
});

const PinAlignment = t.keyof({
    center: null,
    east: null,
    north: null,
    northeast: null,
    northwest: null,
    south: null,
    southeast: null,
    southwest: null,
    west: null,
});

export type PinAlign = t.TypeOf<typeof PinAlignment>;

const ImgProps = t.intersection([
    t.partial({
        textbox: t.boolean,
    }),
    t.type({
        src: t.string,
    }),
]);

export const ValidCoord = t.tuple([t.number, t.number]);

export const SingleSize = t.tuple([t.number]);

const Size = t.union([t.null, ValidCoord, SingleSize]);

export type SizeT = t.TypeOf<typeof Size>;

const Angle = nullable(t.number);

const TextboxProps = t.intersection([
    t.partial({}),
    t.partial({
        a: Angle,
        position: ValidCoord,
        size: Size,
        borderColor: nullable(t.string),
        borderWidth: nullable(t.number),
        cornerradius: t.number,
        fillColor: t.string,
        font: nullable(t.string),
        text: nullable(t.string),
        textColor: nullable(t.string),
    }),
]);

const VectorProps = t.partial({
    arrowheadlength: t.number,
    arrowheadwidth: t.number,
    color: nullable(t.string),
});

const CommonProps = t.partial({
    a: Angle,
    borderWidth: nullable(t.number),
    color: nullable(t.string),
    id: t.string,
    correctColor: nullable(t.string),
    incorrectColor: nullable(t.string),
    position: ValidCoord,
    size: Size,
    type: nullable(ObjectType),

    imgproperties: nullable(ImgProps),
    textboxproperties: nullable(TextboxProps),
    vectorproperties: nullable(VectorProps),
});

const PinProps = t.partial({
    color: nullable(t.string),
    dotRadius: nullable(t.number),
    length: nullable(t.number),
    linewidth: nullable(t.number),
    position: t.partial({
        align: PinAlignment,
        coord: ValidCoord,
        start: ValidCoord,
    }),
    visible: t.boolean,
});

export interface PinPropsT extends t.TypeOf<typeof PinProps> {}

const FixedObjectProps = CommonProps;

const TargetProps = t.intersection([
    CommonProps,
    t.partial({
        dropColor: t.string,
        points: t.dictionary(t.string, t.number),
        snap: t.boolean,
        snapColor: t.string,
        snapOffset: ValidCoord,
    }),
]);

const Lock = t.keyof({
    x: null,
    y: null,
});

const DragObjectProps = t.intersection([
    t.partial({
        lock: Lock,
        pin: PinProps,
        xlimits: ValidCoord,
        ylimits: ValidCoord,
    }),
    FixedObjectProps,
]);

export interface CommonPropsT extends t.TypeOf<typeof CommonProps> {}

export interface DragObjectPropsT extends t.TypeOf<typeof DragObjectProps> {}

export interface TargetPropsT extends t.TypeOf<typeof TargetProps> {}

export interface FixedObjectPropsT extends t.TypeOf<typeof FixedObjectProps> {}

export interface TextboxPropsT extends t.TypeOf<typeof TextboxProps> {}

export interface ImgPropsT extends t.TypeOf<typeof ImgProps> {}

export interface VectorPropsT extends t.TypeOf<typeof VectorProps> {}

export type ObjectTypeT = t.TypeOf<typeof ObjectType>;

const BackgroundProps = t.intersection([
    t.partial({
        a: Angle,
        size: Size,
        position: ValidCoord,
    }),
    t.type({
        src: t.string,
    }),
]);

const DefaultProps = t.intersection([DragObjectProps, TargetProps]);

export interface DefaultPropsT extends t.TypeOf<typeof DefaultProps> {}

export const ImageXMarkup = t.intersection([
    t.partial({
        background: BackgroundProps,
        buttonPlay: t.string,
        buttonRevert: t.string,
        defaults: DefaultProps,
        fixedobjects: nullable(t.readonlyArray(FixedObjectProps)),
        followid: t.string,
        freeHand: t.union([t.boolean, t.literal("use")]),
        freeHandColor: t.string,
        freeHandShortCut: t.boolean,
        freeHandVisible: t.boolean,
        freeHandWidth: t.number,
        objects: nullable(t.readonlyArray(DragObjectProps)),
        targets: nullable(t.readonlyArray(TargetProps)),
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
]);

export const RightAnswer = t.intersection([
    t.type({
        id: t.string,
        position: ValidCoord,
    }),
    t.partial({lineColor: t.string}),
]);

export interface RightAnswerT extends t.TypeOf<typeof RightAnswer> {}

export const ImageXAll = t.intersection([
    t.partial({
        state: nullable(
            t.partial({
                freeHandData: t.array(LineSegment),
                drawings: t.array(DrawData),
                userAnswer: t.partial({
                    drags: t.array(
                        t.type({
                            did: t.string,
                            id: t.string,
                            position: ValidCoord,
                        })
                    ),
                }),
            })
        ),
    }),
    t.type({
        info: Info,
        markup: ImageXMarkup,
        preview: t.boolean,
    }),
]);

// By default, all properties are required when passing data to ObjBase constructors.
// Here we make some properties optional.
export type OptionalCommonPropNames =
    | "id"
    | "size"
    | "imgproperties"
    | "textboxproperties"
    | "vectorproperties"
    | "color"
    | "borderWidth"
    | "correctColor"
    | "incorrectColor";
export type OptionalFixedObjPropNames = OptionalCommonPropNames;
export type OptionalDragObjectPropNames =
    | OptionalFixedObjPropNames
    | "xlimits"
    | "ylimits"
    | "lock";
export type OptionalTargetPropNames = "points" | OptionalFixedObjPropNames;
export type OptionalPropNames =
    | OptionalFixedObjPropNames
    | OptionalDragObjectPropNames
    | OptionalTargetPropNames;
