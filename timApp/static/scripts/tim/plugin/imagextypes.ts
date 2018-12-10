import * as t from "io-ts";
import {GenericPluginMarkup, withDefault} from "./util";

export interface IPoint {
    x: number;
    y: number;
}

const TuplePointR = t.union([t.tuple([t.number, t.number]), t.tuple([t.number, t.number, t.number, t.number])]);
const LineSegment = t.intersection([
    t.type({lines: t.array(TuplePointR)}),
    t.partial({
        color: t.string,
        w: t.number,
    }),
]);
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

// These two are from type-zoo: https://github.com/pelotom/type-zoo
export type Omit<T, K extends keyof any> = T extends any ? Pick<T, Exclude<keyof T, K>> : never;
export type Overwrite<T, U> = Omit<T, keyof T & keyof U> & U;

type MakeOptional<T, U extends keyof T> = Overwrite<T, { [P in U]?: T[P] }>;
export type RequireExcept<T, U extends keyof T> = MakeOptional<Required<T>, U>;
/**
 * Makes all properties required except size.
 */
type RequireExceptSize<T extends CommonPropsT> = RequireExcept<T, "size" | "id">;
export type Id<T> = T;
export type C<T> = Readonly<RequireExceptSize<T>>;

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

type PinAlign = t.TypeOf<typeof PinAlignment>;

const ImgProps = t.type({
    src: t.string,
    textbox: withDefault(t.boolean, true),
});

const ValidCoord = t.tuple([t.number, t.number]);

const TextboxProps = t.intersection([
    // CommonProps,
    t.partial({
        a: t.number,
        // color: t.string,
        // id: t.string,
        position: ValidCoord,
        size: ValidCoord,
        // type: ObjectType,

        borderColor: t.string,
        borderWidth: t.number,
        cornerradius: t.number,
        fillColor: t.string,
        font: t.string,
        text: t.string,
        textColor: t.string,
    })]);

const VectorProps = t.partial({
    arrowheadlength: t.number,
    arrowheadwidth: t.number,
    color: t.string,
});

const CommonProps = t.partial({
    a: t.number,
    color: t.string,
    id: t.string,
    position: ValidCoord,
    size: ValidCoord,
    type: ObjectType,

    imgproperties: ImgProps,
    textboxproperties: TextboxProps,
    vectorproperties: VectorProps,
});

const PinProps = t.partial({
    color: t.string,
    dotRadius: t.number,
    length: t.number,
    linewidth: t.number,
    position: t.partial({
        align: PinAlignment,
        coord: ValidCoord,
        start: ValidCoord,
    }),
    visible: t.boolean,
});

export type PinPropsT = t.TypeOf<typeof PinProps>;

const FixedObjectProps = t.intersection([
    CommonProps,
    t.partial({})]);

const TargetProps = t.intersection([
    CommonProps,
    t.partial({
        dropColor: t.string,
        points: t.dictionary(t.string, t.number),
        snap: t.boolean,
        snapColor: t.string,
        snapOffset: ValidCoord,
    })]);

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

export type CommonPropsT = t.TypeOf<typeof CommonProps>;
export type DragObjectPropsT = t.TypeOf<typeof DragObjectProps>;
export type TargetPropsT = t.TypeOf<typeof TargetProps>;
export type FixedObjectPropsT = t.TypeOf<typeof FixedObjectProps>;
export type TextboxPropsT = t.TypeOf<typeof TextboxProps>;
export type ImgPropsT = t.TypeOf<typeof ImgProps>;
export type VectorPropsT = t.TypeOf<typeof VectorProps>;
export type ObjectTypeT = t.TypeOf<typeof ObjectType>;

const BackgroundProps = t.intersection([
    t.partial({
        a: t.number,
    }),
    t.type({
        src: t.string,
    }),
]);

const DefaultProps = t.intersection([DragObjectProps, TargetProps]);

export type DefaultPropsT = t.TypeOf<typeof DefaultProps>;

export const ImageXMarkup = t.intersection([
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
        max_tries: t.number,
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
]);

export const ImageXAll = t.intersection([
    t.partial({
        freeHandData: t.array(LineSegment),
        preview: t.boolean,
        tries: t.number,
    }),
    t.type({
        markup: ImageXMarkup,
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
    | "color";
export type OptionalFixedObjPropNames = OptionalCommonPropNames;
export type OptionalDragObjectPropNames = OptionalFixedObjPropNames | "xlimits" | "ylimits" | "lock";
export type OptionalTargetPropNames = "points" | OptionalFixedObjPropNames;
export type OptionalPropNames = OptionalFixedObjPropNames | OptionalDragObjectPropNames | OptionalTargetPropNames;
