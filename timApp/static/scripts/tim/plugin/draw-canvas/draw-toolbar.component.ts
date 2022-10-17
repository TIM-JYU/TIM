import type {AfterViewInit, ApplicationRef, DoBootstrap} from "@angular/core";
import {
    Component,
    ElementRef,
    EventEmitter,
    Input,
    NgModule,
    Output,
    ViewChild,
} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import * as t from "io-ts";
import {ButtonsModule} from "ngx-bootstrap/buttons";
import {
    applyOpacity,
    DOCUMENT_BG,
    parseRGBAColor,
    shouldUseDarkText,
} from "tim/util/colorUtils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {TimStorage} from "tim/util/utils";

export interface IDrawVisibleOptions {
    // Interface to define which options should be visible in the drawing toolbar
    enabled?: boolean;
    freeHand?: boolean;
    lineMode?: boolean;
    rectangleMode?: boolean;
    centerEllipseMode?: boolean;
    cornerEllipseMode?: boolean;
    arrowMode?: boolean;
    w?: boolean;
    color?: boolean;
    fill?: boolean;
    opacity?: boolean;
    eraser?: boolean;
}

export enum DrawType {
    Freehand,
    Line,
    Rectangle,
    CenterEllipse,
    CornerEllipse,
    Arrow,
}

const DrawTypeReverseMap: Record<string, DrawType> = Object.entries(
    DrawType
).reduce((acc, [key, value]) => ({...acc, [value]: key}), {});

const DrawTypeCodec = t.keyof({
    [DrawType.Freehand]: null,
    [DrawType.Line]: null,
    [DrawType.Rectangle]: null,
    [DrawType.CenterEllipse]: null,
    [DrawType.CornerEllipse]: null,
    [DrawType.Arrow]: null,
});

export const FillAndWidth = t.type({
    fill: t.boolean,
    w: t.number,
});

export const DrawOptions = t.intersection([
    t.type({
        color: t.string,
        drawType: DrawTypeCodec,
        enabled: t.boolean,
        opacity: t.number,
        eraser: t.boolean,
    }),
    FillAndWidth,
]);

export const DrawSaveOptions = t.type({
    eraseMode: FillAndWidth,
    default: DrawOptions,
});

export interface IFillAndWidth extends t.TypeOf<typeof FillAndWidth> {}

export interface IDrawOptions extends t.TypeOf<typeof DrawOptions> {}

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "draw-toolbar",
    template: `
        <button *ngIf="drawVisibleOptions.enabled"
                type="button"
                class="btn btn-default"
                [(ngModel)]="drawSettings.enabled"
                btnCheckbox
                title="Open drawing toolbar">
            <i class="glyphicon glyphicon-edit"></i>
        </button>
        <div class="drawOptions" [hidden]="!drawSettings.enabled">
            <div class="btn-group drawRadioForm"
                 btnRadioGroup
                 [(ngModel)]="drawTypeStr"
                 (ngModelChange)="onSettingsChanged()">
                <label class="btn btn-default mb-0"
                       title="Free hand drawing"
                       i18n-title
                       btnRadio="Freehand">
                    <i class="glyphicon glyphicon-pencil"></i>
                </label>
                <label class="btn btn-default mb-0"
                       btnRadio="Line"
                       title="Line"
                       i18n-title>
                    <i class="gg-border-style-solid"></i>
                </label>
                <label class="btn btn-default mb-0"
                       btnRadio="Rectangle"
                       title="Rectangle"
                       i18n-title>
                    <i class="gg-shape-square"></i>
                </label>
                <label class="btn btn-default mb-0"
                       btnRadio="CenterEllipse"
                       title="Centered ellipse"
                       i18n-title>
                    <i class="gg-shape-circle-center"></i>
                </label>
                <label class="btn btn-default mb-0"
                       btnRadio="CornerEllipse"
                       title="Ellipse"
                       i18n-title>
                    <i class="gg-shape-circle-corner"></i>
                </label>
                <label class="btn btn-default mb-0"
                       btnRadio="Arrow"
                       title="Arrow"
                       i18n-title>
                    <i class="gg-arrow-top-right"></i>
                </label>
            </div>
            <button class="btn btn-default toggle-option"
                    *ngIf="drawVisibleOptions.fill"
                    [(ngModel)]="drawSettings.fill"
                    btnCheckbox
                    (ngModelChange)="onSettingsChanged()"
                    title="Fill object"
                    i18n-title>
                <i class="gg-color-bucket "></i>
            </button>
            <button class="btn btn-default toggle-option"
                    *ngIf="drawVisibleOptions.eraser"
                    [(ngModel)]="drawSettings.eraser"
                    btnCheckbox
                    (ngModelChange)="onSettingsChanged()"
                    title="Eraser"
                    i18n-title>
                <i class="glyphicon glyphicon-erase"></i>
            </button>
            <span class="sep"></span>
            <label class="text-input"
                   *ngIf="drawVisibleOptions.w"
                   title="Line width"
                   i18n-title>
                <i class="gg-arrows-shrink-h"></i>
                <input class="width form-control"
                       id="freeWidth"
                       size="2"
                       type="number"
                       min="0"
                       [(ngModel)]="drawSettings.w"
                       (ngModelChange)="onSettingsChanged()"/>
            </label>
            <label class="text-input"
                   [ngClass]="{'dim': drawSettings.eraser}"
                   *ngIf="drawVisibleOptions.opacity"
                   title="Opacity"
                   i18n-title>
                <i class="gg-edit-fade"></i>
                <input class="opacity form-control"
                       id="opacity"
                       size="3"
                       type="number"
                       step="0.1" min="0" max="1"
                       [(ngModel)]="drawSettings.opacity"
                       (ngModelChange)="onSettingsChanged()"/>
            </label>
            <span class="sep"></span>
            <div class="color-bar btn-group"
                    *ngIf="drawVisibleOptions.color"
                    [ngClass]="{'dim': drawSettings.eraser}">
                <button class="btn btn-default color-selector"
                        #colorInput
                        title="Color picker"
                        i18n-title>
                    <i class="glyphicon glyphicon-tint picker-icon" [style.color]="selectorIconColor"></i>
                    <input
                            class="form-control"
                            type="color"
                            [(ngModel)]="drawSettings.color"
                            (ngModelChange)="setColor($event)" size="4"/>
                </button>
                <button class="btn btn-default color-preset"
                        title="Select red"
                        i18n-title
                        style="--tim-preset-color: red; --tim-preset-text: white" (click)="setColor('#ff0000')">
                    R
                </button>
                <button class="btn btn-default color-preset"
                        title="Select blue"
                        i18n-title
                        style="--tim-preset-color: blue; --tim-preset-text: white" (click)="setColor('#0000ff')">
                    B
                </button>
                <button class="btn btn-default color-preset"
                        title="Select yellow"
                        i18n-title
                        style="--tim-preset-color: yellow; --tim-preset-text: black" (click)="setColor('#ffff00')">
                    Y
                </button>
                <button class="btn btn-default color-preset"
                        title="Select green"
                        i18n-title
                        style="--tim-preset-color: #0f0; --tim-preset-text: black" (click)="setColor('#00ff00')">
                    G
                </button>
            </div>
            <span class="sep"></span>
            <button class="btn btn-default"
                    *ngIf="undo"
                    (click)="toolbarUndo($event)"
                    title="Undo"
                    i18n-title>
                <i class="glyphicon glyphicon-repeat flip"></i>
            </button>
        </div>
    `,
    styleUrls: ["./draw-toolbar.component.scss"],
})
export class DrawToolbarComponent implements AfterViewInit {
    @Input() drawVisibleOptions: IDrawVisibleOptions = {
        enabled: true,
        freeHand: true,
        lineMode: true,
        rectangleMode: true,
        centerEllipseMode: true,
        cornerEllipseMode: true,
        arrowMode: true,
        w: true,
        color: true,
        fill: true,
        opacity: true,
        eraser: true,
    };

    @Input() public drawSettings: IDrawOptions = {
        enabled: false,
        w: 5,
        opacity: 1,
        color: "red",
        fill: true,
        drawType: DrawType.Freehand,
        eraser: false,
    };
    @Output() drawSettingsChange = new EventEmitter<IDrawOptions>();

    eraserOrNormalOptions: IFillAndWidth = {
        w: 20,
        fill: true,
    };
    eraserState = false;

    @ViewChild("colorInput") colorInput?: ElementRef<HTMLSpanElement>;

    @Input() public undo?: () => void;
    @Input() public optionsStorage?: string;
    selectorIconColor = "black";

    private storage?: TimStorage<t.TypeOf<typeof DrawSaveOptions>>;

    get drawTypeStr(): string {
        return DrawType[this.drawSettings.drawType];
    }

    set drawTypeStr(value: string) {
        this.drawSettings.drawType = DrawTypeReverseMap[value];
    }

    ngOnInit() {
        if (this.optionsStorage) {
            this.storage = new TimStorage(this.optionsStorage, DrawSaveOptions);
            const prevSettings = this.storage.get();
            if (prevSettings) {
                this.loadOptions(prevSettings.default);
                this.eraserOrNormalOptions = prevSettings.eraseMode;
            }
        }
    }

    ngAfterViewInit() {
        this.updateVisuals();
    }

    loadOptions(options: IDrawOptions) {
        this.drawSettings = options;
        this.drawSettingsChange.emit(this.drawSettings);
    }

    onSettingsChanged() {
        if (this.eraserState != this.drawSettings.eraser) {
            const prev = {w: this.drawSettings.w, fill: this.drawSettings.fill};
            this.drawSettings.w = this.eraserOrNormalOptions.w;
            this.drawSettings.fill = this.eraserOrNormalOptions.fill;
            this.eraserOrNormalOptions = prev;
            this.eraserState = this.drawSettings.eraser;
        }
        if (this.storage) {
            // save eraser always as false, fill and width separately for eraser or normal mode
            this.storage.set({
                default: {
                    ...this.drawSettings,
                    eraser: false,
                    ...(this.drawSettings.eraser
                        ? this.eraserOrNormalOptions
                        : {}),
                },
                eraseMode: this.drawSettings.eraser
                    ? {w: this.drawSettings.w, fill: this.drawSettings.fill}
                    : this.eraserOrNormalOptions,
            });
        }
        this.updateVisuals();
    }

    updateVisuals() {
        this.setInputBackgroundColor(this.drawSettings.color);
    }

    public toolbarUndo(e?: Event) {
        e?.preventDefault();
        if (this.undo) {
            this.undo();
        }
    }

    setColor(color: string) {
        this.drawSettings.color = color;
        this.onSettingsChanged();
    }

    setInputBackgroundColor(color: string) {
        if (this.colorInput) {
            this.colorInput.nativeElement.style.backgroundColor = color;
            const prev = window.getComputedStyle(
                this.colorInput.nativeElement
            ).backgroundColor;
            if (prev.includes("rgb(")) {
                this.colorInput.nativeElement.style.backgroundColor = prev
                    .replace("rgb", "rgba")
                    .replace(")", `, ${this.drawSettings.opacity})`);
                const rgb = applyOpacity(
                    {...parseRGBAColor(prev)!, a: this.drawSettings.opacity},
                    DOCUMENT_BG
                );
                this.selectorIconColor = shouldUseDarkText(rgb)
                    ? "black"
                    : "white";
            }
        }
    }
}

@NgModule({
    declarations: [DrawToolbarComponent],
    imports: [
        CommonModule,
        FormsModule,
        ButtonsModule.forRoot(),
        TimUtilityModule,
    ],
    exports: [DrawToolbarComponent],
})
export class DrawToolbarModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
