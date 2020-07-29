import {
    ApplicationRef,
    Component,
    DoBootstrap,
    EventEmitter,
    Input,
    NgModule,
    OnInit,
    Output,
    StaticProvider,
} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {CommonModule} from "@angular/common";


export interface IDrawVisibleOptions {
    // Interface to define which options should be visible in the drawing toolbar
    // For example imageX does not support circles
    enabled?: boolean,
    freeHand?: boolean,
    lineMode?: boolean,
    rectangleMode?: boolean,
    circleMode?: boolean,
    w?: boolean,
    color?: boolean,
    fill?: boolean,
    opacity?: boolean,
}

export enum DrawType {
    Freehand,
    Line,
    Rectangle,
    Circle,
}

@Component({
    selector: "draw-toolbar",
    template: `
        <!--        <span *ngIf="drawSettings">-->
        <label
                *ngIf="drawVisibleOptions.enabled">Draw <input type="checkbox" name="enabled" value="true"
                [(ngModel)]="enabled" (ngModelChange)="enabledChange.emit($event)" ></label> <span><span
                [hidden]="!enabled">
            <span *ngIf="drawVisibleOptions.freeHand">
                <label>FreeHand
                <input type="radio"
                       name="drawType"
                       value="0"
                       [(ngModel)]="drawType" (ngModelChange)="drawTypeChange.emit($event)"></label>
            </span>
            <span *ngIf="drawVisibleOptions.lineMode">
                <label>Line
                <input type="radio"
                       name="drawType"
                       value="1"
                       [(ngModel)]="drawType" (ngModelChange)="drawTypeChange.emit($event)"></label>
            </span>
            <span *ngIf="drawVisibleOptions.rectangleMode">
                <label>Rectangle
                <input type="radio"
                       name="drawType"
                       value="2"
                       [(ngModel)]="drawType" (ngModelChange)="drawTypeChange.emit($event)"></label>
            </span>
            <span *ngIf="drawVisibleOptions.circleMode">
                <label>Circle
                <input type="radio"
                       name="drawType"
                       value="3"
                       [(ngModel)]="drawType" (ngModelChange)="drawTypeChange.emit($event)"></label>
            </span>
            <span *ngIf="drawVisibleOptions.fill">
                <label>Fill
                <input type="checkbox"
                       name="fill"
                       value="true"
                       [(ngModel)]="fill" (ngModelChange)="fillChange.emit($event)"></label>
            </span>
            <span>
                <span *ngIf="drawVisibleOptions.w">
                    Width:
                    <input [hidden]="!true"
                           id="freeWidth"
                           size="1"
                           style="width: 2em"
                           type="number"
                           [(ngModel)]="w" (ngModelChange)="wChange.emit($event)"/>
                </span>
                <span *ngIf="drawVisibleOptions.opacity">
                    Opacity:
                    <input
                           id="opacity"
                           size="3"
                           type="number"
                           step="0.1" min="0" max="1"
                           [(ngModel)]="opacity" (ngModelChange)="opacityChange.emit($event)"/>
                </span>
            <span *ngIf="setFColor">
            <input colorpicker="hex"
                   *ngIf="drawVisibleOptions.color"
                   type="text"
                   [ngStyle]="{'background-color': color}"
                   [(ngModel)]="color" (ngModelChange)="colorChange.emit($event)" size="4"/> <span
                    style="background-color: red; display: table-cell; text-align: center; width: 30px;"
                    (click)="setFColor('#f00')">R</span><span
                    style="background-color: blue; display: table-cell; text-align: center; width: 30px;"
                    (click)="setFColor('#00f')">B</span><span
                    style="background-color: yellow; display: table-cell; text-align: center; width: 30px;"
                    (click)="setFColor('#ff0')">Y</span><span
                    style="background-color: #0f0; display: table-cell; text-align: center; width: 30px;"
                    (click)="setFColor('#0f0')">G</span>
                </span>
            <a href="" *ngIf="undo" (click)="toolbarUndo($event)">Undo</a>
        </span></span></span>
        <!--            </span> -->
    `,
})
export class DrawToolbarComponent implements OnInit {
    @Input() drawVisibleOptions: IDrawVisibleOptions = {
        enabled: true,
        freeHand: true,
        lineMode: true,
        rectangleMode: true,
        circleMode: true,
        w: true,
        color: true,
        fill: true,
        opacity: true,
    };

    // TODO: add these to single object
    @Input() public enabled = false;
    @Output() enabledChange: EventEmitter<boolean> = new EventEmitter();

    @Input() public w = 5;
    @Output() wChange: EventEmitter<number> = new EventEmitter();

    @Input() public opacity = 1;
    @Output() opacityChange: EventEmitter<number> = new EventEmitter();

    @Input() public color = "red";
    @Output() colorChange: EventEmitter<string> = new EventEmitter();

    @Input() public fill = true;
    @Output() fillChange: EventEmitter<boolean> = new EventEmitter();

    @Input() public drawType?: DrawType;
    @Output() drawTypeChange: EventEmitter<DrawType> = new EventEmitter();

    @Input() public setFColor?: (color: string) => void;
    @Input() public undo?: () => void;

    @Input() public rectangleMode?: boolean;
    @Output() rectangleModeChange: EventEmitter<boolean> = new EventEmitter();

    ngOnInit() {
    }

    public toolbarUndo(e?: Event) {
        e?.preventDefault();
        if (this.undo) {
            this.undo();
        }
    }
}

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        DrawToolbarComponent,
    ], imports: [
        CommonModule,
        FormsModule,
    ],
    exports: [DrawToolbarComponent],
})
export class DrawToolbarModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
    }
}

const bootstrapFn = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(DrawToolbarModule);
};

const angularJsModule = createDowngradedModule(bootstrapFn);
doDowngrade(angularJsModule, "drawToolbar", DrawToolbarComponent);
export const moduleDefs = [angularJsModule];
