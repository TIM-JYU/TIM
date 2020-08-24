import {AfterViewInit, ApplicationRef, Component, DoBootstrap, ElementRef, Input, NgModule, OnInit, ViewChild} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";


export interface IDrawVisibleOptions {
    // Interface to define which options should be visible in the drawing toolbar
    // For example imageX does not support ellipses
    enabled?: boolean,
    freeHand?: boolean,
    lineMode?: boolean,
    rectangleMode?: boolean,
    ellipseMode?: boolean,
    w?: boolean,
    color?: boolean,
    fill?: boolean,
    opacity?: boolean,
}

export interface IDrawOptions {
    enabled: boolean,
    drawType: DrawType,
    w: number,
    color: string,
    fill: boolean,
    opacity: number,
}

export enum DrawType {
    Freehand,
    Line,
    Rectangle,
    Ellipse,
}

@Component({
    selector: "draw-toolbar",
    template: `
        <label
                *ngIf="drawVisibleOptions.enabled"><input type="checkbox" name="enabled" value="true"
                                                          [(ngModel)]="drawSettings.enabled"
                                                          (ngModelChange)="updateVisuals()">
            Draw</label>
        <span class="drawOptions" [hidden]="!drawSettings.enabled">
            <span *ngIf="drawVisibleOptions.freeHand">
                <label>
                <input type="radio"
                       name="drawType"
                       [value]="0"
                       [(ngModel)]="drawSettings.drawType">
                FreeHand</label>
            </span>
            <span *ngIf="drawVisibleOptions.lineMode">
                <label>
                <input type="radio"
                       name="drawType"
                       [value]="1"
                       [(ngModel)]="drawSettings.drawType">
                Line</label>
            </span>
            <span *ngIf="drawVisibleOptions.rectangleMode">
                <label>
                <input type="radio"
                       name="drawType"
                       [value]="2"
                       [(ngModel)]="drawSettings.drawType">
                Rectangle</label>
            </span>
            <span *ngIf="drawVisibleOptions.ellipseMode">
                <label>
                <input type="radio"
                       name="drawType"
                       [value]="3"
                       [(ngModel)]="drawSettings.drawType">
                Ellipse</label>
            </span>
            <span *ngIf="drawVisibleOptions.fill">
                <label>
                <input type="checkbox"
                       name="fill"
                       value="true"
                       [(ngModel)]="drawSettings.fill">
                Fill</label>
            </span>
                <span *ngIf="drawVisibleOptions.w">
                    <span class="noWrap" >
                    Width:
                    <input class="width"
                           id="freeWidth"
                           size="2"
                           type="number"
                           [(ngModel)]="drawSettings.w"/>
                    </span>
                </span>
                <span *ngIf="drawVisibleOptions.opacity">
                    <span class="noWrap" >
                    Opacity:
                    <input class="opacity"
                            id="opacity"
                            size="3"
                            type="number"
                            step="0.1" min="0" max="1"
                            [(ngModel)]="drawSettings.opacity"
                            (ngModelChange)="updateVisuals()"/>
                    </span>
                </span>
            <span *ngIf="drawVisibleOptions.color">
                <span class="noWrap">
                    <input #colorInput colorpicker="hex"
                           type="text"
                           [(ngModel)]="drawSettings.color" (ngModelChange)="setColor($event)" size="4"/><span
                        style="background-color: red; display: table-cell; text-align: center; width: 30px;"
                        (click)="setColor('#f00')">R</span><span
                        style="background-color: blue; display: table-cell; text-align: center; width: 30px;"
                        (click)="setColor('#00f')">B</span><span
                        style="background-color: yellow; display: table-cell; text-align: center; width: 30px;"
                        (click)="setColor('#ff0')">Y</span><span
                        style="background-color: #0f0; display: table-cell; text-align: center; width: 30px;"
                        (click)="setColor('#0f0')">G</span>
                    </span>
                </span>
             <a href="" *ngIf="undo" (click)="toolbarUndo($event)">Undo</a>
        </span>
    `,
    styleUrls: ["./draw-toolbar.component.scss"],
})
export class DrawToolbarComponent implements AfterViewInit {
    @Input() drawVisibleOptions: IDrawVisibleOptions = {
        enabled: true,
        freeHand: true,
        lineMode: true,
        rectangleMode: true,
        ellipseMode: true,
        w: true,
        color: true,
        fill: true,
        opacity: true,
    };

    @Input() public drawSettings: IDrawOptions = {
        enabled: false,
        w: 5,
        opacity: 1,
        color: "red",
        fill: true,
        drawType: DrawType.Freehand,
    };

    @ViewChild("colorInput") colorInput?: ElementRef<HTMLInputElement>;

    @Input() public undo?: () => void;

    ngAfterViewInit() {
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
        this.setInputBackgroundColor(color);
        this.drawSettings.color = color;
    }

    setInputBackgroundColor(color: string) {
        if (this.colorInput) {
            this.colorInput.nativeElement.style.backgroundColor = color;
            const prev = window.getComputedStyle(this.colorInput.nativeElement).backgroundColor;
            if (prev.includes("rgb(")) {
                this.colorInput.nativeElement.style.backgroundColor =
                    prev.replace("rgb", "rgba")
                        .replace(")", `, ${this.drawSettings.opacity})`);
            }
        }
    }
}

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

