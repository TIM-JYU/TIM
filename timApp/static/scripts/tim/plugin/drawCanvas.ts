import {Component, ElementRef, Input, OnInit, ViewChild} from "@angular/core";
import {DomSanitizer, SafeResourceUrl} from "@angular/platform-browser";
import {DrawType} from "tim/plugin/drawToolbar";


@Component({
    selector: "draw-canvas",
    template: `
        <div #wrapper>
            <div class="canvascontainer" style="width: 0px; height: 0px; overflow: visible">
                <canvas #drawbase class="drawbase" style="border:1px solid #000000; position: relative;">
                </canvas>
            </div>
            <img #backGround *ngIf="bypassedImage" [src]="bypassedImage" (load)="onImgLoad()">
        </div>
        <draw-toolbar [(enabled)]="drawingAnything" [(drawType)]="drawType"
                      [(color)]="color"
                      [(w)]="w"></draw-toolbar>
    `,
})
export class DrawCanvasComponent implements OnInit {
    @Input() public bgSource = "";
    bypassedImage: SafeResourceUrl = "";
    @ViewChild("drawbase") canvas!: ElementRef<HTMLCanvasElement>;
    @ViewChild("wrapper") wrapper!: ElementRef<HTMLElement>;
    @Input() imgLoadCallback?: (arg0: this) => void;

    drawingAnything = true;
    drawType = DrawType.Freehand;
    color = "red";
    w = 3;

    drawStarted = false;
    clickCallback?: (arg0: this) => void;

    constructor(el: ElementRef<HTMLElement>, private domSanitizer: DomSanitizer) {
    }


    ngOnInit() {
        // console.log(this.backGroundImage);
        this.bypassedImage = this.domSanitizer.bypassSecurityTrustResourceUrl(this.bgSource);
    }

    ngAfterViewInit() {
        this.canvas.nativeElement.addEventListener("mousedown", (event) => {
            this.clickStart(event);
        });
        this.canvas.nativeElement.addEventListener("mousemove", (event) => {
            this.clickMove(event);
        });
        this.canvas.nativeElement.addEventListener("mouseup", (event) => {
            this.clickFinish(event);
        });
    }


    onImgLoad(): void {
        this.canvas.nativeElement.width = this.wrapper.nativeElement.clientWidth;
        this.canvas.nativeElement.height = this.wrapper.nativeElement.clientHeight;
        if (this.imgLoadCallback) {
            this.imgLoadCallback(this);
        }
    }

    public setClickCallback(cb: (arg0: DrawCanvasComponent) => void) {
        this.clickCallback = cb;
    }

    clickStart(e: MouseEvent): void {
        console.log("Started click on canvas at", e.offsetX, e.offsetY);
        this.drawStarted = true;
        if (this.clickCallback) {
            this.clickCallback(this);
        }
    }

    clickMove(e: MouseEvent): void {
        if (this.drawStarted) {
            // console.log("Moved mouse on canvas", e.offsetX, e.offsetY);
        }
    }

    clickFinish(e: MouseEvent): void {
        if (this.drawStarted) {
            console.log("Ended click on canvas");
            this.drawStarted = false;
        }
    }

}

