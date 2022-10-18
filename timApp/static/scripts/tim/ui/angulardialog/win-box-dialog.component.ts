import type {OnInit} from "@angular/core";
import {Component, ElementRef, ViewChild} from "@angular/core";
import WinBox from "winbox/src/js/winbox";

@Component({
    selector: "tim-win-box-dialog",
    template: `
    <div #testContent>
      win-box-dialog works!
        <input type="text" [(ngModel)]="testInput">
        <span>Content: {{testInput}}</span>
    </div>
    <button class="timButton" (click)="openWindow()">Open</button>
  `,
    styleUrls: ["./win-box-dialog.component.scss"],
})
export class WinBoxDialogComponent implements OnInit {
    @ViewChild("testContent") testContent!: ElementRef<HTMLDivElement>;
    testInput!: string;

    constructor(private el: ElementRef<HTMLElement>) {}

    ngOnInit(): void {}

    openWindow() {
        const wb = new WinBox("wew", {
            mount: this.testContent.nativeElement,
            root: this.el.nativeElement,
            autosize: true,
            onmaximize: function () {
                this.removeClass("wb-absolute");
                this.resize("100%", "100%", true).move(0, 0, true);
            },
            onrestore: function () {
                this.addClass("wb-absolute");
            },
        });
        wb.addClass("wb-absolute");
        console.log(wb);
        wb.top = -10000;
        wb.bottom = -10000;
    }
}
