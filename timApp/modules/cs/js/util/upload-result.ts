/* eslint no-underscore-dangle: ["error", { "allow": ["type_", "src_"] }] */
import {Component, Input} from "@angular/core";
import {SafeResourceUrl, DomSanitizer} from "@angular/platform-browser";

@Component({
    selector: "cs-upload-result",
    template: `
    <ng-container *ngIf="src_ && type_">
        <p *ngIf="type_ != 'unknown'" class="smalllink">
            <a [href]="src_" [title]="type_">{{name}}</a>
        </p>
        <p *ngIf="type_ == 'unknown'">
            Ladattu:
            <a [href]="src_" [title]="type_">{{name}}</a>
        </p>
        <ng-container [ngSwitch]="type_">
            <img *ngSwitchCase="'image'" [src]="src_"/>
            <video *ngSwitchCase="'video'" [src]="src_" controls></video>
            <audio *ngSwitchCase="'audio'" [src]="src_" controls></audio>
            <div *ngSwitchCase="'text'" style="overflow: auto; -webkit-overflow-scrolling: touch; max-height:900px; -webkit-box-pack: center; -webkit-box-align: center; display: -webkit-box;">
                <iframe [src]="src_" width="800" sandbox></iframe>
            </div>
            <div *ngSwitchCase="'other'" style="overflow: auto; -webkit-overflow-scrolling: touch; max-height:1200px; -webkit-box-pack: center; -webkit-box-align: center; display: -webkit-box;">
                <iframe [src]="src_" width="800" height="900"></iframe>
            </div>
        </ng-container>
    </ng-container>`,
})
export class UploadResultComponent {
    // TODO: test
    static specializedTypes = ["image", "video", "audio", "text"];
    static otherTypes = ["pdf", "xml"];

    type_?: string;
    src_?: SafeResourceUrl;
    name?: string;

    constructor(private sanitizer: DomSanitizer) {}

    @Input()
    set src(src: string) {
        this.src_ = this.sanitizer.bypassSecurityTrustResourceUrl(src);

        const s = src.split("\\").pop();
        this.name = s ? s.split("/").pop() : "";
    }

    @Input()
    set type(type: string) {
        type = type.toLowerCase();
        for (const t of UploadResultComponent.specializedTypes) {
            if (type.startsWith(t)) {
                this.type_ = t;
                return;
            }
        }
        for (const t of UploadResultComponent.otherTypes) {
            if (type.endsWith(t)) {
                this.type_ = "other";
                return;
            }
        }
        this.type_ = "unknown";
    }
}
