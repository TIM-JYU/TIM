import {Input, OnInit, ViewChild} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {IItem, redirectToItem} from "tim/item/IItem";
import type {IDocument} from "tim/item/IItem";
import {to, to2} from "tim/util/utils";
import {$http} from "tim/util/ngimport";
import type {
    IFile,
    IFileSpecification,
} from "../../../../../modules/cs/js/util/file-select";
import {FileSelectManagerComponent} from "../../../../../modules/cs/js/util/file-select";
import {CsUtilityModule} from "../../../../../modules/cs/js/util/module";
import {tr} from "date-fns/locale";
import * as t from "io-ts";
import {int} from "@zxing/library/es2015/customTypings";

interface ProfileData {
    profile_picture_path: string;
}

const UploadedFile = t.intersection([
    t.type({
        path: t.string,
        type: t.string,
    }),
    t.partial({
        rotation: t.number,
    }),
]);

interface IUploadResponse {
    file: string;
    type: string;
    block: number;
}

interface IUploadedFile extends t.TypeOf<typeof UploadedFile> {}

@Component({
    selector: "tim-user-profile",
    styleUrls: ["./user-profile.component.scss"],
    template: `
        <div class="tim-user-profile-container">
        <div class="profile-heading">
            <h2>About</h2>
            <button *ngIf="!modifyEnabled" class="btn btn-profile" type="button" (click)="modifyUserProfile()">Modify profile <span class="glyphicon glyphicon-wrench"></span></button>
        </div>
        <div class="container">
            <div class="left-column">
                <img id="tim-user-profile-picture" [src]="pictureUrl" alt="profilepic"/>
            </div>
            <div class="right-column">
                <p>
                    Paragraph inside {{pictureUrl}}
                </p>
            </div>
            <ng-container *ngIf="modifyEnabled" body>
                <div class="profile-picture-box form-inline small">
                    <div style="position: relative;" *ngFor="let item of uploadedFiles; let i = index">
                        <div class="uploadContainer" #wraps>
                            <img alt="Uploaded image" #img [src]="item.path" (load)="onImgLoad($event, i)">
                        </div>
                    </div>
                </div>

            <div>
                    <file-select-manager class="small"
                                         [dragAndDrop]="dragAndDrop"
                                         [uploadUrl]="uploadUrl"
                                         [stem]="stem"
                                         (file)="onFileLoad($event)"
                                         (upload)="onUpload($event)"
                                         (uploadDone)="onUploadDone($event)"
                                         [accept]="'image/*,.pdf'">
                    </file-select-manager>
                </div>
            </ng-container>
        </div>
            </div>
    `,
})
export class UserProfileComponent implements OnInit {
    @Input() userId?: int = 0;
    @Input() profileDocument?: int;
    @Input() modifyEnabled: boolean = false;
    pictureUrl: string = "";
    profileUrl: string = "";
    uploadUrl?: string;
    dragAndDrop: boolean = true;
    uploadedFiles: IUploadedFile[] = [];
    stem: string = "Change a profile picture";
    fileSelect?: FileSelectManagerComponent;

    constructor(private http: HttpClient) {}

    async ngOnInit() {
        // TODO: lue inputosta käyttäjä
        await this.getProfileData(this.userId);
        this.uploadUrl = `/profile/picture/${this.profileDocument}`;
        console.log(this.userId);
    }

    @ViewChild(FileSelectManagerComponent)
    set fileSelectSetter(component: FileSelectManagerComponent | undefined) {
        this.fileSelect = component;
        if (!component) {
            return;
        }

        const files: IFileSpecification[] = [];
        files.push({
            paths: ["profilepicture"],
            upload: true,
        });

        component.allowMultiple = true; // this.markup.allowMultipleFiles;
        component.multipleElements = true; // this.markup.multipleUploadElements;
        component.files = files;
    }

    async getProfileData(userId?: int) {
        const data = this.http
            .get(`/profile/${userId}`)
            .subscribe((res: any) => {
                console.log(res);
                this.pictureUrl = res.profile_picture_path;
                this.profileUrl = res.profile_path;
                return true;
            });
        return data;
    }
    async modifyUserProfile() {
        /*const doc = await to($http.get<IDocument>(this.profileUrl));

        if (!doc.ok) {
            console.log("No profile document.");
            return doc.result.data.error;
        }

         */
        console.log("Go to profile document: " + this.profileUrl);
        window.location.href = this.profileUrl;
    }

    onFileLoad(file: IFile) {
        console.log(file);
    }

    onUpload(resp: any) {
        console.log("On upload");
        console.log(resp);
        if (!resp) {
            return;
        }

        const resps = resp as [IUploadResponse];
        for (const response of resps) {
            this.uploadedFiles.push({path: response.file, type: response.type});
        }
    }

    onPasteFocusout() {
        console.log("Focus out");
    }

    onPaste(event: any) {
        console.log("On paste");
        console.log(event);
    }

    onImgLoad(e: Event, index: number): void {
        console.log("On image load");
        console.log(e);
    }

    onUploadDone(success: boolean) {
        console.log("Upload done");
    }
}
/*
@NgModule({
    declarations: [UserProfileComponent],
})
export class UserProfileModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
*/
