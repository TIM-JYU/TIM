import {
    ApplicationRef,
    DoBootstrap,
    EventEmitter,
    Input,
    NgModule,
    OnInit,
    Output,
    TrackByFunction,
    ViewChild,
} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {AngularError, toPromise} from "tim/util/utils";
import type {
    IFile,
    IFileSpecification,
} from "../../../../../modules/cs/js/util/file-select";
import {FileSelectManagerComponent} from "../../../../../modules/cs/js/util/file-select";
import * as t from "io-ts";
import {int} from "@zxing/library/es2015/customTypings";
import {InputDialogKind} from "tim/ui/input-dialog.kind";
import type {Result} from "tim/util/utils";
import {showInputDialog} from "tim/ui/showInputDialog";
import {
    StandaloneTextfieldComponent,
    StandaloneTextfieldModule,
} from "../../../../../modules/fields/js/standalone-textfield.component";
import {CsUtilityModule} from "../../../../../modules/cs/js/util/module";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {boolean} from "fp-ts";

interface ProfileData {
    username?: string;
    realname?: string;
    email?: string;
    profile_description: string;
    profile_links: string[];
    document_id?: int;
}

interface ImageEvent extends Event {
    image: string;
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
    imports: [
        StandaloneTextfieldComponent,
        CsUtilityModule,
        FormsModule,
        CommonModule,
    ],
    standalone: true,
    template: `
        <div class="tim-user-profile-container">
            <div class="profile-heading">
                <h2>About</h2>
                <button *ngIf="!modifyEnabled" class="btn btn-profile" type="button" (click)="modifyUserProfile()">
                    Modify profile <span class="glyphicon glyphicon-wrench"></span></button>
            </div>
            <div class="container">
                <div class="left-column">
                    <img id="tim-user-profile-picture" [src]="pictureUrl" alt="profilepic"/>
                    <ng-container *ngIf="modifyEnabled" body>
                        <div>
                            <file-select-manager class="small"
                                                 [dragAndDrop]="dragAndDrop"
                                                 [uploadUrl]="uploadUrl"
                                                 [stem]="stem"
                                                 (file)="onFileLoad($event)"
                                                 (upload)="onUpload($event)"
                                                 (uploadDone)="onUploadDone($event)"
                                                 [accept]="'image/*'">
                            </file-select-manager>
                        </div>
                    </ng-container>
                </div>
                <div class="right-column">
                    <ng-container *ngIf="modifyEnabled" body>
                        <form (ngSubmit)="onSubmit()" #f="ngForm">
                            <tim-standalone-textfield name="description" inputType="TEXTAREA"
                                                      (valueChange)="updateDescription($event)"
                                                      [initialValue]="profileData.profile_description"
                                                      [inputWarn]="warn"></tim-standalone-textfield>
                            <tim-standalone-textfield #i name="link" inputType="TEXT" *ngFor="let item of profileData.profile_links; index as i; trackBy: linkTrackBy"
                                                      (valueChange)="updateLink($event, i)"
                                                        [initialValue]="profileData.profile_links[i]"
                                                      [inputWarn]="warn"></tim-standalone-textfield>
                            <button type="submit" class="btn">Save <span class="glyphicon glyphicon-send"></span></button>
                        </form>
                    </ng-container>
                    <ng-container *ngIf="!modifyEnabled">
                        <p>
                            {{ profileData.profile_description }}
                        </p>
                        <ng-container *ngFor="let item of profileData.profile_links">
                        <a [href]="item">{{ item }}</a>
                            </ng-container> 
                    </ng-container>
                </div>

                <!-- <div (click)="showTextInput()">Inputti</div> -->
            </div>
            <div class="container">
                <div class="left-column">
                    
                <h2>Hello, {{ profileData.realname }}!</h2>
                <ul>
                <li>
                Username: {{ profileData.username }}
                </li>
                <li>

                Email: {{ profileData.email }}
                </li>
                </ul>
                </div>
                <div class="right-column">
                    <h2>Group number</h2>
                    <ul>
                <li>
    member1
                </li>
                <li>
member2
                </li>
                </ul>
                </div>
            </div>
        </div>
    `,
})
export class UserProfileComponent implements OnInit {
    @Input() userId?: int;
    @Input() documentId?: int;
    @Input() modifyEnabled: boolean = false;
    warn: boolean | null = null;
    pictureUrl: string = "";
    profileUrl: string = "";
    uploadUrl?: string;
    detailsUrl: string = "";
    dragAndDrop: boolean = true;
    uploadedFiles: IUploadedFile[] = [];
    stem: string = "Change a profile picture";
    fileSelect?: FileSelectManagerComponent;
    profileData: ProfileData;

    constructor(private http: HttpClient) {
        this.profileData = {
            profile_description: "",
            profile_links: [""],
            document_id: 1,
        };
    }

    async ngOnInit() {
        await this.getProfileData(this.userId);
        this.uploadUrl = `/profile/picture/${this.documentId}`;
        this.detailsUrl = `/profile/details`;
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
                this.profileData = {
                    realname: res.realname,
                    email: res.email,
                    username: res.username,
                    profile_description: res.profile_description,
                    profile_links: res.profile_links,
                };
                this.pictureUrl = res.profile_picture_path;
                this.profileUrl = res.profile_path;
                return true;
            });
        console.log(data);
        return data;
    }

    modifyUserProfile() {
        window.open(this.profileUrl);
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

        const pic = resp as IUploadResponse;
        // for (const response of resps) {
        //     this.uploadedFiles.push({path: response.file, type: response.type});
        // }
        this.uploadedFiles.push({path: pic.file, type: pic.type});
        this.pictureUrl = `/images/${resp.image}`;
        console.log(this.pictureUrl);
    }

    onImgLoad(event: any, index: number): void {
        console.log("On image load");
        this.pictureUrl = `/images/${event.image}`;
        console.log(this.pictureUrl);
    }

    onUploadDone(success: boolean) {
        console.log("Upload done");
    }

    async onSubmit() {
        // Reset changes warning in child component.
        this.warn = false;

        // Prepare data for submit
        this.profileData.document_id = this.documentId;
        console.log(this.profileData);
        const data: ProfileData = this.profileData;

        // Call endpoint, which handles storing the data into document settings
        const response = toPromise(
            this.http.post<{ok: boolean}>(this.detailsUrl, data)
        );

        // Get result from response of the endpoint.
        const result: Result<{ok: boolean}, AngularError> = await response;
        this.warn = null;
        return result;
    }

    showValue(res: string) {}

    async showTextInput() {
        console.log("mo");
        showInputDialog({
            isInput: InputDialogKind.InputAndValidator,
            text: "Update a profile description.",
            title: "Update profile.",
            defaultValue: "Your description",
            inputType: "textarea",
            validator: (input) => {
                return new Promise<Result<string, string>>((res) => {
                    return res({ok: true, result: input});
                });
            },
        });
    }

    updateDescription($event: string) {
        console.log($event);
        this.profileData.profile_description = $event;
    }

    updateLink($event: string, i: number) {
        console.log($event);
        this.profileData.profile_links[i] = $event;
    }

    linkTrackBy(i: number, item: string): number {
        return i;
    }
}

@NgModule({
    imports: [UserProfileComponent],
})
export class UserProfileModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
