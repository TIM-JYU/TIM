import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {Input, NgModule, ViewChild} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
import * as t from "io-ts";
import {int} from "@zxing/library/es2015/customTypings";
import type {Result, AngularError} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {CsUtilityModule} from "../../../../../modules/cs/js/util/module";
import {FileSelectManagerComponent} from "../../../../../modules/cs/js/util/file-select";
import type {
    IFile,
    IFileSpecification,
} from "../../../../../modules/cs/js/util/file-select";

interface ProfileData extends Object {
    username?: string;
    realname?: string;
    email?: string;
    profile_path: string;
    profile_picture_path: string;
    profile_description: string;
    profile_links: string[];
    edit_access?: boolean;
    course_group_name?: string;
}

interface TimUserGroup {
    id: number;
    name: string;
    admin?: string;
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

interface IUploadedFile extends t.TypeOf<typeof UploadedFile> {}

@Component({
    selector: "tim-user-profile",
    styleUrls: ["./user-profile.component.scss"],
    imports: [CsUtilityModule, FormsModule, CommonModule],
    standalone: true,
    template: `
        <div class="tim-user-profile-container">
            <div class="profile-heading">
                <h2>Profile</h2>
                <button *ngIf="!modifyEnabled" class="btn btn-profile" type="button" (click)="modifyUserProfile()">
                    Modify profile <span class="glyphicon glyphicon-wrench"></span></button>
            </div>
            <div class="container">
                <div class="left-column">
                    <img id="tim-user-profile-picture" [src]="profileData.profile_picture_path" alt="profilepic"/>
                    <ng-container *ngIf="editAccess" body>
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
                    <ng-container *ngIf="editAccess" body>
                        <form (ngSubmit)="onSubmit()" #f="ngForm" class="form">
                            <span class="textfield">
                            <textarea name="description" class="form-control textarea"
                                      (ngModelChange)="setWarning('description')"
                                      [class.warnFrame]="checkWarning('description')"
                                      [(ngModel)]="profileData.profile_description"></textarea></span>

                            <span class="textfield">
                            <input #i name="link" class="form-control" type="text"
                                   *ngFor="let item of profileData.profile_links; index as i; trackBy: linkTrackBy"
                                   (ngModelChange)="setWarning(i.toString())"
                                   [(ngModel)]="profileData.profile_links[i]"
                                   [class.warnFrame]="checkWarning(i.toString())"/>
                            <input #group name="group" class="form-control" type="text"
                                           (ngModelChange)="setWarning(group.toString())"
                                           [(ngModel)]="profileData.course_group_name"
                                           [class.warnFrame]="checkWarning(group.toString())"/>
</span>
                            <button type="submit" class="btn">Save <span class="glyphicon glyphicon-send"></span>
                            </button>
                        </form>
                    </ng-container>
                    <ng-container *ngIf="!editAccess">
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
                        <li>Username: {{ profileData.username }}</li>
                        <li>Email: {{ profileData.email }}</li>
                    </ul>
                </div>
                <div class="right-column">
                    <h2>Your group: {{ myGroupName }}</h2>
                    <ul>
                        <li
                                *ngFor="let member of myGroupMembers;">
                            {{ member.name }}
                        </li>
                    </ul>
                </div>
            </div>
        </div>
    `,
})
export class UserProfileComponent implements OnInit {
    @Input() documentId: int = 0;
    @Input() modifyEnabled: boolean = false;
    @Input() userId?: int;
    editAccess: boolean = false;
    warnings: string[] = [];
    uploadUrl?: string;
    detailsUrl: string = "";
    dragAndDrop: boolean = true;
    uploadedFiles: IUploadedFile[] = [];
    stem: string = "Change a profile picture";
    fileSelect?: FileSelectManagerComponent;
    profileData: ProfileData;
    myProjectGroup?: TimUserGroup;
    myGroups?: TimUserGroup[];
    myGroupName?: string;
    myGroupMembers?: TimUserGroup[];

    constructor(private http: HttpClient) {
        this.profileData = {
            profile_description: "",
            profile_links: [""],
            profile_picture_path: "",
            profile_path: "",
        };
    }

    ngOnInit() {
        // TODO: change data fetching technique/method to another
        this.getProfileData(this.userId);
        // this.getMyGroups();
        // this.getMyGroupName();

        this.uploadUrl = `/profile/picture/${this.documentId}`;
        this.detailsUrl = `/profile/details/${this.documentId}`;
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

    getProfileData(userId?: int) {
        let dataEndpoint = "/profile";

        if (userId != undefined) {
            dataEndpoint = `/profile/${userId}`;
        }
        const data = this.http.get<ProfileData>(dataEndpoint).subscribe({
            next: (res: ProfileData) => {
                this.profileData = res;

                // TODO: organize dependent calls into ngOnInit function
                this.getMyGroups();

                if (this.modifyEnabled && this.profileData.edit_access) {
                    this.editAccess = true;
                }
            },
        });
        return data;
    }

    getMyGroups() {
        const endpoint: string = `/groups/usergroups`;

        const data = this.http.get<TimUserGroup[]>(endpoint).subscribe({
            next: (res) => {
                this.myGroups = res;

                // TODO: organize into ngOnInit
                this.myGroupName = this.getMyGroupName();
                this.getMyGroupMembers();
            },
        });

        return data;
    }

    getMyGroupMembers() {
        if (!this.myGroupName) {
            console.log("Group name not yet stored.");
            return;
        }

        const groupName: string = this.myGroupName;
        const endpoint: string = `/groups/show/${groupName}`;

        const data = this.http.get<TimUserGroup[]>(endpoint).subscribe({
            next: (res) => {
                this.myGroupMembers = res;
            },
        });
        return data;
    }

    getMyGroupName(): string {
        let group = "No group found.";
        if (this.myGroups == undefined) {
            console.log(group);
            return group;
        }

        // Find prefixed
        const myGroup = this.myGroups.filter((item) => {
            if (this.profileData.course_group_name == undefined) {
                console.log("Course group name undefined.");
                return "";
            }
            item.name.startsWith(`${this.profileData.course_group_name}-`);
        });

        if (myGroup[0]) {
            group = myGroup[0].name;
        }

        return group;
    }

    modifyUserProfile() {
        window.open(this.profileData.profile_path);
    }

    onFileLoad(file: IFile) {
        console.log(file);
    }

    onUpload(resp: unknown) {
        console.log("On upload");
        console.log(resp);
        if (!resp) {
            return;
        }

        const img: {image: string} = resp as {image: string};

        this.profileData.profile_picture_path = `/images/${img.image}`;
    }

    onUploadDone(success: boolean) {
        console.log("Upload done");
    }

    async onSubmit() {
        // Reset changes warning in child component.
        this.warnings = [];

        // Prepare data for submit

        const data: ProfileData = this.profileData;

        // Call endpoint, which handles storing the data into document settings
        const response = toPromise(
            this.http.post<{ok: boolean}>(this.detailsUrl, data)
        );

        // Get result from response of the endpoint.
        const result: Result<{ok: boolean}, AngularError> = await response;

        return result;
    }

    setWarning(name: string) {
        if (!this.warnings.includes(name)) {
            this.warnings.push(name);
        }
    }

    checkWarning(name: string) {
        return this.warnings.includes(name);
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
