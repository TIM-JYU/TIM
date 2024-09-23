import {Input, OnInit} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {redirectToItem} from "tim/item/IItem";

@Component({
    selector: "tim-user-profile",
    template: `
        <div class="profile-heading">
            <h2>About</h2>
            <button class="btn btn-profile" type="button" (click)="modifyUserProfile()">Modify profile <span class="glyphicon glyphicon-wrench"></span></button>
        </div>
        <div class="container">
            <div class="left-column">
                <img [src]="profilePictureUrl" alt="profile-picture"/>
            </div>
            <div class="right-column">
                <p>
                    Paragraph inside {{foo}}
                </p>
            </div>
        </div>
    `,
    styleUrls: ["./user-profile.component.scss"],
})
export class UserProfileComponent implements OnInit {
    @Input() foo?: string | null;
    pictureUrl: string = "";
    profileUrl: string = "";

    constructor(private http: HttpClient) {}

    async ngOnInit() {
        // TODO: lue inputosta käyttäjä
        const data = await this.getProfileData();
        console.log(data);
        console.log(this.foo);
    }

    async getProfileData() {
        const data = this.http.get("/profile");
        return data;
    }
    modifyUserProfile() {
        this.profileUrl = "URL not set.";
        console.log("Go to profile document: " + this.profileUrl);
    }

    get profilePictureUrl() {
        return this.pictureUrl;
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
