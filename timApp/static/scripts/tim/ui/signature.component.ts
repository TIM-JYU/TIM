import {Component, Input} from "@angular/core";
import {Moment} from "moment";
import {IUser} from "../user/IUser";

@Component({
    selector: "tim-signature",
    template: `
        <span class="comment-info">
            <a class="username" title="{{ user.real_name }}"
               href="mailto:{{ user.email }}">{{ user.name }}</a>
                   <span class="timestamp"
                         title="{{ time.toISOString() }}">
                       {{ time | relative_time }}</span>
        </span>
    `,
    styleUrls: ["./signature.component.scss"],
})
export class SignatureComponent {
    @Input() user!: IUser;
    @Input() time!: Moment;
}
