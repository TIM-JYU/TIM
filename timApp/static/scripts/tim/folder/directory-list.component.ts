import {Component} from "@angular/core";
import type {DocumentOrFolder, IFolder, IItem} from "tim/item/IItem";
import {Users} from "tim/user/userService";
import {folderglobals} from "tim/util/globals";

const MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX = "archives/";
const TIM_MESSAGES_FOLDER_PREFIX = "messages/tim-messages";

@Component({
    selector: "tim-index",
    template: `
        <table class="table" *ngIf="itemList.length > 0 || item.path">
            <thead>
            <tr>
                <th></th>
                <th>Name</th>
                <th></th>
                <th>Last modified</th>
                <th>Owners</th>
                <th>Rights</th>
                <th class="gray" (click)="showId = !showId">Id</th>
            </tr>
            </thead>
            <tbody>
            <tr *ngIf="item.path">
                <td>
                    <a href="/view/{{ item.location }}">
                        <span class="glyphicon glyphicon-level-up" aria-hidden="true"></span>
                    </a>
                </td>
                <td><a href="/view/{{ item.location }}">Go to parent folder</a></td>
                <td></td>
                <td></td>
                <td></td>
                <td></td>
                <td></td>
            </tr>
            <tr *ngFor="let item of itemList">
                <td>
                    <a *ngIf="item.isFolder" href="/view/{{ item.path }}">
                        <span class="glyphicon glyphicon-folder-open" aria-hidden="true"></span>
                    </a>
                </td>
                <td>
                    <a href="/view/{{ item.path }}">{{ item.title }}</a>&ngsp;
                    <a><i *ngIf="item.unpublished" class="glyphicon glyphicon-lock"
                          title="Unpublished item"></i></a>
                </td>
                <td></td>
                <td>{{ item.modified }}</td>
                <td>{{ listOwnerNames(item) }}</td>
                <td>
                    <a title="Edit" *ngIf="item.rights.editable && !item.isFolder"
                       href="/view/{{ item.path }}"><i
                            class="glyphicon glyphicon-pencil"></i></a>
                    &ngsp;<a title="Manage" *ngIf="item.rights.manage" href="/manage/{{ item.path }}"><i
                            class="glyphicon glyphicon-cog"></i></a>
                    &ngsp;<a title="Teacher" *ngIf="item.rights.teacher && !item.isFolder"
                       href="/teacher/{{ item.path }}"><i class="glyphicon glyphicon-education"></i></a>
                </td>
                <td *ngIf="showId">
                    {{item.id}}
                </td>
            </tr>
            </tbody>
        </table>
        <p *ngIf="itemList.length == 0">There are no items to show.</p>
        <tabset *ngIf="canCreate">
            <tab [active]="false">
                        <ng-template tabHeading>
                            <span>Create a new document</span>
                            <span class="glyphicon glyphicon-file icon-inline" aria-hidden="true"></span>
                        </ng-template>
                <create-item itemType="document" itemLocation="{{ item.path }}"></create-item>
            </tab>
            <tab [active]="false">
                        <ng-template tabHeading>
                            <span>Create a new folder</span>
                            <span class="glyphicon glyphicon-folder-open icon-inline" aria-hidden="true"></span>
                        </ng-template>
                <create-item itemType="folder" itemLocation="{{ item.path }}"></create-item>
            </tab>
        </tabset>
    `,
    styleUrls: ["directory-list.component.scss"],
})
export class DirectoryListComponent {
    itemList: DocumentOrFolder[];
    item: IFolder;
    canCreate: boolean;
    showId = false;

    constructor() {
        const fg = folderglobals();
        this.itemList = fg.items;
        this.item = fg.curr_item;
        this.canCreate = Users.isRealUser();

        // TODO: Allow to sort all columns instead
        if (
            this.item.path.startsWith(MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX) ||
            this.item.path.startsWith(TIM_MESSAGES_FOLDER_PREFIX)
        ) {
            this.itemList = this.itemList.sort((a, b) => b.id - a.id);
        }
    }

    listOwnerNames(i: IItem) {
        return i.owners.map((o) => o.name).join(", ");
    }
}
