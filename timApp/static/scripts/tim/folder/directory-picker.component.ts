import {Component, EventEmitter, Input, Output} from "@angular/core";
import type {DocumentOrFolder, IFolder, IItem, ITag} from "tim/item/IItem";
import {HttpClient} from "@angular/common/http";

@Component({
    selector: "tim-directory-picker",
    template: `
        <table class="table" *ngIf="itemList.length > 0 || item.path">
            <thead>
            <tr>
                <th></th>
                <th>Name</th>
                <th>Selected</th>
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
            </tr>
            <tr *ngFor="let item of itemList">
                <td>
                    <a *ngIf="item.isFolder" href="/view/{{ item.path }}">
                        <span class="glyphicon glyphicon-folder-open" aria-hidden="true"></span>
                    </a>
                </td>
                <td>
                    <a href="/view/{{ item.path }}">{{ item.title }}</a>&ngsp;
                </td>
                <td></td>
                <td *ngIf="showId">
                    {{item.id}}
                </td>
            </tr>
            </tbody>
        </table>
        <p *ngIf="itemList.length == 0">There are no items to show.</p>
    `,
    styleUrls: ["directory-list.component.scss"],
})
export class DirectoryPickerComponent {
    itemList: DocumentOrFolder[] = [];
    item: IFolder;

    currentFolder: string = "";
    selected: Set<string> = new Set();
    showId: boolean = false;
    error: string | null = null;

    @Input() startFolder: string = "";
    @Input() selectable: "folders" | "documents" | "both" = "both";
    @Input() selection: string[] = [];

    @Output() selectionChange: EventEmitter<string[]> = new EventEmitter();

    constructor(private http: HttpClient) {}

    ngOnInit() {
        this.currentFolder = this.startFolder ?? "";
        this.selected = new Set(this.selection ?? []);
    }
}
