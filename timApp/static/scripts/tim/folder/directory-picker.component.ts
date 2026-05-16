import type {OnInit} from "@angular/core";
import {Component, EventEmitter, Input, Output} from "@angular/core";
import type {DocumentOrFolder} from "tim/item/IItem";
import {toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {CommonModule} from "@angular/common";

@Component({
    selector: "tim-directory-picker",
    standalone: true,
    imports: [CommonModule],
    template: `
        <table class="table" *ngIf="itemList.length > 0 || currentFolder">
            <thead>
            <tr>
                <th></th>
                <th>Name</th>
                <th>Selected</th>
                <th class="gray" (click)="showId = !showId">Id</th>
            </tr>
            </thead>
            <tbody>
            <tr *ngIf="canGoUp">
                <td>
                    <a href (click)="goUp($event)">
                        <span class="glyphicon glyphicon-level-up" aria-hidden="true"></span>
                    </a>
                </td>
                <td><a href (click)="goUp($event)">Go to parent folder</a></td>
                <td></td>
                <td></td>
            </tr>
            <tr *ngFor="let item of itemList">
                <td>
                    <a *ngIf="item.isFolder" href (click)="openFolder(item, $event)">
                        <span class="glyphicon glyphicon-folder-open" aria-hidden="true"></span>
                    </a>
                </td>
                <td>
                    <a *ngIf="item.isFolder" href (click)="openFolder(item, $event)">{{ item.title }}</a>
                    <span *ngIf="!item.isFolder">{{ item.title }}</span>
                </td>
                <td></td>
                <td *ngIf="showId">
                    {{ item.id }}
                </td>
            </tr>
            </tbody>
        </table>
        <p *ngIf="itemList.length == 0">There are no items to show.</p>
    `,
    styleUrls: ["directory-list.component.scss"],
})
export class DirectoryPickerComponent implements OnInit {
    itemList: DocumentOrFolder[] = [];

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
        void this.loadFolder(this.currentFolder);
    }

    /* Go up to the parent folder. */
    goUp(e: Event) {
        e.preventDefault();
        void this.loadFolder(this.parentFolder);
    }

    /* Open the given folder and load documents inside. */
    openFolder(item: DocumentOrFolder, e: Event) {
        e.preventDefault();
        if (!item.isFolder) {
            return;
        }
        void this.loadFolder(item.path);
    }

    /* Load items for a folder path. */
    private async loadFolder(folder: string) {
        this.error = null;
        this.currentFolder = folder;
        const r = await toPromise(
            this.http.get<DocumentOrFolder[]>("/getItems", {params: {folder}})
        );
        if (!r.ok) {
            this.error = "Failed to load items";
            this.itemList = [];
            return;
        }
        this.itemList = r.result;
    }

    /* Return the path string of the parent folder. */
    get parentFolder(): string {
        if (!this.currentFolder) {
            return "";
        }
        const parts = this.currentFolder.split("/").filter((p) => p.length);
        parts.pop();
        return parts.join("/");
    }

    get canGoUp() {
        return this.currentFolder.length > 0;
    }
}
