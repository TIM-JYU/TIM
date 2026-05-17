import type {OnInit} from "@angular/core";
import {Component, EventEmitter, Input, Output} from "@angular/core";
import type {DocumentOrFolder} from "tim/item/IItem";
import {toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

export interface DirectoryPickerRestrictions {
    // Paths that are selectable. Can be folders or documents.
    allowedPaths?: string[];
    // Depth for checking the paths in allowedPaths.
    // Value 0 is only the path itself.
    maxDepth?: number;
    // Maximum number of items that can be selected.
    maxSelectedCount?: number;
    // Which items are selectable.
    selectable?: "folders" | "documents" | "both";
    // Checkbox behavior of unselectable items.
    behavior?: "disable" | "hide";
}

@Component({
    selector: "tim-directory-picker",
    standalone: true,
    imports: [CommonModule, FormsModule, TimUtilityModule],
    template: `
        <div>
            Selected items: {{ totalSelectedCount }}
            <button class="btn btn-default btn-xs"
                    (click)="unselectAll()">Unselect all
            </button>
        </div>
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
                    <span *ngIf="item.isFolder && (selectedUnderFolder(item) > 0)"
                          class="badge pull-right"
                          [title]="selectedUnderFolder(item) + ' selected inside'">
                          {{ selectedUnderFolder(item) }}
                    </span>
                    <span *ngIf="!item.isFolder">{{ item.title }}</span>
                    <tim-loading *ngIf="loadingFolder === item.id"></tim-loading>
                </td>
                <td>
                    <input
                        type="checkbox"
                        aria-label="Select item"
                        [checked]="isSelected(item)"
                        [disabled]="isDisabled(item)"
                        [hidden]="isHidden && isDisabled(item)"
                        (click)="$event.stopPropagation()"
                        (change)="toggleSelection(item)"
                    >
                </td>
                <td *ngIf="showId">
                    {{ item.id }}
                </td>
            </tr>
            </tbody>
        </table>
        <tim-alert *ngIf="error" severity="danger" i18n>
            {{ error }}
        </tim-alert>
        <p *ngIf="itemList.length == 0">There are no items to show.</p>
    `,
    styleUrls: ["directory-list.component.scss"],
})
export class DirectoryPickerComponent implements OnInit {
    itemList: DocumentOrFolder[] = [];

    currentFolder: string = "";
    selected: Set<string> = new Set();
    allowedPaths?: Set<string>;

    selectedUnderCount: Map<string, number> = new Map();

    showId: boolean = false;
    loadingFolder?: number;
    error?: string;

    @Input() selection: string[] = [];
    @Input() startFolder?: string;
    @Input() restrictions?: DirectoryPickerRestrictions;
    @Input() canSelectItem?: (item: DocumentOrFolder) => boolean;

    @Output() selectionChange: EventEmitter<string[]> = new EventEmitter();

    constructor(private http: HttpClient) {}

    ngOnInit() {
        this.currentFolder = this.startFolder ?? "";
        this.selected = new Set(this.selection ?? []);
        if (this.restrictions?.allowedPaths != undefined) {
            this.allowedPaths = new Set([...this.restrictions.allowedPaths]);
        }
        void this.loadFolder(this.currentFolder);
        this.rebuildSelectedUnderCount();
    }

    isSelected(item: DocumentOrFolder): boolean {
        return this.selected.has(item.path);
    }

    /* Check if the item is selectable. */
    isDisabled(item: DocumentOrFolder): boolean {
        if (this.selected.has(item.path)) {
            return false;
        }
        if (this.hasMaxSelected) {
            return true;
        }
        const canSelect: boolean = this.canSelectItem
            ? this.canSelectItem(item)
            : true;
        if (!this.restrictions) {
            return !canSelect;
        }
        const selectable = this.restrictions.selectable ?? "both";
        return (
            !canSelect ||
            !this.isAllowedItem(item) ||
            (selectable === "folders" && !item.isFolder) ||
            (selectable === "documents" && item.isFolder)
        );
    }

    /* Select or unselect an item path. */
    toggleSelection(item: DocumentOrFolder) {
        if (this.selected.has(item.path)) {
            this.selected.delete(item.path);
            this.bumpAncestors(item, -1);
        } else {
            this.selected.add(item.path);
            this.bumpAncestors(item, 1);
        }
        this.selectionChange.emit([...this.selected]);
    }

    /* Unselect all selected paths. */
    unselectAll() {
        if (this.totalSelectedCount == 0) {
            return;
        }
        if (window.confirm("Unselect all items?")) {
            this.selected.clear();
            this.selectionChange.emit([]);
            this.selectedUnderCount.clear();
        }
    }

    /* Return the amount of items selected under this folder. */
    selectedUnderFolder(item: DocumentOrFolder): number {
        if (!item.isFolder) {
            return -1;
        }
        const total: number = this.selectedUnderCount.get(item.path) ?? 0;
        return this.selected.has(item.path) ? Math.max(0, total - 1) : total;
    }

    /* Rebuild the map maintaining a count of selected items under each folder. */
    private rebuildSelectedUnderCount() {
        this.selectedUnderCount.clear();
        for (const p of this.selected) {
            const parts: string[] = p.split("/").filter(Boolean);
            let cur: string = "";
            for (const part of parts) {
                cur = cur ? `${cur}/${part}` : part;
                this.selectedUnderCount.set(
                    cur,
                    (this.selectedUnderCount.get(cur) ?? 0) + 1
                );
            }
        }
    }

    /* Increment or decrement the item count for all ancestors of the item. */
    private bumpAncestors(item: DocumentOrFolder, delta: 1 | -1) {
        const parts: string[] = item.path.split("/").filter(Boolean);
        let cur: string = "";
        for (const part of parts) {
            cur = cur ? `${cur}/${part}` : part;
            const next: number =
                (this.selectedUnderCount.get(cur) ?? 0) + delta;
            if (next <= 0) {
                this.selectedUnderCount.delete(cur);
            } else {
                this.selectedUnderCount.set(cur, next);
            }
        }
    }

    /* Go up to the parent folder. */
    goUp(e: Event) {
        e.preventDefault();
        void this.loadFolder(this.parentFolder);
    }

    /* Open the given folder and load documents inside. */
    async openFolder(item: DocumentOrFolder, e: Event) {
        e.preventDefault();
        if (!item.isFolder) {
            return;
        }
        this.loadingFolder = item.id;
        await this.loadFolder(item.path);
        this.loadingFolder = undefined;
    }

    /* Load items for a folder path. */
    private async loadFolder(folder: string) {
        this.error = undefined;
        this.currentFolder = folder;
        const r = await toPromise(
            this.http.get<DocumentOrFolder[]>("/getItems", {params: {folder}})
        );
        if (!r.ok) {
            this.error = `Failed to load folder "${folder}"`;
            this.itemList = [];
            return;
        }
        this.itemList = r.result;
    }

    /* Check if the item is in the allowed paths. */
    private isAllowedItem(item: DocumentOrFolder): boolean {
        const allowed = this.allowedPaths;
        if (!allowed) {
            return true;
        }
        const maxDepth = this.restrictions?.maxDepth;
        if (maxDepth === undefined) {
            return allowed.has(item.path);
        }

        // Check item.path, then its parents up to maxDepth steps.
        let path: string = item.path;
        for (let d: number = 0; d <= maxDepth; d++) {
            if (allowed.has(path)) {
                return true;
            }
            const cut: number = path.lastIndexOf("/");
            if (cut < 0) {
                break;
            }
            path = path.slice(0, cut);
        }
        return false;
    }

    /* Return the path string of the parent folder. */
    private get parentFolder(): string {
        if (!this.currentFolder) {
            return "";
        }
        const parts: string[] = this.currentFolder
            .split("/")
            .filter((p) => p.length);
        parts.pop();
        return parts.join("/");
    }

    private get hasMaxSelected(): boolean {
        if (this.restrictions?.maxSelectedCount === undefined) {
            return false;
        }
        return this.totalSelectedCount >= this.restrictions.maxSelectedCount;
    }

    get totalSelectedCount(): number {
        return this.selected.size;
    }

    get canGoUp(): boolean {
        return this.currentFolder.length > 0;
    }

    get isHidden(): boolean {
        if (this.restrictions?.behavior) {
            return this.restrictions.behavior === "hide";
        }
        return false;
    }
}
