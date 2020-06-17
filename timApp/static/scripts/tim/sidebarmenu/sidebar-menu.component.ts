import {AfterViewInit, Component, DoCheck, OnInit, ViewChild} from "@angular/core";
import {TabDirective, TabsetComponent} from "ngx-bootstrap/tabs";
import {TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import {TabEntryListService} from "tim/sidebarmenu/services/tab-entry-list.service";
import {TabContainerComponent} from "tim/sidebarmenu/tab-container.component";
import {slugify} from "tim/util/slugify";
import {getStorage, setStorage} from "tim/util/utils";
import {LectureController} from "tim/lecture/lectureController";

@Component({
    selector: "app-sidebar-menu",
    template: `
        <div class="left-fixed-side" [class.show]="showSidebar">
            <div class="btn btn-default btn-sm pull-left" (click)="toggleSidebar()" i18n-title title="Show menu">
                <i class="glyphicon glyphicon-menu-hamburger" i18n-title title="Click to open sidebar-menu"></i>
            </div>
            <tabset id="menuTabs" [class.hidden-sm]="hidden" [class.hidden-xs]="hidden" #tabs>
                <ng-container *ngFor="let menuTab of menuTabs">
                    <tab *ngIf="tabsVisTable[menuTab.id]"
                         [id]="menuTab.id"
                         (selectTab)="onTabSelect($event, tabContainer)">
                        <ng-template tabHeading>
                            <i class="glyphicon glyphicon-{{menuTab.icon}}" title="{{menuTab.title}}"></i>
                        </ng-template>
                        <tab-container #tabContainer [tabItem]="menuTab"
                                       [class.hidden]="!shouldRender(menuTab.id)"></tab-container>
                    </tab>
                </ng-container>
            </tabset>
        </div>
    `,
})
export class SidebarMenuComponent implements OnInit, AfterViewInit, DoCheck {
    hidden = true;
    showSidebar = true;
    private currentTab?: string;
    @ViewChild("tabs") private tabs!: TabsetComponent;
    menuTabs!: TabEntry[];
    tabsVisTable: Record<string, boolean> = {};
    lctrl = LectureController.instance;

    constructor(private tabEntryList: TabEntryListService) {
    }

    get lastUsedTab() {
        const val = getStorage("sideBarMenu_lastUsedTab");
        if (typeof val != "string") {
            return "";
        }
        return val;
    }

    set lastUsedTab(value: string) {
        setStorage("sideBarMenu_lastUsedTab", value);
    }

    ngOnInit(): void {
        this.currentTab = this.lastUsedTab;
        this.menuTabs = this.tabEntryList.getTabEntries();
        const defaultTabs = this.tabEntryList.defaultTabOrder;
        const defaultTabsSet = new Set(defaultTabs);
        const defaultTableVis: Record<string, boolean> = {};
        for (const tab of this.menuTabs) {
            const visible = tab.visible();
            this.tabsVisTable[tab.id] = visible;
            if (defaultTabsSet.has(tab.id)) {
                defaultTableVis[tab.id] = visible;
            }
        }
        if (!this.currentTab) {
            const firstTab = defaultTabs.find((val) => defaultTableVis[val]);
            if (!firstTab) {
                return;
            }
            this.currentTab = firstTab;
        }
    }

    ngDoCheck() {
        // Previously, old sidebar menu could make do with ng-ifs to detect which one to show.
        // Here' to reduce size we use lazy imports and data-based approach to defining tabs, in which case
        // we need to implement custom change detection.
        // Therefore, the visible() checks should generally be really small and fast
        let shouldSet = false;
        const visTabs: Record<string, boolean> = {};
        for (const tab of this.menuTabs) {
            const isVisible = tab.visible();
            visTabs[tab.id] = isVisible;
            if (isVisible != this.tabsVisTable[tab.id]) {
                shouldSet = true;
            }
        }
        if (shouldSet) {
            this.tabsVisTable = visTabs;
        }
        // TODO: Move to a more appropriate place
        void this.lctrl.refreshWall();
    }

    ngAfterViewInit() {
        this.setSidebarState(true);
    }

    onTabSelect(tab: TabDirective, tabContainer: TabContainerComponent) {
        this.showSidebar = true;
        this.currentTab = tab.id;
        this.lastUsedTab = this.currentTab;
        void tabContainer.onSelect();
    }

    shouldRender(tabId: string) {
        return this.currentTab == tabId;
    }

    private setSidebarState(visible: boolean) {
        this.showSidebar = visible;

        if (!this.tabs) {
            return;
        }
        // In ngx-bootstrap, the first tab is always active by default, which won't fire the selectTab event
        // if it's selected by default. In turn this won't cause lazy loading to occur
        // To fix this, mark all tabs inactive and then search for tab to activate, which will always trigger
        // selectTab event.
        this.tabs.tabs.forEach((t) => t.active = false);
        if (!this.showSidebar) {
            return;
        }
        const activeTab = this.tabs.tabs.find((t) => t.id == this.currentTab);
        if (activeTab) {
            activeTab.active = true;
        }
    }

    toggleSidebar() {
        this.setSidebarState(!this.showSidebar);
        this.hidden = !this.showSidebar;
    }
}
