import {AfterViewInit, Component, DoCheck, OnInit, ViewChild} from "@angular/core";
import {TabDirective, TabsetComponent} from "ngx-bootstrap/tabs";
import {TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import {TabEntryListService} from "tim/sidebarmenu/services/tab-entry-list.service";
import {TabContainerComponent} from "tim/sidebarmenu/tab-container.component";
import {slugify} from "tim/util/slugify";
import {getStorage, isSmallScreen, queryDeviceVisible, setStorage} from "tim/util/utils";
import {LectureController} from "tim/lecture/lectureController";
import Menu = JQueryUI.Menu;

enum MenuState {
    OPEN,
    ICONS,
    CLOSED,
    MAX
}

@Component({
    selector: "app-sidebar-menu",
    template: `
        <div class="left-fixed-side" [class.show]="showMenu">
            <div class="btn btn-default btn-sm pull-left" (click)="nextVisibilityState()" i18n-title title="Show menu">
                <i class="glyphicon glyphicon-menu-hamburger" i18n-title title="Click to open sidebar-menu"></i>
            </div>
            <tabset id="menuTabs" [class.show]="showTabset" #tabs>
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
    private currentTab?: string;
    @ViewChild("tabs") private tabs!: TabsetComponent;
    menuTabs!: TabEntry[];
    tabsVisTable: Record<string, boolean> = {};
    lctrl = LectureController.instance;
    currentMenuState: MenuState = MenuState.OPEN;

    constructor(private tabEntryList: TabEntryListService) {
    }

    get showMenu() {
        return this.currentMenuState == MenuState.OPEN;
    }

    get showTabset() {
        return this.currentMenuState != MenuState.CLOSED;
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

    get lastVisState(): MenuState {
        const val = getStorage("sideBarMenu_lastVisState");
        if (typeof val != "number" || val < 0 || val >= MenuState.MAX) {
            return MenuState.OPEN;
        }
        return val;
    }

    set lastVisState(value: MenuState) {
        if (value < 0 || value >= MenuState.MAX) {
            value = MenuState.OPEN;
        }
        setStorage("sideBarMenu_lastVisState", value);
    }

    ngOnInit(): void {
        this.currentTab = this.lastUsedTab;
        this.menuTabs = this.tabEntryList.getTabEntries();
        for (const tab of this.menuTabs) {
            this.tabsVisTable[tab.id] = tab.visible();
        }
        this.trySetCurrentTabToDefault();
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
            this.tabVisibilityChanged();
        }
        // TODO: Move to a more appropriate place
        void this.lctrl.refreshWall();
    }

    private trySetCurrentTabToDefault() {
        if (this.currentTab && this.tabsVisTable[this.currentTab]) {
            return false;
        }
        const firstDefaultTab = this.tabEntryList.defaultTabOrder.find((id) => this.tabsVisTable[id]);
        if (!firstDefaultTab) {
            return false;
        }
        this.currentTab = firstDefaultTab;
        this.lastUsedTab = this.currentTab;
        return true;
    }

    ngAfterViewInit() {
        this.setVisibleState(!this.isSmallScreen ? this.lastVisState : MenuState.CLOSED);
    }

    onTabSelect(tab: TabDirective, tabContainer: TabContainerComponent) {
        this.currentTab = tab.id;
        this.lastUsedTab = this.currentTab;
        this.setVisibleState(MenuState.OPEN, false);
        void tabContainer.onSelect();
    }

    shouldRender(tabId: string) {
        return this.currentTab == tabId;
    }

    nextVisibilityState() {
        this.setVisibleState(this.isSmallScreen ? this.nextMobileState : this.nextDesktopState);
    }

    setVisibleState(newState: MenuState, updateTabVisibility = true) {
        this.currentMenuState = newState;
        this.lastVisState = newState;
        if (updateTabVisibility) {
            this.updateTabs();
        }
    }

    private updateTabs() {
        if (!this.tabs) {
            return;
        }
        // In ngx-bootstrap, the first tab is always active by default, which won't fire the selectTab event
        // if it's selected by default. In turn this won't cause lazy loading to occur
        // To fix this, mark all tabs inactive and then search for tab to activate, which will always trigger
        // selectTab event.
        this.tabs.tabs.forEach((t) => t.active = false);
        if (this.currentMenuState != MenuState.OPEN) {
            return;
        }
        const activeTab = this.tabs.tabs.find((t) => t.id == this.currentTab);
        if (activeTab) {
            activeTab.active = true;
        }
    }

    private tabVisibilityChanged() {
        if (!this.trySetCurrentTabToDefault() || !this.currentTab) {
            return;
        }
        this.setVisibleState(MenuState.OPEN);
    }

    private get isSmallScreen() {
        return queryDeviceVisible("xs") || queryDeviceVisible("sm");
    }

    private get nextMobileState() {
        return this.currentMenuState == MenuState.OPEN ? MenuState.CLOSED : MenuState.OPEN;
    }

    private get nextDesktopState() {
        return (this.currentMenuState + 1) % MenuState.MAX;
    }
}
