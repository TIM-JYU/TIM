import {AfterViewInit, Component, DoCheck, OnInit, ViewChild} from "@angular/core";
import {TabDirective, TabsetComponent} from "ngx-bootstrap/tabs";
import {TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import {TabEntryListService} from "tim/sidebarmenu/services/tab-entry-list.service";
import {TabContainerComponent} from "tim/sidebarmenu/tab-container.component";
import {getStorage, isSmScreen, setStorage} from "tim/util/utils";
import {LectureController} from "tim/lecture/lectureController";
import {ISettings} from "tim/user/settings.component";
import {genericglobals} from "tim/util/globals";

enum MenuState {
    Open,
    Icons,
    Closed,
    Max
}

const MENU_BUTTON_ICONS: Record<MenuState, string> = {
    [MenuState.Open]: "menu-hamburger",
    [MenuState.Icons]: "option-horizontal",
    [MenuState.Closed]: "menu-left",
    [MenuState.Max]: "",
};

@Component({
    selector: "tim-sidebar-menu",
    template: `
        <div (window:resize)="onResize()" class="left-fixed-side" [class.show]="showMenu">
            <div class="btn btn-default btn-sm pull-left" (click)="nextVisibilityState()" i18n-title title="Show menu">
                <i class="glyphicon glyphicon-{{nextGlyphicon}}" i18n-title title="Open sidebar"></i>
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
    private settings: ISettings = genericglobals().userPrefs;
    private lctrl = LectureController.instance;
    private currentMenuState: MenuState = MenuState.Open;
    private isSm = isSmScreen();
    private lastNonSmState = this.lastVisState;
    @ViewChild("tabs") private tabs!: TabsetComponent;
    menuTabs!: TabEntry[];
    tabsVisTable: Record<string, boolean> = {};
    nextGlyphicon: string = MENU_BUTTON_ICONS[this.nextState];

    constructor(private tabEntryList: TabEntryListService) {
    }

    get showMenu() {
        return this.currentMenuState == MenuState.Open;
    }

    get showTabset() {
        return this.currentMenuState != MenuState.Closed;
    }

    get lastUsedTab() {
        if (!this.settings.remember_last_sidebar_menu_tab) {
            return "";
        }
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
        if (!this.settings.remember_last_sidebar_menu_state) {
            return MenuState.Open;
        }
        const val = getStorage("sideBarMenu_lastVisState");
        if (typeof val != "number" || val < 0 || val >= MenuState.Max) {
            return MenuState.Open;
        }
        return val;
    }

    set lastVisState(value: MenuState) {
        if (value < 0 || value >= MenuState.Max) {
            value = MenuState.Open;
        }
        setStorage("sideBarMenu_lastVisState", value);
    }

    onResize(): void {
        if (!this.isSm && isSmScreen()) {
            this.isSm = true;
            this.lastNonSmState = this.currentMenuState;
            this.setVisibleState(MenuState.Closed, true, false);
        } else if (!isSmScreen()) {
            this.isSm = false;
            this.setVisibleState(this.lastNonSmState, true, false);
        }
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

    ngAfterViewInit() {
        this.setVisibleState(!isSmScreen() ? this.lastVisState : MenuState.Closed);
    }

    onTabSelect(tab: TabDirective, tabContainer: TabContainerComponent) {
        this.currentTab = tab.id;
        this.lastUsedTab = this.currentTab;
        this.setVisibleState(MenuState.Open, false);
        void tabContainer.onSelect();
    }

    shouldRender(tabId: string) {
        return this.currentTab == tabId;
    }

    nextVisibilityState() {
        this.setVisibleState(this.nextState);
    }

    setVisibleState(newState: MenuState, updateTabVisibility = true, updateLastVisState = true) {
        this.currentMenuState = newState;
        if (updateLastVisState) {
            this.lastVisState = newState;
        }
        this.nextGlyphicon = MENU_BUTTON_ICONS[this.nextState];
        if (updateTabVisibility) {
            this.updateTabs();
        }
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

    private updateTabs() {
        if (!this.tabs) {
            return;
        }
        // In ngx-bootstrap, the first tab is always active by default, which won't fire the selectTab event
        // if it's selected by default. In turn this won't cause lazy loading to occur
        // To fix this, mark all tabs inactive and then search for tab to activate, which will always trigger
        // selectTab event.
        this.tabs.tabs.forEach((t) => t.active = false);
        if (this.currentMenuState != MenuState.Open) {
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
        this.setVisibleState(MenuState.Open);
    }

    private get nextMobileState(): MenuState {
        return this.currentMenuState == MenuState.Open ? MenuState.Closed : MenuState.Open;
    }

    private get nextDesktopState(): MenuState {
        return (this.currentMenuState + 1) % MenuState.Max;
    }

    private get nextState() {
        return isSmScreen() ? this.nextMobileState : this.nextDesktopState;
    }
}
