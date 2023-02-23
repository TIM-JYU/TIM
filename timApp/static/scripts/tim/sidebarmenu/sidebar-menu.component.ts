import type {AfterViewInit, DoCheck, OnInit} from "@angular/core";
import {Component, ViewChild} from "@angular/core";
import type {TabDirective} from "ngx-bootstrap/tabs";
import {TabsetComponent} from "ngx-bootstrap/tabs";
import type {TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import {TabEntryListService} from "tim/sidebarmenu/services/tab-entry-list.service";
import type {TabContainerComponent} from "tim/sidebarmenu/tab-container.component";
import {isScreenSizeOrLower, TimStorage} from "tim/util/utils";
import type {IDocumentGlobals, ISettings} from "tim/util/globals";
import {genericglobals, isDocumentGlobals} from "tim/util/globals";
import type {IVisibilityVars} from "tim/timRoot";
import {getVisibilityVars} from "tim/timRoot";
import * as t from "io-ts";

enum MenuState {
    Open,
    Icons,
    Closed,
    Max,
}

// Map of menu state names to menu state
const MENU_STATE_MAP: Record<string, MenuState> = Object.fromEntries(
    Object.entries(MenuState)
        .filter(([_, v]) => typeof v === "number")
        .map(([k, v]) => [k.toLowerCase(), v as MenuState])
);

interface UpdateVisStateOptions {
    tabs?: boolean;
    lastVisState?: boolean;
}
const UPDATE_ALL: UpdateVisStateOptions = {lastVisState: true, tabs: true};

const MENU_BUTTON_ICONS: Record<MenuState, string> = {
    [MenuState.Open]: "menu-hamburger",
    [MenuState.Icons]: "option-horizontal",
    [MenuState.Closed]: "menu-left",
    [MenuState.Max]: "",
};

const sizeBreakpoint = "md";

@Component({
    selector: "tim-sidebar-menu",
    template: `
        <div *ngIf="!hide.sidebar && tabsVisible" (window:resize)="onResize()" class="left-fixed-side" [class.show]="showMenu">
            <div class="btn btn-default btn-sm pull-left" (click)="nextVisibilityState()" i18n-title title="Show menu">
                <i class="glyphicon glyphicon-{{nextGlyphicon}}" i18n-title title="Open sidebar"></i>
            </div>
            <ng-container *ngIf="showTabset">
                <tim-switch-button></tim-switch-button>
            </ng-container>
            <tabset id="menuTabs" [class.show]="showTabset" #tabs>
                <ng-container *ngFor="let menuTab of menuTabs">
                    <tab *ngIf="tabsVisTable[menuTab.id]"
                         [id]="menuTab.id"
                         (selectTab)="onTabSelect($event, tabContainer)">
                        <ng-template tabHeading>
                            <i class="glyphicon glyphicon-{{menuTab.icon}}" title="{{menuTab.title}}"></i>
                            <span class="sr-only">Open {{menuTab.title}}</span>
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
    private lastUsedTabStorage = new TimStorage(
        "sideBarMenu_lastUsedTab",
        t.string
    );
    private lastVisStateStorage = new TimStorage(
        "sideBarMenu_lastVisState",
        t.number
    );
    private currentTab?: string;
    private settings: ISettings = genericglobals().userPrefs;
    private docGlobals?: IDocumentGlobals;
    private currentMenuState: MenuState = MenuState.Open;
    private isSm = isScreenSizeOrLower(sizeBreakpoint);
    private lastNonSmState = this.lastVisState;
    @ViewChild("tabs") private tabs!: TabsetComponent;
    hide: IVisibilityVars = getVisibilityVars();
    menuTabs!: TabEntry[];
    tabsVisTable: Record<string, boolean> = {};
    nextGlyphicon: string = MENU_BUTTON_ICONS[this.nextState];
    tabsVisible = false;

    constructor(private tabEntryList: TabEntryListService) {
        const g = genericglobals();
        if (isDocumentGlobals(g)) {
            this.docGlobals = g;
        }
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
        const val = this.lastUsedTabStorage.get();
        if (val === undefined) {
            return "";
        }
        return val;
    }

    set lastUsedTab(value: string) {
        if (!this.settings.remember_last_sidebar_menu_tab) {
            return;
        }
        this.lastUsedTabStorage.set(value);
    }

    get lastVisState(): MenuState {
        if (this.docGlobals?.docSettings.sidemenu_initial_state) {
            const state =
                MENU_STATE_MAP[
                    this.docGlobals.docSettings.sidemenu_initial_state
                ];
            if (state !== undefined) {
                return state;
            }
        }
        if (!this.settings.remember_last_sidebar_menu_state) {
            return MenuState.Open;
        }
        const val = this.lastVisStateStorage.get();
        if (val === undefined || val < 0 || val >= MenuState.Max) {
            return MenuState.Open;
        }
        return val;
    }

    set lastVisState(value: MenuState) {
        if (!this.settings.remember_last_sidebar_menu_state) {
            return;
        }
        if (value < 0 || value >= MenuState.Max) {
            value = MenuState.Open;
        }
        this.lastVisStateStorage.set(value);
    }

    onResize(): void {
        if (!this.isSm && isScreenSizeOrLower(sizeBreakpoint)) {
            this.isSm = true;
            this.lastNonSmState = this.currentMenuState;
            this.setVisibleState(MenuState.Closed, {tabs: true});
        } else if (this.isSm && !isScreenSizeOrLower(sizeBreakpoint)) {
            this.isSm = false;
            this.setVisibleState(this.lastNonSmState, {tabs: true});
        }
    }

    ngOnInit(): void {
        this.currentTab = this.lastUsedTab;
        this.menuTabs = this.tabEntryList.getTabEntries();
        for (const tab of this.menuTabs) {
            this.tabsVisTable[tab.id] = tab.visible();
        }
        this.updateTabsVisible();
    }

    ngDoCheck() {
        // Previously, old sidebar menu could make do with ng-ifs to detect which one to show.
        // Here, to simplify code, we use data-based approach to defining tabs.
        // Because of that, we need to implement custom change detection.
        // Therefore, the visible() checks should generally be very small and fast.
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
    }

    ngAfterViewInit() {
        let initialViewState = this.lastVisState;
        const someTabSelected = this.trySetCurrentTabToDefault();
        if (isScreenSizeOrLower(sizeBreakpoint)) {
            initialViewState = MenuState.Closed;
        } else if (!someTabSelected) {
            initialViewState = MenuState.Icons;
        }
        this.setVisibleState(initialViewState, UPDATE_ALL);
    }

    onTabSelect(tab: TabDirective, tabContainer: TabContainerComponent) {
        this.currentTab = tab.id;
        this.lastUsedTab = this.currentTab ?? "";
        this.setVisibleState(MenuState.Open, {lastVisState: true});
        void tabContainer.onSelect();
    }

    shouldRender(tabId: string) {
        return this.currentTab == tabId;
    }

    nextVisibilityState() {
        this.setVisibleState(this.nextState, UPDATE_ALL);
    }

    private setVisibleState(
        newState: MenuState,
        updateOpts?: UpdateVisStateOptions
    ) {
        this.currentMenuState = newState;
        if (updateOpts?.lastVisState) {
            this.lastVisState = newState;
        }
        this.nextGlyphicon = MENU_BUTTON_ICONS[this.nextState];
        if (updateOpts?.tabs) {
            this.updateTabs();
        }
    }

    private trySetCurrentTabToDefault() {
        if (this.currentTab && this.tabsVisTable[this.currentTab]) {
            return true;
        }
        const firstDefaultTab = this.tabEntryList.defaultTabOrder.find(
            (id) => this.tabsVisTable[id]
        );
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
        this.tabs.tabs.forEach((tab) => (tab.active = false));
        if (this.currentMenuState != MenuState.Open) {
            return;
        }
        const activeTab = this.tabs.tabs.find(
            (tab) => tab.id == this.currentTab
        );
        if (activeTab) {
            activeTab.active = true;
        }
    }

    private tabVisibilityChanged() {
        let nextState = MenuState.Open;
        if (!this.trySetCurrentTabToDefault() || !this.currentTab) {
            nextState = MenuState.Icons;
        }
        this.setVisibleState(nextState, UPDATE_ALL);
        this.updateTabsVisible();
    }

    private get nextMobileState(): MenuState {
        return this.currentMenuState == MenuState.Open
            ? MenuState.Closed
            : MenuState.Open;
    }

    private get nextDesktopState(): MenuState {
        return (this.currentMenuState + 1) % MenuState.Max;
    }

    private get nextState() {
        return isScreenSizeOrLower(sizeBreakpoint)
            ? this.nextMobileState
            : this.nextDesktopState;
    }

    private updateTabsVisible() {
        this.tabsVisible = this.menuTabs.some(
            (tab) => this.tabsVisTable[tab.id]
        );
    }
}
