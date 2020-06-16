import {AfterViewInit, Component, DoCheck, OnInit, ViewChild} from "@angular/core";
import {TabDirective, TabsetComponent} from "ngx-bootstrap/tabs";
import {TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import {TabEntryListService} from "tim/sidebarmenu/services/tab-entry-list.service";
import {TabContainerComponent} from "tim/sidebarmenu/tab-container.component";

@Component({
    selector: "app-sidebar-menu",
    template: `
        <div class="left-fixed-side" [class.show]="showSidebar">
            <div class="btn btn-default btn-sm pull-left" (click)="toggleSidebar()" i18n-title title="Show menu">
                <i class="glyphicon glyphicon-menu-hamburger" i18n-title title="Click to open sidebar-menu"></i>
            </div>
            <tabset id="menuTabs" [class.hidden-sm]="hidden" [class.hidden-xs]="hidden" #tabs>
                <ng-container *ngFor="let menuTab of menuTabs">
                    <tab *ngIf="tabsVisTable[menuTab.title]" (selectTab)="onTabSelect($event, tabContainer)"
                         #currentTab>
                        <ng-template tabHeading>
                            <i class="glyphicon glyphicon-{{menuTab.icon}}" i18n-title title="{{menuTab.title}}"></i>
                        </ng-template>
                        <tab-container #tabContainer [tabItem]="menuTab"
                                       [class.hidden]="!shouldRender(currentTab)"></tab-container>
                    </tab>
                </ng-container>
            </tabset>
        </div>
    `,
})
export class SidebarMenuComponent implements OnInit, AfterViewInit, DoCheck {
    hidden = true;
    showSidebar = true;
    // TODO: Ability to set default tab
    private currentElement?: HTMLElement;
    @ViewChild("tabs") private tabs!: TabsetComponent;
    menuTabs!: TabEntry[];
    tabsVisTable: Record<string, boolean> = {};

    constructor(private tabEntryList: TabEntryListService) {
    }

    ngOnInit(): void {
        this.menuTabs = this.tabEntryList.getTabEntries();
        for (const tab of this.menuTabs) {
            this.tabsVisTable[tab.title] = tab.visible();
        }
    }

    ngDoCheck() {
        let shouldSet = false;
        const visTabs: Record<string, boolean> = {};
        for (const tab of this.menuTabs) {
            const isVisible = tab.visible();
            visTabs[tab.title] = isVisible;
            if (isVisible != this.tabsVisTable[tab.title]) {
                shouldSet = true;
            }
        }
        if (shouldSet) {
            this.tabsVisTable = visTabs;
        }
    }

    ngAfterViewInit() {
        this.setSidebarState(false);
    }

    onTabSelect(tab: TabDirective, ew: TabContainerComponent) {
        this.showSidebar = true;
        this.currentElement = tab.elementRef.nativeElement as HTMLElement;
        ew.onSelect();
    }

    shouldRender(tab: HTMLElement) {
        return this.currentElement == tab;
    }

    private setSidebarState(visible: boolean) {
        this.showSidebar = visible;

        if (!this.tabs) {
            return;
        }
        for (const tab of this.tabs.tabs) {
            if (!this.showSidebar) {
                tab.active = false;
            } else if (tab.elementRef.nativeElement == this.currentElement) {
                tab.active = true;
            }
        }
    }

    toggleSidebar() {
        this.setSidebarState(!this.showSidebar);
        this.hidden = !this.showSidebar;
    }
}
