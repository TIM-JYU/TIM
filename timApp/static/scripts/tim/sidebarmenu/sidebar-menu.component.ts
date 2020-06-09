import {AfterViewInit, Component, OnInit, ViewChild} from "@angular/core";
import {TabDirective, TabsetComponent} from "ngx-bootstrap/tabs";
import {IMenuTab} from "tim/sidebarmenu/menu-tab.directive";
import {BookmarksTabComponent} from "tim/sidebarmenu/tabs/bookmarks-tab.component";

@Component({
    selector: "app-sidebar-menu",
    template: `
        <div class="left-fixed-side" [ngStyle]="{'min-width': sidebarWidth}">
            <div class="btn btn-default btn-sm pull-left" (click)="toggleSidebar()" i18n-title title="Show menu">
                <i class="glyphicon glyphicon-menu-hamburger" i18n-title title="Click to open sidebar-menu"></i>
            </div>
            <tabset id="menuTabs" [class.hidden-sm]="hidden" [class.hidden-xs]="hidden" #tabs>
                <tab *ngFor="let menuTab of menuTabs" (selectTab)="onTabSelect($event)" #currentTab>
                    <ng-template tabHeading>
                        <i class="glyphicon glyphicon-{{menuTab.icon}}" i18n-title title="{{menuTab.title}}"></i>
                    </ng-template>
                    <tab-container [tabItem]="menuTab" [class.hidden]="!shouldRender(currentTab)"></tab-container>
                </tab>
            </tabset>
        </div>
    `,
})
export class SidebarMenuComponent implements OnInit, AfterViewInit {
    hidden = false;
    sidebarWidth = "12em";
    private showSidebar = true;
    // TODO: Ability to set default tab
    private currentElement?: HTMLElement;
    @ViewChild("tabs") private tabs!: TabsetComponent;

    menuTabs: IMenuTab[] = [
        {
            tabType: BookmarksTabComponent,
            icon: "bookmark",
            title: "Bookmarks",
        },
    ];

    constructor() { }

    ngOnInit(): void {
    }

    ngAfterViewInit() {
        this.setSidebarState(false);
    }

    onTabSelect(tab: TabDirective) {
        this.showSidebar = true;
        this.currentElement = tab.elementRef.nativeElement as HTMLElement;
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
        if (!this.showSidebar) {
            this.sidebarWidth = "0";
        } else {
            this.sidebarWidth = "12em";
        }
    }
}
