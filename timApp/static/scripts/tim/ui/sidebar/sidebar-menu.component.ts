import {AfterViewInit, Component, OnInit, ViewChild} from "@angular/core";
import {TabDirective, TabsetComponent} from "ngx-bootstrap/tabs";

@Component({
    selector: "app-sidebar-menu",
    templateUrl: "./sidebar-menu.component.html",
})
export class SidebarMenuComponent implements OnInit, AfterViewInit {
    hidden = false;
    sidebarWidth = "12em";
    private showSidebar = true;
    // TODO: Ability to set default tab
    private currentElement?: HTMLElement;
    @ViewChild("tabs") private tabs!: TabsetComponent;
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
