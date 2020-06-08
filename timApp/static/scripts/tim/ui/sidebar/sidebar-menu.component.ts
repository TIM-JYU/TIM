import {AfterViewInit, Component, OnInit, ViewChild} from "@angular/core";
import {TabDirective, TabsetComponent} from "ngx-bootstrap/tabs";

@Component({
    selector: "app-sidebar-menu",
    templateUrl: "./sidebar-menu.component.html",
    styleUrls: ["./sidebar-menu.component.scss"],
})
export class SidebarMenuComponent implements OnInit, AfterViewInit {
    private showSidebar = false;
    currentElement?: HTMLElement;
    @ViewChild("tabs") tabs!: TabsetComponent;
    constructor() { }

    ngOnInit(): void {
    }

    ngAfterViewInit() {
        // By default, ngx-bootstrap tabset has first tab selected, so we
        // unselect a tab on PC
        // TODO: Replicate proper mobile behaviour
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
    }
}
