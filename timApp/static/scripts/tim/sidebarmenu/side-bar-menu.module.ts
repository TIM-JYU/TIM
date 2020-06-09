import { NgModule } from "@angular/core";
import { CommonModule } from "@angular/common";
import {TabsModule} from "ngx-bootstrap/tabs";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {SidebarMenuComponent} from "tim/sidebarmenu/sidebar-menu.component";
import { BookmarksTabComponent } from "./tabs/bookmarks-tab.component";
import { MenuTabDirective } from "./menu-tab.directive";

@NgModule({
    declarations: [
        SidebarMenuComponent,
        BookmarksTabComponent,
        MenuTabDirective,
    ],
    imports: [
        CommonModule,
        TabsModule.forRoot(),
        TooltipModule.forRoot(),
    ],
})
export class SideBarMenuModule { }
