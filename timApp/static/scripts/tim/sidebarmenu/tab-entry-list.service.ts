import { Injectable } from "@angular/core";
import {TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import {BookmarksTabComponent} from "tim/sidebarmenu/tabs/bookmarks-tab.component";

@Injectable({
  providedIn: "root",
})
export class TabEntryListService {

  constructor() { }

  getTabEntries(): TabEntry[] {
      return [
        {
            tabType: BookmarksTabComponent,
            icon: "bookmark",
            title: "Bookmarks",
        },
    ];;
  }
}
