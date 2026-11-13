import type {OnDestroy, OnInit, OutputRefSubscription} from "@angular/core";
import {Directive} from "@angular/core";
import {TabDirective} from "ngx-bootstrap/tabs";

/**
 * Keeps a ngx-bootstrap tab closed until the user opens it:
 *
 * `<tab heading="Create" timStartClosed>`
 *
 * Since version 20, ngx-bootstrap opens the first tab of a tabset by itself right after the tabs
 * are created, even if the tab has `[active]="false"`. This directive closes the tab again if it
 * was opened that way. Import this directive together with `TabsModule`.
 */
@Directive({
    selector: "tab[timStartClosed]",
    standalone: true,
})
export class TabStartClosedDirective implements OnInit, OnDestroy {
    private sub?: OutputRefSubscription;
    private timer?: number;

    constructor(private tab: TabDirective) {}

    ngOnInit() {
        // ngx-bootstrap opens the default tab in a microtask,
        // so anything selected before the next macrotask was not selected by the user.
        this.sub = this.tab.selectTab.subscribe(() => {
            this.tab.active = false;
        });
        this.timer = window.setTimeout(() => this.stopWatching());
    }

    ngOnDestroy() {
        window.clearTimeout(this.timer);
        this.stopWatching();
    }

    private stopWatching() {
        this.sub?.unsubscribe();
        this.sub = undefined;
    }
}
