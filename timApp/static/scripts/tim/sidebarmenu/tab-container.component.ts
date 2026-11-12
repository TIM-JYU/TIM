import type {ComponentRef} from "@angular/core";
import {
    Component,
    Input,
    ViewChild,
    ChangeDetectionStrategy,
} from "@angular/core";
import type {OnTabSelect} from "tim/sidebarmenu/menu-tab.directive";
import {TabEntry, MenuTabDirective} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "tab-container",
    template: `
        <ng-template timMenuTab></ng-template>
    `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class TabContainerComponent {
    @Input() tabItem!: TabEntry;
    @ViewChild(MenuTabDirective, {static: true}) timMenuTab!: MenuTabDirective;
    private tabComponent?: ComponentRef<unknown>;

    private initComponent() {
        this.tabComponent = this.timMenuTab.vcr.createComponent(
            this.tabItem.component
        );
    }

    private static hasOnSelect(inst: unknown): inst is OnTabSelect {
        return (
            typeof inst == "object" &&
            inst != null &&
            typeof (inst as Record<string, unknown>).onSelect == "function"
        );
    }

    onSelect() {
        if (!this.tabComponent) {
            this.initComponent();
        }
        if (TabContainerComponent.hasOnSelect(this.tabComponent?.instance)) {
            this.tabComponent?.instance.onSelect();
        }
    }
}
