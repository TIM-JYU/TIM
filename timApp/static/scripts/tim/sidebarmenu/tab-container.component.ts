import {
    Component,
    ComponentFactoryResolver,
    ComponentRef,
    Input,
    ViewChild,
} from "@angular/core";
import {TabEntry, MenuTabDirective, OnTabSelect} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "tab-container",
    template: `
        <ng-template timMenuTab></ng-template>
    `,
})
export class TabContainerComponent {
    @Input() tabItem!: TabEntry;
    @ViewChild(MenuTabDirective, {static: true}) timMenuTab!: MenuTabDirective;
    private tabComponent?: ComponentRef<unknown>;

    constructor(private cfr: ComponentFactoryResolver) {
    }

    private initComponent() {
        const factory = this.cfr.resolveComponentFactory(this.tabItem.component);
        this.tabComponent = this.timMenuTab.vcr.createComponent(factory);
    }

    private static hasOnSelect(inst: unknown): inst is OnTabSelect {
        return typeof inst == "object"
            && inst != null
            && typeof (inst as Record<string, unknown>).onSelect == "function";
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
