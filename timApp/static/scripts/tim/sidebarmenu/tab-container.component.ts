import {Component, ComponentFactoryResolver, ComponentRef, Input, OnInit, ViewChild} from "@angular/core";
import {TabEntry, MenuTabDirective, IMenuTab} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "tab-container",
    template: `
        <ng-template timMenuTab></ng-template>
    `,
})
export class TabContainerComponent {
    @Input() tabItem!: TabEntry;
    @ViewChild(MenuTabDirective, {static: true}) timMenuTab!: MenuTabDirective;
    private tabComponent?: ComponentRef<IMenuTab>;

    constructor(private cfr: ComponentFactoryResolver) {
    }

    private async initComponent() {
        const factory = this.cfr.resolveComponentFactory(await this.tabItem.importComponent());
        this.tabComponent = this.timMenuTab.vcr.createComponent(factory);
        this.tabComponent.instance.entry = this.tabItem;
    }

    async onSelect() {
        if (!this.tabComponent) {
            await this.initComponent();
        }
        const onSelect = this.tabComponent?.instance.onSelect;
        if (onSelect) {
            onSelect.apply(this.tabComponent?.instance);
        }
    }
}
