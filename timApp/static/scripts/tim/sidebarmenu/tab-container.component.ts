import {Component, ComponentFactoryResolver, ComponentRef, Input, OnInit, ViewChild} from "@angular/core";
import {TabEntry, MenuTabDirective, IMenuTab} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "tab-container",
    template: `
        <ng-template timMenuTab></ng-template>
    `,
})
export class TabContainerComponent implements OnInit {
    @Input() tabItem!: TabEntry;
    @ViewChild(MenuTabDirective, {static: true}) timMenuTab!: MenuTabDirective;
    private tabComponent?: ComponentRef<IMenuTab>;

    constructor(private cfr: ComponentFactoryResolver) {
    }

    ngOnInit(): void {
        const factory = this.cfr.resolveComponentFactory(this.tabItem.tabType);
        this.tabComponent = this.timMenuTab.vcr.createComponent(factory);
        this.tabComponent.instance.entry = this.tabItem;
    }

    onSelect() {
        const onSelect = this.tabComponent?.instance.onSelect;
        if (onSelect) {
            onSelect();
        }
    }
}
