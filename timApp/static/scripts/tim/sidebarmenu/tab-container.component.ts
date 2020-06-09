import {Component, ComponentFactoryResolver, Input, OnInit, ViewChild} from "@angular/core";
import {TabEntry, MenuTabDirective} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "tab-container",
    template: `
        <ng-template timMenuTab></ng-template>
    `,
})
export class TabContainerComponent implements OnInit {
    @Input() tabItem!: TabEntry;
    @ViewChild(MenuTabDirective, { static: true }) timMenuTab!: MenuTabDirective;

    constructor(private cfr: ComponentFactoryResolver) { }

    ngOnInit(): void {
        const factory = this.cfr.resolveComponentFactory(this.tabItem.tabType);
        const vcr = this.timMenuTab.vcr;
        const componentRef = vcr.createComponent(factory);
        componentRef.instance.entry = this.tabItem;
    }
}
