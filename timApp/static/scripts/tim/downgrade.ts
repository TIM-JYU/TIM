import {StaticProvider, Type} from "@angular/core";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {AppModule} from "tim/app.module";
import {downgradeComponent, downgradeModule, setAngularJSGlobal} from "@angular/upgrade/static";
import angular, {IDirectiveFactory, Injectable} from "angular";
import {HeaderComponent} from "tim/header/header.component";
import {CreateItemComponent} from "tim/item/create-item.component";
import {TimAlertComponent} from "tim/ui/tim-alert.component";

function createDowngradedModule() {
    const bootstrapFn = (extraProviders: StaticProvider[]) => {
        const platformRef = platformBrowserDynamic(extraProviders);
        return platformRef.bootstrapModule(AppModule);
    };

    setAngularJSGlobal(angular);
    const dg = angular.module(downgradeModule(bootstrapFn));

    function doDowngrade(component: string, angularComponent: Type<unknown>) {
        dg.directive(component, downgradeComponent({
            component: angularComponent,
        }) as Injectable<IDirectiveFactory>);
    }

    doDowngrade("timHeader", HeaderComponent);
    doDowngrade("createItem", CreateItemComponent);
    doDowngrade("timAlert", TimAlertComponent);
    return dg;
}

export const downgradedModule = createDowngradedModule();
