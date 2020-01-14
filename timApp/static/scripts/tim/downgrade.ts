import {NgModuleRef, StaticProvider, Type} from "@angular/core";
import {downgradeComponent, downgradeModule} from "@angular/upgrade/static";
import angular, {IDirectiveFactory, IModule, Injectable} from "angular";

export function doDowngrade(m: IModule, component: string, angularComponent: Type<unknown>) {
    m.directive(component, downgradeComponent({
        component: angularComponent,
        downgradedModule: m.name,
    }) as Injectable<IDirectiveFactory>);
}


export function createDowngradedModule<T>(b: (extraProviders: StaticProvider[]) => Promise<NgModuleRef<T>>) {
    return angular.module(downgradeModule(b));
}
