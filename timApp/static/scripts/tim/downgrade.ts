import type {NgModuleRef, StaticProvider, Type} from "@angular/core";
import {downgradeComponent, downgradeModule} from "@angular/upgrade/static";
import type {IDirectiveFactory, IModule, Injectable} from "angular";
import angular from "angular";

export enum Digest {
    Propagate,
    DontPropagate,
}

export function doDowngrade(
    m: IModule,
    component: string,
    angularComponent: Type<unknown>,
    propagateDigest = Digest.DontPropagate
) {
    m.directive(
        component,
        downgradeComponent({
            component: angularComponent,
            downgradedModule: m.name,
            propagateDigest: propagateDigest == Digest.Propagate,
        }) as Injectable<IDirectiveFactory>
    );
    return m;
}

export function createDowngradedModule<T>(
    b: (extraProviders: StaticProvider[]) => Promise<NgModuleRef<T>>
) {
    return angular.module(downgradeModule(b));
}
