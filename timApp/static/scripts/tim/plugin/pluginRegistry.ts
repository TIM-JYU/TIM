import {Type} from "@angular/core";
import {PluginJson} from "tim/plugin/angular-plugin-base.directive";

export interface IRegisteredPlugin {
    module: Type<unknown>;
    component: Type<PluginJson>;
}

const pluginMap = new Map<string, IRegisteredPlugin>();

export function registerPlugin(
    pluginSelector: string,
    module: Type<unknown>,
    component: Type<PluginJson>
) {
    pluginMap.set(pluginSelector, {module, component});
}

export function getPlugin(
    pluginSelector: string
): IRegisteredPlugin | undefined {
    return pluginMap.get(pluginSelector);
}
