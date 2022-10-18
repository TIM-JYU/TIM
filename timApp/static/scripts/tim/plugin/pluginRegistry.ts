import type {Type} from "@angular/core";
import type {PluginJson} from "tim/plugin/angular-plugin-base.directive";

/**
 * Information about a registered plugin.
 */
export interface IRegisteredPlugin {
    /**
     * Angular module to which the plugin belongs.
     */
    module: Type<unknown>;

    /**
     * Angular component for the plugin.
     * Component must implement PluginJson interface for receiving the data from the server.
     */
    component: Type<PluginJson>;
}

const pluginMap = new Map<string, IRegisteredPlugin>();

/**
 * Register a plugin component with the plugin registry.
 *
 * Registered plugins can be dynamically loaded by the plugin loader.
 *
 * @param pluginSelector The selector of the plugin component used for referencing the plugin component.
 * @param module The component to which the plugin component belongs. Used for loading the module dependencies.
 * @param component The plugin component to register.
 */
export function registerPlugin(
    pluginSelector: string,
    module: Type<unknown>,
    component: Type<PluginJson>
) {
    pluginMap.set(pluginSelector, {module, component});
}

/**
 * Get plugin information from the plugin registry.
 *
 * @param pluginSelector The selector of the plugin component used for referencing the plugin component.
 */
export function getPlugin(
    pluginSelector: string
): IRegisteredPlugin | undefined {
    return pluginMap.get(pluginSelector);
}
