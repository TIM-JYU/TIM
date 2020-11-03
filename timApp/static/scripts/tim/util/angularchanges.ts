/**
 * A more accurate type for ngOnChanges parameter.
 */
export type Changes<Component, Inputs extends keyof Component> = {
    [K in Inputs]: Change<Component, K> | undefined;
};

interface Change<Component, Input extends keyof Component> {
    currentValue: Component[Input];
    previousValue: Component[Input];
    isFirstChange(): boolean;
}
