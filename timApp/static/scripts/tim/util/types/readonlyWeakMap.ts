export type ReadonlyWeakMap<K extends object, V> = Readonly<
    Omit<WeakMap<K, V>, "delete" | "set">
>;
