// eslint-disable-next-line @typescript-eslint/ban-types
export type ReadonlyWeakMap<K extends object, V> = Readonly<
    Omit<WeakMap<K, V>, "delete" | "set">
>;
