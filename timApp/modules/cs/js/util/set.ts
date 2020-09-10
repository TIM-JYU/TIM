abstract class SetBase<T> implements Iterable<T> {
    constructor(
        protected getKey: (obj: T) => string = (o: {toString: () => string}) =>
            o.toString()
    ) {}

    abstract getByKey(key: string): T | undefined;

    get(obj: T): T | undefined {
        return this.getByKey(this.getKey(obj));
    }

    abstract push(obj: T): void;

    abstract removeByKey(key: string): T | undefined;

    remove(obj: T): T | undefined {
        return this.removeByKey(this.getKey(obj));
    }

    abstract toArray(): T[];

    abstract get length(): number;

    *[Symbol.iterator](): Iterator<T> {
        for (const item of this.toArray()) {
            yield item;
        }
    }
}

export class Set<T> extends SetBase<T> {
    private items: {[key: string]: T} = {};

    getByKey(key: string): T | undefined {
        return this.items[key];
    }

    push(...objs: T[]): void {
        for (const obj of objs) {
            this.items[this.getKey(obj)] = obj;
        }
    }

    removeByKey(key: string): T | undefined {
        const obj = this.items[key];
        delete this.items[key];
        return obj;
    }

    clear() {
        this.items = {};
    }

    toArray(): T[] {
        return Object.values(this.items);
    }

    get length(): number {
        return Object.keys(this.items).length;
    }
}

export class OrderedSet<T> extends SetBase<T> {
    private indices: {[key: string]: number} = {};
    private items: T[] = [];

    getByIndex(index: number): T | undefined {
        return this.items[index];
    }

    getIndexByKey(key: string): number {
        return this.indices[key] ?? -1;
    }

    getIndex(obj: T): number {
        return this.getIndexByKey(this.getKey(obj));
    }

    getByKey(key: string): T | undefined {
        return this.getByIndex(this.getIndexByKey(key));
    }

    push(...objs: T[]): void {
        for (const obj of objs) {
            const key = this.getKey(obj);
            let index = this.indices[key];
            if (index === undefined) {
                index = this.items.length;
                this.indices[key] = index;
            }
            this.items[index] = obj;
        }
    }

    removeByKey(key: string): T | undefined {
        const index = this.getIndexByKey(key);
        let obj: T | undefined;
        if (index != -1) {
            obj = this.items[index];
            this.items.splice(index);
            delete this.indices[key];
        }
        return obj;
    }

    clear() {
        this.indices = {};
        this.items = [];
    }

    toArray(): T[] {
        return this.items;
    }

    get length(): number {
        return this.items.length;
    }
}
