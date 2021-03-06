export class TimDefer<T> {
    public promise: Promise<T>;

    // These are initialized by the Promise constructor.
    public reject!: (reason?: unknown) => void;
    public resolve!: (value: T | PromiseLike<T>) => void;

    constructor() {
        this.promise = new Promise<T>((resolve, reject) => {
            this.resolve = resolve;
            this.reject = reject;
        });
    }
}
