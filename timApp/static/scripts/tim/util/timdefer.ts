export class TimDefer<T> {
    public promise: Promise<T>;

    // These are initialized by the Promise constructor.
    public reject!: (reason?: unknown) => void;
    public resolve!: <T>(value?: (T | PromiseLike<T> | undefined)) => void;

    constructor() {
        this.promise = new Promise<T>((resolve, reject) => {
            this.resolve = resolve as <T>(value?: (T | PromiseLike<T> | undefined)) => void;
            this.reject = reject;
        });
    }
}
