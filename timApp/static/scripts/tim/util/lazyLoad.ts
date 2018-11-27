export function lazyLoad<T>(moduleName: string): Promise<T> {
    return new Promise<T>((resolve, reject) => {
        SystemJS.amdRequire([moduleName], (module) => {
            resolve(module);
        });
    });
}

export function lazyLoadMany(moduleNames: string[]): Promise<any[]> {
    return new Promise<any[]>((resolve, reject) => {
        if (moduleNames.length === 0) {
            resolve([]);
        }
        SystemJS.amdRequire(moduleNames, (...args) => {
            resolve(args);
        });
    });
}
