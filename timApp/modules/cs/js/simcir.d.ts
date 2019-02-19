declare module "simcir" {
    function setupSimcir(element: JQuery, data: {}): void;

    function controller(element: JQuery): {data(): {connectors: any[], devices: any[]}};
}
declare module "simcir/basicset";
declare module "simcir/library";
declare module "simcir/oma-kirjasto";
