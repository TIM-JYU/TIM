// idea from https://github.com/bcherny/ngimport
import * as angular from "angular";
import {ILazyLoad} from "oclazyload";
import {timApp} from "tim/app";

class Services {
    private static checkServiceAccess(service) {
        if (service == null) {
            throw new Error("You cannot access Angular services before angular.bootstrap has been called");
        }
    }

    private static checkServiceSet(service) {
        if (service != null) {
            throw new Error("You cannot reassign services");
        }
    }

    private anchorScroll: angular.IAnchorScrollService = null;
    private cacheFactory: angular.ICacheFactoryService = null;
    private compile: angular.ICompileService = null;
    private controller: angular.IControllerService = null;
    private document: angular.IDocumentService = null;
    private exceptionHandler: angular.IExceptionHandlerService = null;
    private filter: angular.IFilterService = null;
    private http: angular.IHttpService = null;
    private httpBackend: angular.IHttpBackendService = null;
    private httpParamSerializer: angular.IHttpParamSerializer = null;
    private httpParamSerializerJQLike: angular.IHttpParamSerializer = null;
    private httpProvider: angular.IHttpProvider = null;
    private injector: angular.auto.IInjectorService = null;
    private interpolate: angular.IInterpolateService = null;
    private interval: angular.IIntervalService = null;
    private locale: angular.ILocaleService = null;
    private location: angular.ILocationService = null;
    private log: angular.ILogService = null;
    private logProvider: angular.ILogProvider = null;
    private ocLazyLoad: ILazyLoad = null;
    private parse: angular.IParseService = null;
    private provide: angular.auto.IProvideService = null;
    private q: angular.IQService = null;
    private rootElement: angular.IRootElementService = null;
    private rootScope: angular.IRootScopeService = null;
    private sce: angular.ISCEService = null;
    private sceDelegate: angular.ISCEDelegateService = null;
    private templateCache: angular.ITemplateCacheService = null;
    private templateRequest: angular.ITemplateRequestService = null;
    private timeout: angular.ITimeoutService = null;
    private window: angular.IWindowService = null;
    private xhrFactory: angular.IXhrFactory<any> = null;

    get $anchorScroll() {
        Services.checkServiceAccess(this.anchorScroll);
        return this.anchorScroll;
    }

    set $anchorScroll(service: angular.IAnchorScrollService) {
        Services.checkServiceSet(this.anchorScroll);
        this.anchorScroll = service;
    }

    get $cacheFactory() {
        Services.checkServiceAccess(this.cacheFactory);
        return this.cacheFactory;
    }

    set $cacheFactory(service: angular.ICacheFactoryService) {
        Services.checkServiceSet(this.cacheFactory);
        this.cacheFactory = service;
    }

    get $compile() {
        Services.checkServiceAccess(this.compile);
        return this.compile;
    }

    set $compile(service: angular.ICompileService) {
        Services.checkServiceSet(this.compile);
        this.compile = service;
    }

    get $controller() {
        Services.checkServiceAccess(this.controller);
        return this.controller;
    }

    set $controller(service: angular.IControllerService) {
        Services.checkServiceSet(this.controller);
        this.controller = service;
    }

    get $document() {
        Services.checkServiceAccess(this.document);
        return this.document;
    }

    set $document(service: angular.IDocumentService) {
        Services.checkServiceSet(this.document);
        this.document = service;
    }

    get $exceptionHandler() {
        Services.checkServiceAccess(this.exceptionHandler);
        return this.exceptionHandler;
    }

    set $exceptionHandler(service: angular.IExceptionHandlerService) {
        Services.checkServiceSet(this.exceptionHandler);
        this.exceptionHandler = service;
    }

    get $filter() {
        Services.checkServiceAccess(this.filter);
        return this.filter;
    }

    set $filter(service: angular.IFilterService) {
        Services.checkServiceSet(this.filter);
        this.filter = service;
    }

    get $http() {
        Services.checkServiceAccess(this.http);
        return this.http;
    }

    set $http(service: angular.IHttpService) {
        Services.checkServiceSet(this.http);
        this.http = service;
    }

    get $httpBackend() {
        Services.checkServiceAccess(this.httpBackend);
        return this.httpBackend;
    }

    set $httpBackend(service: angular.IHttpBackendService) {
        Services.checkServiceSet(this.httpBackend);
        this.httpBackend = service;
    }

    get $httpParamSerializer() {
        Services.checkServiceAccess(this.httpParamSerializer);
        return this.httpParamSerializer;
    }

    set $httpParamSerializer(service: angular.IHttpParamSerializer) {
        Services.checkServiceSet(this.httpParamSerializer);
        this.httpParamSerializer = service;
    }

    get $httpParamSerializerJQLike() {
        Services.checkServiceAccess(this.httpParamSerializerJQLike);
        return this.httpParamSerializerJQLike;
    }

    set $httpParamSerializerJQLike(service: angular.IHttpParamSerializer) {
        Services.checkServiceSet(this.httpParamSerializerJQLike);
        this.httpParamSerializerJQLike = service;
    }

    get $httpProvider() {
        Services.checkServiceAccess(this.httpProvider);
        return this.httpProvider;
    }

    set $httpProvider(service: angular.IHttpProvider) {
        Services.checkServiceSet(this.httpProvider);
        this.httpProvider = service;
    }

    get $injector() {
        Services.checkServiceAccess(this.injector);
        return this.injector;
    }

    set $injector(service: angular.auto.IInjectorService) {
        Services.checkServiceSet(this.injector);
        this.injector = service;
    }

    get $interpolate() {
        Services.checkServiceAccess(this.interpolate);
        return this.interpolate;
    }

    set $interpolate(service: angular.IInterpolateService) {
        Services.checkServiceSet(this.interpolate);
        this.interpolate = service;
    }

    get $interval() {
        Services.checkServiceAccess(this.interval);
        return this.interval;
    }

    set $interval(service: angular.IIntervalService) {
        Services.checkServiceSet(this.interval);
        this.interval = service;
    }

    get $locale() {
        Services.checkServiceAccess(this.locale);
        return this.locale;
    }

    set $locale(service: angular.ILocaleService) {
        Services.checkServiceSet(this.locale);
        this.locale = service;
    }

    get $location() {
        Services.checkServiceAccess(this.location);
        return this.location;
    }

    set $location(service: angular.ILocationService) {
        Services.checkServiceSet(this.location);
        this.location = service;
    }

    get $log() {
        Services.checkServiceAccess(this.log);
        return this.log;
    }

    set $log(service: angular.ILogService) {
        Services.checkServiceSet(this.log);
        this.log = service;
    }

    get $logProvider() {
        Services.checkServiceAccess(this.logProvider);
        return this.logProvider;
    }

    set $logProvider(service: angular.ILogProvider) {
        Services.checkServiceSet(this.logProvider);
        this.logProvider = service;
    }

    get $ocLazyLoad() {
        Services.checkServiceAccess(this.ocLazyLoad);
        return this.ocLazyLoad;
    }

    set $ocLazyLoad(service: ILazyLoad) {
        Services.checkServiceSet(this.ocLazyLoad);
        this.ocLazyLoad = service;
    }

    get $parse() {
        Services.checkServiceAccess(this.parse);
        return this.parse;
    }

    set $parse(service: angular.IParseService) {
        Services.checkServiceSet(this.parse);
        this.parse = service;
    }

    get $provide() {
        Services.checkServiceAccess(this.provide);
        return this.provide;
    }

    set $provide(service: angular.auto.IProvideService) {
        Services.checkServiceSet(this.provide);
        this.provide = service;
    }

    get $q() {
        Services.checkServiceAccess(this.q);
        return this.q;
    }

    set $q(service: angular.IQService) {
        Services.checkServiceSet(this.q);
        this.q = service;
    }

    get $rootElement() {
        Services.checkServiceAccess(this.rootElement);
        return this.rootElement;
    }

    set $rootElement(service: angular.IRootElementService) {
        Services.checkServiceSet(this.rootElement);
        this.rootElement = service;
    }

    get $rootScope() {
        Services.checkServiceAccess(this.rootScope);
        return this.rootScope;
    }

    set $rootScope(service: angular.IRootScopeService) {
        Services.checkServiceSet(this.rootScope);
        this.rootScope = service;
    }

    get $sce() {
        Services.checkServiceAccess(this.sce);
        return this.sce;
    }

    set $sce(service: angular.ISCEService) {
        Services.checkServiceSet(this.sce);
        this.sce = service;
    }

    get $sceDelegate() {
        Services.checkServiceAccess(this.sceDelegate);
        return this.sceDelegate;
    }

    set $sceDelegate(service: angular.ISCEDelegateService) {
        Services.checkServiceSet(this.sceDelegate);
        this.sceDelegate = service;
    }

    get $templateCache() {
        Services.checkServiceAccess(this.templateCache);
        return this.templateCache;
    }

    set $templateCache(service: angular.ITemplateCacheService) {
        Services.checkServiceSet(this.templateCache);
        this.templateCache = service;
    }

    get $templateRequest() {
        Services.checkServiceAccess(this.templateRequest);
        return this.templateRequest;
    }

    set $templateRequest(service: angular.ITemplateRequestService) {
        Services.checkServiceSet(this.templateRequest);
        this.templateRequest = service;
    }

    get $timeout() {
        Services.checkServiceAccess(this.timeout);
        return this.timeout;
    }

    set $timeout(service: angular.ITimeoutService) {
        Services.checkServiceSet(this.timeout);
        this.timeout = service;
    }

    get $window() {
        Services.checkServiceAccess(this.window);
        return this.window;
    }

    set $window(service: angular.IWindowService) {
        Services.checkServiceSet(this.window);
        this.window = service;
    }

    get $xhrFactory() {
        Services.checkServiceAccess(this.xhrFactory);
        return this.xhrFactory;
    }

    set $xhrFactory(service: angular.IXhrFactory<any>) {
        Services.checkServiceSet(this.xhrFactory);
        this.xhrFactory = service;
    }
}

export const services = new Services();

timApp.config(["$provide", "$httpProvider", "$logProvider",
    ($a: angular.auto.IProvideService,
     $b: angular.IHttpProvider,
     $c: angular.ILogProvider) => {
        services.$provide = $a;
        services.$httpProvider = $b;
        services.$logProvider = $c;
    }]);

timApp.run(["$injector", ($i: angular.auto.IInjectorService) => {
    services.$anchorScroll = $i.get("$anchorScroll") as angular.IAnchorScrollService;
    services.$cacheFactory = $i.get("$cacheFactory") as angular.ICacheFactoryService;
    services.$compile = $i.get("$compile") as angular.ICompileService;
    services.$controller = $i.get("$controller") as angular.IControllerService;
    services.$document = $i.get("$document") as angular.IDocumentService;
    services.$exceptionHandler = $i.get("$exceptionHandler") as angular.IExceptionHandlerService;
    services.$filter = $i.get("$filter") as angular.IFilterService;
    services.$http = $i.get("$http") as angular.IHttpService;
    services.$httpBackend = $i.get("$httpBackend") as angular.IHttpBackendService;
    services.$httpParamSerializer = $i.get("$httpParamSerializer") as angular.IHttpParamSerializer;
    services.$httpParamSerializerJQLike = $i.get("$httpParamSerializerJQLike") as angular.IHttpParamSerializer;
    services.$injector = $i;
    services.$interpolate = $i.get("$interpolate") as angular.IInterpolateService;
    services.$interval = $i.get("$interval") as angular.IIntervalService;
    services.$locale = $i.get("$locale") as angular.ILocaleService;
    services.$location = $i.get("$location") as angular.ILocationService;
    services.$log = $i.get("$log") as angular.ILogService;
    services.$parse = $i.get("$parse") as angular.IParseService;
    services.$q = $i.get("$q") as angular.IQService;
    services.$rootElement = $i.get("$rootElement") as angular.IRootElementService;
    services.$rootScope = $i.get("$rootScope") as angular.IRootScopeService;
    services.$sce = $i.get("$sce") as angular.ISCEService;
    services.$sceDelegate = $i.get("$sceDelegate") as angular.ISCEDelegateService;
    services.$templateCache = $i.get("$templateCache") as angular.ITemplateCacheService;
    services.$templateRequest = $i.get("$templateRequest") as angular.ITemplateRequestService;
    services.$timeout = $i.get("$timeout") as angular.ITimeoutService;
    services.$window = $i.get("$window") as angular.IWindowService;
    services.$xhrFactory = $i.get("$xhrFactory") as angular.IXhrFactory<any>;

    // 3rd party services
    services.$ocLazyLoad = $i.get("$ocLazyLoad") as ILazyLoad;
}]);
