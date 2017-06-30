// idea from https://github.com/bcherny/ngimport
import * as angular from "angular";
import {ngStorage} from "ngstorage";
import {ILazyLoad} from "oclazyload";

export let $anchorScroll: angular.IAnchorScrollService = null as any;
export let $cacheFactory: angular.ICacheFactoryService = null as any;
export let $compile: angular.ICompileService = null as any;
export let $controller: angular.IControllerService = null as any;
export let $document: angular.IDocumentService = null as any;
export let $exceptionHandler: angular.IExceptionHandlerService = null as any;
export let $filter: angular.IFilterService = null as any;
export let $http: angular.IHttpService = null as any;
export let $httpBackend: angular.IHttpBackendService = null as any;
export let $httpParamSerializer: angular.IHttpParamSerializer = null as any;
export let $httpParamSerializerJQLike: angular.IHttpParamSerializer = null as any;
export let $httpProvider: angular.IHttpProvider = null as any;
export let $injector: angular.auto.IInjectorService = null as any;
export let $interpolate: angular.IInterpolateService = null as any;
export let $interval: angular.IIntervalService = null as any;
export let $locale: angular.ILocaleService = null as any;
export let $localStorage: ngStorage.StorageService = null as any;
export let $location: angular.ILocationService = null as any;
export let $log: angular.ILogService = null as any;
export let $logProvider: angular.ILogProvider = null as any;
export let $ocLazyLoad: ILazyLoad = null as any;
export let $parse: angular.IParseService = null as any;
export let $provide: angular.auto.IProvideService = null as any;
export let $q: angular.IQService = null as any;
export let $rootElement: angular.IRootElementService = null as any;
export let $rootScope: angular.IRootScopeService = null as any;
export let $sanitize: angular.sanitize.ISanitizeService = null as any;
export let $sce: angular.ISCEService = null as any;
export let $sceDelegate: angular.ISCEDelegateService = null as any;
export let $templateCache: angular.ITemplateCacheService = null as any;
export let $templateRequest: angular.ITemplateRequestService = null as any;
export let $timeout: angular.ITimeoutService = null as any;
export let $uibModal: angular.ui.bootstrap.IModalService = null as any;
export let $upload: angular.angularFileUpload.IUploadService = null as any;
export let $window: angular.IWindowService = null as any;
export let $xhrFactory: angular.IXhrFactory<any> = null as any;

export function injectProviders($a: angular.auto.IProvideService,
    $b: angular.IHttpProvider,
    $c: angular.ILogProvider) {
    $provide = $a;
    $httpProvider = $b;
    $logProvider = $c;
}

injectProviders.$inject = ["$provide", "$httpProvider", "$logProvider"];

export function injectServices($i: angular.auto.IInjectorService) {
    $anchorScroll = $i.get("$anchorScroll") as angular.IAnchorScrollService;
    $cacheFactory = $i.get("$cacheFactory") as angular.ICacheFactoryService;
    $compile = $i.get("$compile") as angular.ICompileService;
    $controller = $i.get("$controller") as angular.IControllerService;
    $document = $i.get("$document") as angular.IDocumentService;
    $exceptionHandler = $i.get("$exceptionHandler") as angular.IExceptionHandlerService;
    $filter = $i.get("$filter") as angular.IFilterService;
    $http = $i.get("$http") as angular.IHttpService;
    $httpBackend = $i.get("$httpBackend") as angular.IHttpBackendService;
    $httpParamSerializer = $i.get("$httpParamSerializer") as angular.IHttpParamSerializer;
    $httpParamSerializerJQLike = $i.get("$httpParamSerializerJQLike") as angular.IHttpParamSerializer;
    $injector = $i;
    $interpolate = $i.get("$interpolate") as angular.IInterpolateService;
    $interval = $i.get("$interval") as angular.IIntervalService;
    $locale = $i.get("$locale") as angular.ILocaleService;
    $localStorage = $i.get("$localStorage") as ngStorage.StorageService;
    $location = $i.get("$location") as angular.ILocationService;
    $log = $i.get("$log") as angular.ILogService;
    $parse = $i.get("$parse") as angular.IParseService;
    $q = $i.get("$q") as angular.IQService;
    $rootElement = $i.get("$rootElement") as angular.IRootElementService;
    $rootScope = $i.get("$rootScope") as angular.IRootScopeService;
    $sanitize = $i.get("$sanitize") as angular.sanitize.ISanitizeService;
    $sce = $i.get("$sce") as angular.ISCEService;
    $sceDelegate = $i.get("$sceDelegate") as angular.ISCEDelegateService;
    $templateCache = $i.get("$templateCache") as angular.ITemplateCacheService;
    $templateRequest = $i.get("$templateRequest") as angular.ITemplateRequestService;
    $timeout = $i.get("$timeout") as angular.ITimeoutService;
    $window = $i.get("$window") as angular.IWindowService;
    $xhrFactory = $i.get("$xhrFactory") as angular.IXhrFactory<any>;

    // 3rd party services
    $ocLazyLoad = $i.get("$ocLazyLoad") as ILazyLoad;
    $uibModal = $i.get("$uibModal") as angular.ui.bootstrap.IModalService;
    $upload = $i.get("Upload") as angular.angularFileUpload.IUploadService;
}

injectServices.$inject = ["$injector"];
