// idea from https://github.com/bcherny/ngimport
import * as angular from "angular";
import {ngStorage} from "ngstorage";
import {ILazyLoad} from "oclazyload";

export let $anchorScroll: angular.IAnchorScrollService = null;
export let $cacheFactory: angular.ICacheFactoryService = null;
export let $compile: angular.ICompileService = null;
export let $controller: angular.IControllerService = null;
export let $document: angular.IDocumentService = null;
export let $exceptionHandler: angular.IExceptionHandlerService = null;
export let $filter: angular.IFilterService = null;
export let $http: angular.IHttpService = null;
export let $httpBackend: angular.IHttpBackendService = null;
export let $httpParamSerializer: angular.IHttpParamSerializer = null;
export let $httpParamSerializerJQLike: angular.IHttpParamSerializer = null;
export let $httpProvider: angular.IHttpProvider = null;
export let $injector: angular.auto.IInjectorService = null;
export let $interpolate: angular.IInterpolateService = null;
export let $interval: angular.IIntervalService = null;
export let $locale: angular.ILocaleService = null;
export let $localStorage: ngStorage.StorageService = null;
export let $location: angular.ILocationService = null;
export let $log: angular.ILogService = null;
export let $logProvider: angular.ILogProvider = null;
export let $ocLazyLoad: ILazyLoad = null;
export let $parse: angular.IParseService = null;
export let $provide: angular.auto.IProvideService = null;
export let $q: angular.IQService = null;
export let $rootElement: angular.IRootElementService = null;
export let $rootScope: angular.IRootScopeService = null;
export let $sanitize: angular.sanitize.ISanitizeService = null;
export let $sce: angular.ISCEService = null;
export let $sceDelegate: angular.ISCEDelegateService = null;
export let $templateCache: angular.ITemplateCacheService = null;
export let $templateRequest: angular.ITemplateRequestService = null;
export let $timeout: angular.ITimeoutService = null;
export let $uibModal: angular.ui.bootstrap.IModalService = null;
export let $upload: angular.angularFileUpload.IUploadService = null;
export let $window: angular.IWindowService = null;
export let $xhrFactory: angular.IXhrFactory<any> = null;

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
