// idea from https://github.com/bcherny/ngimport
import * as angular from "angular";
import {ngStorage} from "ngstorage";

export let $anchorScroll: angular.IAnchorScrollService;
export let $cacheFactory: angular.ICacheFactoryService;
export let $compile: angular.ICompileService;
export let $controller: angular.IControllerService;
export let $document: angular.IDocumentService;
export let $exceptionHandler: angular.IExceptionHandlerService;
export let $filter: angular.IFilterService;
export let $http: angular.IHttpService;
export let $httpBackend: angular.IHttpBackendService;
export let $httpParamSerializer: angular.IHttpParamSerializer;
export let $httpParamSerializerJQLike: angular.IHttpParamSerializer;
export let $httpProvider: angular.IHttpProvider;
export let $injector: angular.auto.IInjectorService;
export let $interpolate: angular.IInterpolateService;
export let $interval: angular.IIntervalService;
export let $locale: angular.ILocaleService;
export let $localStorage: ngStorage.StorageService;
export let $location: angular.ILocationService;
export let $log: angular.ILogService;
export let $logProvider: angular.ILogProvider;
export let $parse: angular.IParseService;
export let $provide: angular.auto.IProvideService;
export let $q: angular.IQService;
export let $rootElement: angular.IRootElementService;
export let $rootScope: angular.IRootScopeService;
export let $sanitize: angular.sanitize.ISanitizeService;
export let $sce: angular.ISCEService;
export let $sceDelegate: angular.ISCEDelegateService;
export let $templateCache: angular.ITemplateCacheService;
export let $templateRequest: angular.ITemplateRequestService;
export let $timeout: angular.ITimeoutService;
export let $uibModal: angular.ui.bootstrap.IModalService;
export let $upload: angular.angularFileUpload.IUploadService;

// The following is intentionally commented: $window should not be used to access global variables anymore.
// Use tim/utils/globals.ts instead.
// export let $window: angular.IWindowService;
export let $xhrFactory: angular.IXhrFactory<unknown>;

export function injectProviders($a: angular.auto.IProvideService,
    $b: angular.IHttpProvider,
    $c: angular.ILogProvider) {
    $provide = $a;
    $httpProvider = $b;
    $logProvider = $c;
}

export function injectServices($i: angular.auto.IInjectorService) {
    $anchorScroll = $i.get("$anchorScroll");
    $cacheFactory = $i.get("$cacheFactory");
    $compile = $i.get("$compile");
    $controller = $i.get("$controller");
    $document = $i.get("$document");
    $exceptionHandler = $i.get("$exceptionHandler");
    $filter = $i.get("$filter");
    $http = $i.get("$http");
    $httpBackend = $i.get("$httpBackend");
    $httpParamSerializer = $i.get("$httpParamSerializer");
    $httpParamSerializerJQLike = $i.get("$httpParamSerializerJQLike");
    $injector = $i;
    $interpolate = $i.get("$interpolate");
    $interval = $i.get("$interval");
    $locale = $i.get("$locale");
    $localStorage = $i.get("$localStorage");
    $location = $i.get("$location");
    $log = $i.get("$log");
    $parse = $i.get("$parse");
    $q = $i.get("$q");
    $rootElement = $i.get("$rootElement");
    $rootScope = $i.get("$rootScope");
    $sanitize = $i.get("$sanitize");
    $sce = $i.get("$sce");
    $sceDelegate = $i.get("$sceDelegate");
    $templateCache = $i.get("$templateCache");
    $templateRequest = $i.get("$templateRequest");
    $timeout = $i.get("$timeout");
    $xhrFactory = $i.get("$xhrFactory");

    // 3rd party services
    $uibModal = $i.get("$uibModal");
    $upload = $i.get("Upload");
}

function doInject() {
    injectProviders.$inject = ["$provide", "$httpProvider", "$logProvider"];
    injectServices.$inject = ["$injector"];
}

doInject();
