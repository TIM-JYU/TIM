// idea from https://github.com/bcherny/ngimport
import * as angular from "angular";
import {ngStorage} from "ngstorage";

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
    $window = $i.get("$window");
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
