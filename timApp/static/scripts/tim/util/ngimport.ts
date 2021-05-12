// idea from https://github.com/bcherny/ngimport
import * as angular from "angular";
import {IHttpPromise, IRequestShortcutConfig} from "angular";
import {JsonValue} from "tim/util/jsonvalue";

/**
 * Restricts AngularJS HTTP POST data to contain only JSON serializable types.
 * TODO: Add also other methods such as PUT.
 */
export interface ISaferHttpService extends angular.IHttpService {
    post<T>(
        url: string,
        data: JsonValue,
        config?: IRequestShortcutConfig
    ): IHttpPromise<T>;
}

// These MUST be initialized to non-undefined values. Otherwise lazy loading plugins won't work if the plugin imports
// and uses some of these.
export let $anchorScroll: angular.IAnchorScrollService = (null as unknown) as angular.IAnchorScrollService;
export let $cacheFactory: angular.ICacheFactoryService = (null as unknown) as angular.ICacheFactoryService;
export let $compile: angular.ICompileService = (null as unknown) as angular.ICompileService;
export let $controller: angular.IControllerService = (null as unknown) as angular.IControllerService;
export let $document: angular.IDocumentService = (null as unknown) as angular.IDocumentService;
export let $exceptionHandler: angular.IExceptionHandlerService = (null as unknown) as angular.IExceptionHandlerService;
export let $filter: angular.IFilterService = (null as unknown) as angular.IFilterService;
export let $http: ISaferHttpService = (null as unknown) as angular.IHttpService;
export let $httpBackend: angular.IHttpBackendService = (null as unknown) as angular.IHttpBackendService;
export let $httpParamSerializer: angular.IHttpParamSerializer = (null as unknown) as angular.IHttpParamSerializer;
export let $httpParamSerializerJQLike: angular.IHttpParamSerializer = (null as unknown) as angular.IHttpParamSerializer;
export let $httpProvider: angular.IHttpProvider = (null as unknown) as angular.IHttpProvider;
export let $injector: angular.auto.IInjectorService = (null as unknown) as angular.auto.IInjectorService;
export let $interpolate: angular.IInterpolateService = (null as unknown) as angular.IInterpolateService;
export let $interval: angular.IIntervalService = (null as unknown) as angular.IIntervalService;
export let $locale: angular.ILocaleService = (null as unknown) as angular.ILocaleService;
export let $location: angular.ILocationService = (null as unknown) as angular.ILocationService;
export let $log: angular.ILogService = (null as unknown) as angular.ILogService;
export let $logProvider: angular.ILogProvider = (null as unknown) as angular.ILogProvider;
export let $parse: angular.IParseService = (null as unknown) as angular.IParseService;
export let $provide: angular.auto.IProvideService = (null as unknown) as angular.auto.IProvideService;
export let $q: angular.IQService = (null as unknown) as angular.IQService;
export let $rootElement: angular.IRootElementService = (null as unknown) as angular.IRootElementService;
export let $rootScope: angular.IRootScopeService = (null as unknown) as angular.IRootScopeService;
export let $sanitize: angular.sanitize.ISanitizeService = (null as unknown) as angular.sanitize.ISanitizeService;
export let $sce: angular.ISCEService = (null as unknown) as angular.ISCEService;
export let $sceDelegate: angular.ISCEDelegateService = (null as unknown) as angular.ISCEDelegateService;
export let $templateCache: angular.ITemplateCacheService = (null as unknown) as angular.ITemplateCacheService;
export let $templateRequest: angular.ITemplateRequestService = (null as unknown) as angular.ITemplateRequestService;
export let $timeout: angular.ITimeoutService = (null as unknown) as angular.ITimeoutService;
export let $uibModal: angular.ui.bootstrap.IModalService = (null as unknown) as angular.ui.bootstrap.IModalService;
export let $upload: angular.angularFileUpload.IUploadService = (null as unknown) as angular.angularFileUpload.IUploadService;

// The following is intentionally commented: $window should not be used to access global variables anymore.
// Use tim/utils/globals.ts instead.
// export let $window: angular.IWindowService = null as unknown as angular.IWindowService;
export let $xhrFactory: angular.IXhrFactory<unknown> = (null as unknown) as angular.IXhrFactory<
    unknown
>;

export function injectProviders(
    $a: angular.auto.IProvideService,
    $b: angular.IHttpProvider,
    $c: angular.ILogProvider
) {
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
