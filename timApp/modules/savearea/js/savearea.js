"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
Object.defineProperty(exports, "__esModule", { value: true });
/**
 * Defines the client-side implementation of an example plugin (a saveareandrome checker).
 */
var angular_1 = require("angular");
var t = require("io-ts");
var util_1 = require("tim/plugin/util");
var ngimport_1 = require("tim/util/ngimport");
var utils_1 = require("tim/util/utils");
var lib_1 = require("../../../static/scripts/jspm_packages/npm/io-ts@1.4.1/lib");
var saveareaApp = angular_1.default.module("saveareaApp", ["ngSanitize"]);
exports.moduleDefs = [saveareaApp];
var SaveareaMarkup = t.intersection([
    t.partial({
        initword2: t.number,
        inputplaceholder2: util_1.nullable(t.number),
        inputstem2: t.string,
    }),
    util_1.GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: util_1.withDefault(t.number, 500),
        cols: util_1.withDefault(t.number, 20),
    }),
]);
var SaveareaAll = t.intersection([
    t.partial({
        demopisteet: t.number,
    }),
    t.type({
        info: util_1.Info,
        markup: SaveareaMarkup,
        preview: t.boolean,
    }),
]);
var SaveareaController = /** @class */ (function (_super) {
    __extends(SaveareaController, _super);
    function SaveareaController() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.isRunning = false;
        _this.demopisteet = lib_1.number;
        return _this;
    }
    SaveareaController.prototype.getDefaultMarkup = function () {
        return {};
    };
    SaveareaController.prototype.buttonText = function () {
        return _super.prototype.buttonText.call(this) || "Tallenna";
    };
    SaveareaController.prototype.$onInit = function () {
        _super.prototype.$onInit.call(this);
        this.modelOpts = { debounce: this.autoupdate };
    };
    Object.defineProperty(SaveareaController.prototype, "autoupdate", {
        get: function () {
            return this.attrs.autoupdate;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(SaveareaController.prototype, "inputstem2", {
        get: function () {
            return this.attrs.inputstem2 || null;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(SaveareaController.prototype, "cols", {
        get: function () {
            return this.attrs.cols;
        },
        enumerable: true,
        configurable: true
    });
    SaveareaController.prototype.initCode = function () {
        this.demopisteet = lib_1.number;
        this.error = undefined;
        this.result = undefined;
    };
    SaveareaController.prototype.saveText = function () {
        this.doSaveText(false);
    };
    SaveareaController.prototype.doSaveText = function (nosave) {
        return __awaiter(this, void 0, void 0, function () {
            var params, url, r, data;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        this.error = "... saving ...";
                        this.isRunning = true;
                        this.result = undefined;
                        params = {
                            input: {
                                nosave: false,
                                demopisteet: this.demopisteet,
                            },
                        };
                        if (nosave) {
                            params.input.nosave = true;
                        }
                        url = this.pluginMeta.getAnswerUrl();
                        return [4 /*yield*/, utils_1.to(ngimport_1.$http.put(url, params))];
                    case 1:
                        r = _a.sent();
                        this.isRunning = false;
                        if (r.ok) {
                            data = r.result.data;
                            this.error = data.web.error;
                            this.result = data.web.result;
                        }
                        else {
                            this.error = "Infinite loop or some other error?";
                        }
                        return [2 /*return*/];
                }
            });
        });
    };
    SaveareaController.prototype.getAttributeType = function () {
        return SaveareaAll;
    };
    return SaveareaController;
}(util_1.PluginBase));
saveareaApp.component("saveareaRunner", {
    bindings: {
        json: "@",
    },
    controller: SaveareaController,
    template: "\n<div class=\"no-popup-menu\">\n    <h4 ng-if=\"::$ctrl.header\" ng-bind-html=\"::$ctrl.header\"></h4>\n    <p ng-if=\"::$ctrl.stem\">{{::$ctrl.stem}}</p>\n    <div class=\"form-inline\">\n    <button class=\"timButton\"\n            ng-if=\"::$ctrl.buttonText()\"\n            ng-click=\"$ctrl.saveText()\">\n        {{::$ctrl.buttonText()}}\n    </button>\n    </div>\n    <a href=\"\" ng-if=\"$ctrl.edited\" ng-click=\"$ctrl.initCode()\">{{::$ctrl.resetText}}</a>\n    <div ng-if=\"$ctrl.error\" ng-bind-html=\"$ctrl.error\"></div>\n    <pre ng-if=\"$ctrl.result\">{{$ctrl.result}}</pre>\n    <p ng-if=\"::$ctrl.footer\" ng-bind=\"::$ctrl.footer\" class=\"plgfooter\"></p>\n</div>\n\n",
});
