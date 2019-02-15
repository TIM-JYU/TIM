System.register(["angular", "io-ts", "tim/plugin/util", "tim/util/ngimport", "tim/util/utils"], function (exports_1, context_1) {
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
    var angular_1, t, util_1, ngimport_1, utils_1, utils_2, paliApp, PaliMarkup, PaliAll, PaliController;
    var __moduleName = context_1 && context_1.id;
    function isPalindrome(s) {
        var sc = s.toLowerCase();
        sc = sc.replace(/[^a-zåöä]/g, "");
        for (var i1 = 0, i2 = sc.length - 1; i1 < i2; i1++, i2--) {
            if (sc[i1] !== sc[i2]) {
                return false;
            }
        }
        return true;
    }
    return {
        setters: [
            function (angular_1_1) {
                angular_1 = angular_1_1;
            },
            function (t_1) {
                t = t_1;
            },
            function (util_1_1) {
                util_1 = util_1_1;
            },
            function (ngimport_1_1) {
                ngimport_1 = ngimport_1_1;
            },
            function (utils_1_1) {
                utils_1 = utils_1_1;
                utils_2 = utils_1_1;
            }
        ],
        execute: function () {
            paliApp = angular_1.default.module("paliApp", ["ngSanitize"]);
            PaliMarkup = t.intersection([
                t.partial({
                    initword: t.string,
                    inputplaceholder: util_1.nullable(t.string),
                    inputstem: t.string,
                }),
                util_1.GenericPluginMarkup,
                t.type({
                    autoupdate: util_1.withDefault(t.number, 500),
                    cols: util_1.withDefault(t.number, 20),
                }),
            ]);
            PaliAll = t.intersection([
                t.partial({
                    userword: t.string,
                }),
                t.type({ markup: PaliMarkup }),
            ]);
            PaliController = (function (_super) {
                __extends(PaliController, _super);
                function PaliController() {
                    var _this = _super !== null && _super.apply(this, arguments) || this;
                    _this.isRunning = false;
                    _this.userword = "";
                    _this.runTestGreen = false;
                    return _this;
                }
                PaliController.prototype.getDefaultMarkup = function () {
                    return {};
                };
                PaliController.prototype.buttonText = function () {
                    return _super.prototype.buttonText.call(this) || "Save";
                };
                PaliController.prototype.$onInit = function () {
                    _super.prototype.$onInit.call(this);
                    this.userword = this.attrsall.userword || this.attrs.initword || "";
                    this.modelOpts = { debounce: this.autoupdate };
                    this.checkPalindrome();
                };
                Object.defineProperty(PaliController.prototype, "edited", {
                    get: function () {
                        return this.attrs.initword !== this.userword;
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(PaliController.prototype, "autoupdate", {
                    get: function () {
                        return this.attrs.autoupdate;
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(PaliController.prototype, "inputplaceholder", {
                    get: function () {
                        return this.attrs.inputplaceholder || null;
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(PaliController.prototype, "inputstem", {
                    get: function () {
                        return this.attrs.inputstem || null;
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(PaliController.prototype, "cols", {
                    get: function () {
                        return this.attrs.cols;
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(PaliController.prototype, "resetText", {
                    get: function () {
                        return utils_2.valueDefu(this.attrs.resetText, "Reset");
                    },
                    enumerable: true,
                    configurable: true
                });
                PaliController.prototype.checkPalindrome = function () {
                    var is = isPalindrome(this.userword);
                    this.runTestGreen = is;
                    return is;
                };
                PaliController.prototype.initCode = function () {
                    this.userword = this.attrs.initword || "";
                    this.error = undefined;
                    this.result = undefined;
                    this.checkPalindrome();
                };
                PaliController.prototype.saveText = function () {
                    this.doSaveText(false);
                };
                PaliController.prototype.doSaveText = function (nosave) {
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
                                            paliOK: this.checkPalindrome(),
                                            userword: this.userword,
                                        },
                                    };
                                    if (nosave) {
                                        params.input.nosave = true;
                                    }
                                    url = this.pluginMeta.getAnswerUrl();
                                    return [4, utils_1.to(ngimport_1.$http.put(url, params))];
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
                                    return [2];
                            }
                        });
                    });
                };
                PaliController.prototype.getAttributeType = function () {
                    return PaliAll;
                };
                return PaliController;
            }(util_1.PluginBase));
            paliApp.component("paliRunner", {
                bindings: {
                    json: "@",
                },
                controller: PaliController,
                template: "\n<div class=\"csRunDiv no-popup-menu\">\n    <h4 ng-if=\"::$ctrl.header\" ng-bind-html=\"::$ctrl.header\"></h4>\n    <p ng-if=\"::$ctrl.stem\">{{::$ctrl.stem}}</p>\n    <div class=\"form-inline\"><label>{{::$ctrl.inputstem}} <span>\n        <input type=\"text\"\n               class=\"form-control\"\n               ng-model=\"$ctrl.userword\"\n               ng-model-options=\"::$ctrl.modelOpts\"\n               ng-change=\"$ctrl.checkPalindrome()\"\n               ng-trim=\"false\"\n               placeholder=\"{{::$ctrl.inputplaceholder}}\"\n               size=\"{{::$ctrl.cols}}\"></span></label>\n        <span class=\"unitTestGreen\" ng-if=\"$ctrl.runTestGreen && $ctrl.userword\">OK</span>\n        <span class=\"unitTestRed\" ng-if=\"!$ctrl.runTestGreen\">Wrong</span>\n    </div>\n    <button class=\"timButton\"\n            ng-if=\"::$ctrl.buttonText()\"\n            ng-disabled=\"$ctrl.isRunning || !$ctrl.userword\"\n            ng-click=\"$ctrl.saveText()\">\n        {{::$ctrl.buttonText()}}\n    </button>\n    <a href=\"\" ng-if=\"$ctrl.edited\" ng-click=\"$ctrl.initCode()\">{{::$ctrl.resetText}}</a>\n    <div ng-if=\"$ctrl.error\" ng-bind-html=\"$ctrl.error\"></div>\n    <pre ng-if=\"$ctrl.result\">{{$ctrl.result}}</pre>\n    <p ng-if=\"::$ctrl.footer\" ng-bind=\"::$ctrl.footer\" class=\"plgfooter\"></p>\n</div>\n",
            });
        }
    };
});
//# sourceMappingURL=pali.js.map