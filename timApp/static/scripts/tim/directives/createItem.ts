
import angular from "angular";
import {timApp} from "tim/app";
import * as formErrorMessage from "tim/directives/formErrorMessage";
import * as shortNameValidator from "tim/directives/shortNameValidator";
import {markAsUsed} from "tim/utils";
import {$http, $window} from "../ngimport";
import {slugify} from "../services/slugify";

markAsUsed(formErrorMessage, shortNameValidator);

timApp.directive("createItem", [function() {
    "use strict";
    return {
        restrict: "E",
        scope: {
            itemType: "@", // folder or document
            itemTitle: "@?",
            itemName: "@?",
            itemLocation: "@?",
            fullPath: "@?",
            params: "=?", // any additional parameters to be sent to server
        },
        templateUrl: "/static/templates/createItem.html",
        link($scope, $element) {

        },

        controller: ["$scope", "$element", "$attrs", function($scope, $element, $attrs) {
            const sc = $scope;

            sc.automaticShortName = true;

            if (sc.fullPath) {
                const str = sc.fullPath;
                sc.itemLocation = str.substring(0, str.lastIndexOf("/"));
                sc.itemTitle = str.substring(str.lastIndexOf("/") + 1, str.length);
            }
            if (sc.itemTitle) {
                sc.itemName = slugify(sc.itemTitle);
            }

            sc.alerts = [];

            sc.createItem = function() {
                $http.post<{path}>("/createItem", angular.extend({
                    item_path: sc.itemLocation + "/" + sc.itemName,
                    item_type: sc.itemType,
                    item_title: sc.itemTitle,
                }, sc.params)).then(function(response) {
                    $window.location.href = "/view/" + response.data.path;
                }, function(response) {
                    sc.alerts = [];
                    sc.alerts.push({msg: response.data.error, type: "danger"});
                });
            };

            sc.closeAlert = function(index) {
                sc.alerts.splice(index, 1);
            };

            sc.titleChanged = function() {
                if (!sc.automaticShortName) {
                    return;
                }
                sc.itemName = slugify(sc.itemTitle);
            };

            sc.nameChanged = function() {
                sc.automaticShortName = (sc.itemName || []).length === 0;
            };
        }],
    };
}]);
