
import {timApp} from "tim/app";
import {$timeout, $uibModal} from "../ngimport";
import * as allAnswersController from "tim/controllers/allAnswersController";
import {markAsUsed} from "../utils";

markAsUsed(allAnswersController);

timApp.controller("UserListController", ["$scope", "$element", "uiGridConstants",
    function($scope, $element, uiGridConstants) {
        "use strict";
        $scope.$watch(
            function() {
                return $element[0].offsetHeight + $element[0].offsetWidth;
            },
            function(sum) {
                const grid = $element.find(".grid");
                grid.css("width", ($element[0].offsetWidth - 5) + "px");
                grid.css("height", ($element[0].offsetHeight - 30) + "px");
            },
        );

        let anyAnnotations = false;
        let smallFieldWidth = 59;

        for (let i = 0; i < $scope.users.length; ++i) {
            if ($scope.users[i].velped_task_count > 0) {
                anyAnnotations = true;
                smallFieldWidth = 40;
                break;
            }
        }

        $scope.columns = [
            {field: "real_name", name: "Full name", cellTooltip: true, headerTooltip: true},
            {field: "name", name: "Username", cellTooltip: true, headerTooltip: true, maxWidth: 100},
            {field: "task_count", name: "Tasks", cellTooltip: true, headerTooltip: true, maxWidth: smallFieldWidth},
            {
                field: "task_points",
                name: "Task points",
                cellTooltip: true,
                headerTooltip: true,
                maxWidth: smallFieldWidth,
                visible: anyAnnotations,
            },
            {
                field: "velped_task_count",
                name: "Velped tasks",
                cellTooltip: true,
                headerTooltip: true,
                maxWidth: smallFieldWidth,
                visible: anyAnnotations,
            },
            {
                field: "velp_points",
                name: "Velp points",
                cellTooltip: true,
                headerTooltip: true,
                maxWidth: smallFieldWidth,
                visible: anyAnnotations,
            },
            {field: "total_points", name: "Points", cellTooltip: true, headerTooltip: true, maxWidth: smallFieldWidth},
        ];

        $scope.fireUserChange = function(row, updateAll) {
            $scope.changeUser(row.entity, updateAll);
        };

        $scope.instantUpdate = false;

        $scope.exportKorppi = function(options) {
            if (!options.taskPointField && !options.velpPointField && !options.totalPointField) {
                return;
            }
            const data = $scope.gridApi.grid.getVisibleRows();
            let dataKorppi = "";

            const fields = ["task_points", "velp_points", "total_points"];
            const fieldNames = {};
            fieldNames[fields[0]] = options.taskPointField;
            fieldNames[fields[1]] = options.velpPointField;
            fieldNames[fields[2]] = options.totalPointField;

            for (let i = 0; i < fields.length; ++i) {
                if (fieldNames[fields[i]]) {
                    if (dataKorppi !== "") {
                        dataKorppi += "\n";
                    }
                    for (let j = 0; j < data.length; j++) {
                        if (data[j].entity[fields[i]] !== null) {
                            dataKorppi += data[j].entity.name + ";" + fieldNames[fields[i]] + ";" + data[j].entity[fields[i]] + "\n";
                        }
                    }
                }
            }

            const filename = "korppi_" + $scope.docId + ".txt";
            // from https://stackoverflow.com/a/33542499
            const blob = new Blob([dataKorppi], {type: "text/plain"});
            if (window.navigator.msSaveOrOpenBlob) {
                window.navigator.msSaveBlob(blob, filename);
            } else {
                const elem = window.document.createElement("a");
                elem.href = window.URL.createObjectURL(blob);
                elem.download = filename;
                document.body.appendChild(elem);
                elem.click();
                document.body.removeChild(elem);
            }
        };

        $scope.gridOptions = {
            exporterMenuPdf: false,
            multiSelect: false,
            enableFullRowSelection: true,
            enableRowHeaderSelection: false,
            noUnselect: true,
            enableFiltering: true,
            enableColumnMenus: false,
            enableGridMenu: true,
            data: $scope.users,
            enableSorting: true,
            columnDefs: $scope.columns,
            onRegisterApi(gridApi) {
                $scope.gridApi = gridApi;

                gridApi.selection.on.rowSelectionChanged($scope, function(row) {
                    $scope.fireUserChange(row, $scope.instantUpdate);
                });
                gridApi.grid.modifyRows($scope.gridOptions.data);
                gridApi.selection.selectRow($scope.gridOptions.data[0]);
                gridApi.cellNav.on.navigate($scope, function(newRowCol, oldRowCol) {
                    $scope.gridApi.selection.selectRow(newRowCol.row.entity);
                });
            },
            gridMenuCustomItems: [
                {
                    title: "Export to Korppi",
                    action($event) {
                        $timeout(function() {
                            const instance = $uibModal.open({
                                animation: false,
                                ariaLabelledBy: "modal-title",
                                ariaDescribedBy: "modal-body",
                                templateUrl: "/static/templates/korppiExport.html",
                                controller: "KorppiExportCtrl",
                                controllerAs: "$ctrl",
                                size: "md",
                            });
                            instance.result.then($scope.exportKorppi, function() {

                            });
                        });
                    },
                    order: 10,
                },
                {
                    title: "Enable instant update",
                    action($event) {
                        $scope.instantUpdate = true;
                    },
                    shown() {
                        return !$scope.instantUpdate;
                    },
                    leaveOpen: true,
                    order: 20,
                },
                {
                    title: "Disable instant update",
                    action($event) {
                        $scope.instantUpdate = false;
                    },
                    shown() {
                        return $scope.instantUpdate;
                    },
                    leaveOpen: true,
                    order: 30,
                },
                {
                    title: "Answers as plain text",
                    action($event) {
                        $uibModal.open({
                            animation: false,
                            ariaLabelledBy: "modal-title",
                            ariaDescribedBy: "modal-body",
                            templateUrl: "/static/templates/allAnswersOptions.html",
                            controller: "AllAnswersCtrl",
                            controllerAs: "$ctrl",
                            size: "md",
                            resolve: {
                                options() {
                                    return {
                                        url: "/allDocumentAnswersPlain/" + $scope.docId,
                                        identifier: $scope.docId,
                                        allTasks: true,
                                    };
                                },
                            },
                        });
                    },
                    order: 40,
                },
            ],
            rowTemplate: "<div ng-dblclick=\"grid.appScope.fireUserChange(row, true)\" ng-repeat=\"(colRenderIndex, col) in colContainer.renderedColumns track by col.colDef.name\" class=\"ui-grid-cell\" ng-class=\"{ 'ui-grid-row-header-cell': col.isRowHeader }\" ui-grid-cell></div>",
        };
    }]);

timApp.controller("KorppiExportCtrl", ["$uibModalInstance", function($uibModalInstance) {
    "use strict";
    const $ctrl = this;
    $ctrl.options = {};

    $ctrl.ok = function() {
        $uibModalInstance.close($ctrl.options);
    };

    $ctrl.cancel = function() {
        $uibModalInstance.dismiss("cancel");
    };
}]);
