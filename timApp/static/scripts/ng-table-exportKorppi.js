angular.module('ngTableExportKorppi', [])
.config(['$compileProvider', function($compileProvider) {
    // allow data links
    $compileProvider.aHrefSanitizationWhitelist(/^\s*(https?|ftp|mailto|data):/);
}])
.directive('exportKorppi', ['$parse', function ($parse) {
    return {
        restrict: 'A',
        scope: false,
        link: function(scope, element, attrs) {
            var data = '';
            var csv = {
                stringify: function(str) {
                    var result = str.replace(/^\s\s*/, '').replace(/\s*\s$/, ''); // trim spaces
                    return result;
                },
                generate: function() {
                    data = '';
                    if ( !csv.korppiExportId ) { alert("Give name for Korppi-field"); return; }
                    var rows = element.find('tr');
                    angular.forEach(rows, function(row, i) {
                        var tr = angular.element(row),
                            tds = tr.find('th'),
                            rowData = '';
                        if (tr.hasClass('ng-table-filters')) {
                            return;
                        }
                        if (tds.length == 0) {
                            tds = tr.find('td');
                        }
                        if (i <= 1) return; // forget heading and filter rows
                        rowData += csv.stringify(angular.element(tds[1]).text()) + ';'; // user name
                        rowData += csv.korppiExportId + ';'
                        rowData += csv.stringify(angular.element(tds[3]).text()); // points
                        data += rowData + "\n";
                    });
                },
                link: function() {
                    return 'data:text/csv;charset=UTF-8,' + encodeURIComponent(data);
                }
            };
            $parse(attrs.exportKorppi).assign(scope.$parent, csv);
        }
    };
}]);