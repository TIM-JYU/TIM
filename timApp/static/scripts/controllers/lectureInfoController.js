var docId, lectureId, lectureCode, lectureStartTime, lectureEndTime;
/**
 * Created by hajoviin on 11.5.2015.
 */
timApp.controller('LectureInfoController', ['$scope', '$http', function ($scope, $http) {
    $scope.docId = docId;
    $scope.lectureId = lectureId;
    $scope.lectureCode = "Lecture info: " + lectureCode;
    $scope.lectureStartTime = lectureStartTime;
    $scope.lectureEndTime = lectureEndTime;
    $scope.msg = "";

    $scope.getAllMessages = function () {
        $http({
            url: '/getAllMessages',
            type: 'GET',
            params: {lecture_id: $scope.lectureId}
        })
            .success(function (answer) {
                angular.forEach(answer.data, function (msg) {
                    $scope.msg = $scope.msg + msg + "\n";
                });

            })
            .error(function () {
                console.log("fail")
            })
    };

    $scope.deleteLecture = function () {
        var confirmAnswer = confirm("Do you really want to delete this lecture?");
        if (confirmAnswer) {
            $http({
                url: '/deleteLecture',
                method: 'POST',
                params: {'doc_id': $scope.docId, lecture_id: $scope.lectureId}
            })
                .success(function (answer) {
                    window.history.back();
                })
                .error(function () {
                    console.log("Failed to delete the lecture");
                })

        }
    };

    $scope.getAllMessages();
}]);