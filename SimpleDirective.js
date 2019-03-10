define(["require", "exports", "angular"], function (require, exports, angular) {
    return function (template, build, extract, pluginType) {
        return function ($sce) {
            return {
                restrict: 'E',
                templateUrl: function (elem, attrs) {
                    return '/' + pluginType + "/" + template;
                },
                replace: true,
                scope: {},
                controller: function ($scope, $element, $http) {
                    $scope.content = JSON.parse($element.attr("data-content"));
                    build($scope, $element);
                    $scope.submit = function () {
                        var message = {input: extract($scope)};
                        var ident = $element.parent().attr("id");

                        $http({
                            method: 'PUT'
                            , url: "/" + pluginType + "/" + ident + "/answer/"
                            , data: message
                        })
                            .then(function (resp) {
                                $scope.content = resp.data.web;
                                $scope.checked = true;
                            }, function (err) {

                            });
                    };
                }
            }
        }
    }
});
