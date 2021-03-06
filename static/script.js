function standardDirective(template, extract) {
    return function () {
        return {
            restrict: 'E',
            templateUrl: function (elem, attrs) {
                return $sce.trustAsUrl(elem.parent().attr('data-plugin') + "/" + template);
            },
            replace: true,
            scope: {},
            controller: function ($scope, $element, $http) {
                $scope.ident = $element.parent().attr("id");
                $scope.content = JSON.parse($element.attr("data-content"));
                $scope.plugin = $element.parent().attr("data-plugin");
                $scope.submit = function () {
                    var message = {input: extract($scope)};
                    var localState = JSON.parse($element.parent().attr("data-state") || "null");
                    var localMarkup = JSON.parse($element.parent().attr("data-markup") || "null");
                    if (localState !== null) {
                        message.state = localState
                    }
                    if (localMarkup !== null) {
                        message.markup = localMarkup
                    }
                    $http({
                        method: 'PUT'
                        , url: $scope.plugin + "/" + $scope.ident + "/answer/"
                        , data: message
                    })
                        .then(function (resp) {
                            var data = resp.data;
                            $scope.content = data.web;
                            console.log(["data", $scope.content]);
                        }, function (err) {
                            alert(["error", err]);
                        });
                };
            }
        }
    }
}

angular.module('MCQ', [])
    .directive('mcq', standardDirective("MCQTemplate.html"
        , function (scope) {
            return scope.userSelection;
        }));

