function standardDirective(template, extract) {
    return function () {
        return {
            restrict: 'E',
            templateUrl: function (elem, attrs) {
                return elem.parent().attr('data-plugin') + "/" + template;
            },
            replace: true,
            scope: {},
            controller: function ($scope, $element, $http) {
                $scope.ident = $element.parent().attr("id");
                $scope.content = JSON.parse($element.attr("data-content"));
                console.log(["initial", $scope.content]);
                $scope.plugin = $element.parent().attr("data-plugin");
                $scope.submit = function () {
                    var message = {input: extract($scope)};
                    console.log(["sent", $scope.content.content]);
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

angular.module('Note', [])
    .directive('shortnote',
        standardDirective("ShortNoteTemplate.html"
            , function (scope) {
                return scope.content.content
            }));
