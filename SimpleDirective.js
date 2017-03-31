define(["require", "exports", "angular"], function (require, exports, angular) {
return function (template,build,extract) {
 return function($sce) {
    return {
      restrict: 'E',
      templateUrl: function(elem,attrs) {
                    return (elem.parent().attr('data-plugin')+"/"+template);
                   }, 
      replace: true,
      scope: {},
      controller: function($scope, $element, $http) {
        $scope.ident     = $element.parent().attr("id"); 
        $scope.content = JSON.parse($element.attr("data-content"));
        $scope.plugin = $element.parent().attr("data-plugin");
        build($scope,$element);
        $scope.submit = function () {
            var message = {input:extract($scope)};
            var localState  = JSON.parse($element.parent().attr("data-state")  || "null");
            var localMarkup = JSON.parse($element.parent().attr("data-markup") || "null");
            if (localState!==null) {message.state = localState};
            if (localMarkup!==null) {message.markup = localMarkup};
            $http({method:'PUT'
                  ,url:$scope.plugin+"/"+$scope.ident+"/answer/"
                  ,data:message})
             .success(function(data){
                  $scope.content = data.web
                  $scope.checked = true; 			
                 })
             .error(function(data,status,hdrs,cfg){

                 });
            };
       }
    }
  }
}
});
