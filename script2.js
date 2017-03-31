define(["require", "exports", "angular", "./SimpleDirective.js"], function (require, exports, angular, standardDirective) {
angular.module('MCQ', [])
  .directive('mmcq', standardDirective("MMCQTemplate.html"
                     , function($scope){
                    $scope.active  = false;
                    $scope.checked = true;
                    $scope.headerText  = "Check your understanding";
                    $scope.buttonText  = "Submit";
                    $scope.trueText  = "True";
                    $scope.falseText  = "False";
                    $scope.correctText  = "Correct!";
                    $scope.wrongText  = "Wrong!";
                    ['headerText', 'buttonText', 'falseText', 'trueText', 'correctText', 'wrongText'].forEach(function(opt){
                        if ($scope.content.question[opt] !== null) {
                            $scope[opt] = $scope.content.question[opt];
                        }
                    });
                    $scope.answer = $scope.content.state;
                    if ($scope.answer==null||$scope.answer==undefined)
                        {
                        $scope.checked = false;
                        $scope.active  = true;
                        $scope.answer = new Array($scope.content.question.choices.length);
                        }
                    }
                    , function(scope){
                        for(var j=0;j<scope.answer.length;j++) {
                            if ( scope.answer[j] == "false") scope.answer[j]=false;
                            if ( scope.answer[j] == "true") scope.answer[j]=true;
                        }
                        scope.active=false;
                        return scope.answer;
                    })) //TODO: cleanup
  .directive('mcq', standardDirective("MCQTemplate.html"
    , function($scope){
        $scope.headerText  = "Check your understanding";
        $scope.buttonText  = "Submit";
        ['headerText', 'buttonText'].forEach(function(opt){
            if ($scope.content.question[opt] !== null) {
                $scope[opt] = $scope.content.question[opt];
            }
        });
        return;}
    , function(scope){return parseInt(scope.userSelection);}));

function toBool(lst) {
  var l = new Array(lst.length);
  for(var i=0;i<lst.length;l++)
       if (lst[i] === "true") {l[i]=true;}
       else if (lst[i] === "false") {l[i]=false;}
       else {l[i]=null;}
}
});
