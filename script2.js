angular.module('MCQ', [])
  .directive('mmcq', standardDirective("MMCQTemplate.html"
                     , function($scope){
                    $scope.active  = false;
                    $scope.checked = true;
                    $scope.headertext  = "Check your understanding";
                    $scope.buttonText  = "Submit";
                    if ($scope.content.question.headerText !== null)
                        $scope.headertext  = $scope.content.question.headerText;
                    if ($scope.content.question.buttonText !== null)
                        $scope.buttonText  = $scope.content.question.buttonText;
                            
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
        $scope.headertext  = "Check your understanding";
        console.log(["MCQ content",$scope.content]);
        $scope.headertext  = "Check your understanding";
        $scope.buttonText  = "Submit";
        if ($scope.content.question.headerText !== null)
            $scope.headerText  = $scope.content.question.headerText;
        if ($scope.content.question.buttonText !== null)
            $scope.buttonText  = $scope.content.question.buttonText;
        return;}
    , function(scope){return parseInt(scope.userSelection);}));

function toBool(lst) {
  var l = new Array(lst.length);
  for(var i=0;i<lst.length;l++)
       if (lst[i] === "true") {l[i]=true;}
       else if (lst[i] === "false") {l[i]=false;}
       else {l[i]=null;}
}
