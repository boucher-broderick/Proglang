import Data.List (delete, nub)
import PA1Helper (Lexp (..), runProgram)
import System.Environment (getArgs)

id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda _ _) = lexp
id' lexp@(Apply _ _) = lexp

-- Remove the occurance from the list
remove :: (Eq a) => a -> [a] -> [a]
remove = delete

-- Get free variables (variable, abstraction, application)
getFreeVars :: Lexp -> [String]
getFreeVars (Atom v) = [v]
getFreeVars (Lambda v e) = remove v (getFreeVars e)
getFreeVars (Apply e1 e2) = nub $ getFreeVars e1 ++ getFreeVars e2

-- Reduce the lambda expression
reducer :: Lexp -> Lexp
-- can't reduce variable
reducer (Atom v) = Atom v
-- beta reduction necessary
reducer (Apply (Lambda v e) e2) =
  let reduced = beta v (reducer e2) e
  in if Apply (Lambda v e) e2 == reduced then reduced else reducer reduced
-- reduce both parts
reducer (Apply e1 e2) =
  let reduced = Apply (reducer e1) (reducer e2)
  in if Apply e1 e2 == reduced then reduced else reducer reduced
-- eta reduction necessary
reducer (Lambda v (Apply e1 e2)) =
  let reduced = eta (Lambda v (Apply e1 e2)) v e1 e2
  in if Lambda v (Apply e1 e2) == reduced then reduced else reducer reduced
-- reduce lambda body
reducer (Lambda v e) =
  let reduced = Lambda v (reducer e)
  in if Lambda v e == reduced then reduced else reducer reduced

-- Beta reduction (variable, application, abstraction)
beta :: String -> Lexp -> Lexp -> Lexp
beta substVar substValue exp = case exp of
  Atom atomVar -> if atomVar == substVar then substValue else exp
  Apply applyFirst applySecond -> Apply (recur applyFirst) (recur applySecond)
  Lambda lambdaVar lambdaExp ->
    if lambdaVar == substVar
      then exp
      else
        if lambdaVar `elem` getFreeVars substValue
          then
            let renamedLambda = rename exp (getFreeVars substValue)
            in recur renamedLambda
          else Lambda lambdaVar (recur lambdaExp)
  where
    recur = beta substVar substValue

-- Eta reduction
eta :: Lexp -> String -> Lexp -> Lexp -> Lexp
eta expression lambdaVar exp1 m =
  case m of
    Atom atomVar ->
      if lambdaVar /= atomVar || lambdaVar `elem` getFreeVars exp1
        then expression
        else exp1
    _ -> expression

-- alpha rename
rename :: Lexp -> [String] -> Lexp
rename (Lambda v e) oldNames =
  let newName = validName oldNames e
  in Lambda newName (beta v (Atom newName) e)
rename exp _ = exp

-- get a valid name replacement
validName :: [String] -> Lexp -> String
validName oldNames exp =
  let allNames = map (: []) ['a' .. 'z']
      usedNames = getFreeVars exp
      availableNames = filter (`notElem` (oldNames ++ usedNames)) allNames
  in head availableNames

main = do
  args <- getArgs
  let inFile = case args of x : _ -> x; _ -> "input.lambda"
  let outFile = case args of x : y : _ -> y; _ -> "output.lambda"
  -- id' simply returns its input, so runProgram will result
  -- in printing each lambda expression twice.
  runProgram inFile outFile reducer