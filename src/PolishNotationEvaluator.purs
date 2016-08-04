--module PolishNotationEvaluator (evaluate) where
module PolishNotationEvaluator where

import Prelude ((&&), (==), ($), (>=), (>), not, (+), (-), (*), (/), (<>), map)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), isRight)
import Data.Array ((:), length, reverse, (!!), head, cons, tail, drop)
import Data.Tuple
import Data.Show (show)
import Data.Functor ((<$>))
import Data.Traversable (any, foldr)
import Data.String (split)
import Global (readFloat)

--https://en.wikipedia.org/wiki/PolishNotation_notation

data PolishNotationSymbol = PolishNotationOperator (Tuple (Number -> Number -> Number) String) | PolishNotationOperand (Number)
type PolishNotationParseResult = Either PolishNotationSymbol String

type Stack = Array Number
type PolishNotationEvaluationState = Tuple (Array PolishNotationSymbol) Stack
type PolishNotationStepResult = Either PolishNotationEvaluationState String

showPolishNotationSymbol :: PolishNotationSymbol -> String
showPolishNotationSymbol a = case a of
    PolishNotationOperator (Tuple f name) -> name
    PolishNotationOperand num -> show num

showPolishNotationParseResult :: PolishNotationParseResult -> String
showPolishNotationParseResult a = case a of
    Left s -> showPolishNotationSymbol s
    Right str -> str

lefts :: forall a b. Array (Either a b) -> Array a
lefts x =
    foldr func [] x where
        func elem accum = case elem of
            Left y -> cons y accum
            Right _ -> accum

rights :: forall a b. Array (Either a b) -> Array b
rights x =
    foldr func [] x where
        func elem accum = case elem of
            Left x -> accum
            Right y -> cons y accum

computeNextStep :: forall a b. b -> Array a -> (a -> a -> a) -> Either (Tuple b (Array a)) String
computeNextStep otherSymbols stack operator = let
    maybeFirst = stack !! 0 in
    case maybeFirst of
        Nothing -> Right "Should never happen by design"
        Just first -> let maybeSecond = stack !! 1 in
            case maybeSecond of
                Nothing -> Right "Should never happen by design"
                Just second -> let
                    result = (operator first second) in
                    Left $ (Tuple otherSymbols ([result] <> (drop 2 stack)))

step :: PolishNotationEvaluationState -> PolishNotationStepResult
step (Tuple polishSymbols stack) =
    let polishSymbolsLength = length polishSymbols in
    if (polishSymbolsLength > 0) then
        let
        maybeFirstSymbol = head polishSymbols
        maybeOtherSymbols = tail polishSymbols in
        case maybeFirstSymbol of
            Nothing -> Right "Should never happen by design"
            Just firstSymbol -> case maybeOtherSymbols of
                    Nothing -> Right "Should never happen by design"
                    Just otherSymbols -> case firstSymbol of
                        PolishNotationOperator (Tuple operator name) -> let
                            hasEnoughstack = (length stack) >= 2 in
                            if hasEnoughstack then
                                computeNextStep otherSymbols stack operator
                            else
                                Right $ "There is not enough stack when trying to apply function " <> name <> " ."
                        PolishNotationOperand operand -> Left (Tuple otherSymbols ([operand] <> stack))
    else
        let nb = length stack in
            if nb == 1 then
                Left $ (Tuple [] stack)
            else
                Right $ "There is no more operator or operand and the stack contains " <> (show nb) <> " elements."

isDone :: PolishNotationEvaluationState -> Boolean
isDone (Tuple polishSymbols stack) =
    ((length polishSymbols) == 0) && ((length stack) == 1)

run2 :: PolishNotationEvaluationState -> Int -> Either Number String
run2 a b =
    let iter = step a in
        case iter of
            Left (Tuple polishSymbols stack) ->
                if isDone (Tuple polishSymbols stack) then
                    let i = head stack in
                    case i of
                        Just x -> Left x
                        Nothing -> Right "Should never happen by design"
                else
                    run2 (Tuple polishSymbols stack) (b + 1)
            Right error -> Right (error <> " At iteration " <> (show b))

run :: PolishNotationEvaluationState -> Either Number String
run a = run2 a 0

convertStringToPolishNotationSymbol :: String -> PolishNotationParseResult
convertStringToPolishNotationSymbol aString =
    case aString of
        "+"  -> Left $ PolishNotationOperator (Tuple (+) "(+)")
        "-"  -> Left $ PolishNotationOperator (Tuple (-) "(-)")
        "*"  -> Left $ PolishNotationOperator (Tuple (*) "(*)")
        "/"  -> Left $ PolishNotationOperator (Tuple (/) "(/)")
        aStr ->
            Left (PolishNotationOperand (readFloat aStr)) -- not safe

parseInput :: String -> Array PolishNotationParseResult
parseInput aString = reverse $ map convertStringToPolishNotationSymbol $ (split " " aString)

evaluate :: String -> Either Number String
evaluate inputString = let
    parseResult = parseInput inputString
    isOk = not (any isRight parseResult) in
    if isOk then
        let filteredResult = lefts parseResult in
            run $ Tuple filteredResult []
    else
        let maybeHead = (head (rights parseResult)) in
            case maybeHead of
                Just x -> Right x
                Nothing -> Right "Should never happen by design"
