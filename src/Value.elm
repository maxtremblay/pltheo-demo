module Value exposing (evaluate)

import Term exposing (Term(..))


evaluate : Term -> Maybe Term
evaluate term =
    case term of
        Zero ->
            Just Zero

        Yes ->
            Just Yes

        No ->
            Just No
       
        IfThenElse Yes yesBranch _ ->
          evaluate yesBranch

        IfThenElse No _ noBranch ->
          evaluate noBranch

        IfThenElse condition yesBranch noBranch ->
          evaluate condition
          |> Maybe.andThen (\evaluatedCondition ->
            if not (isNumber evaluatedCondition) then
              evaluate (Term.IfThenElse evaluatedCondition yesBranch noBranch)
              else 
                Nothing
          )

        Next inner ->
          Maybe.map Next (evaluate inner)

        Prev Zero ->
          Just Zero

        Prev (Next inner) ->
          if isNumber inner then
            evaluate inner
          else
            Nothing

        Prev inner ->
          Maybe.map Prev (evaluate inner)

        IsZero Zero ->
          Just Yes
       
        IsZero (Next inner) ->
          if isNumber inner then
            Just No
          else
            Nothing
  
        IsZero inner ->
          Maybe.map IsZero (evaluate inner)

        _ -> Nothing

isNumber : Term -> Bool 
isNumber term =
  case term of
    Zero -> True
    Next inner -> isNumber inner
    Prev inner -> isNumber inner
    _ -> False

