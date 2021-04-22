module Term exposing (Term(..), parse, toString)


type Term
    = Yes
    | No
    | Not Term
    | IfThenElse Term Term Term
    | Zero
    | Next Term
    | Prev Term
    | IsZero Term


toString : Term -> String
toString term =
    case term of
        Yes ->
            "Yes"

        No ->
            "No"

        Not t ->
            "Not ( " ++ toString t ++ " )"

        IfThenElse c y n ->
            "IfThenElse ( " ++ toString c ++ " | " ++ toString y ++ " | " ++ toString n ++ " )"

        Zero ->
            "Zero"

        Next t ->
            "Next ( " ++ toString t ++ " )"

        Prev t ->
            "Prev ( " ++ toString t ++ " )"

        IsZero t ->
            "IsZero ( " ++ toString t ++ " )"


type alias Parsed value =
    { restOfInput : List String
    , parsed : value
    }


type alias Parser value =
    List String -> Maybe (Parsed value)


parse : String -> Maybe Term
parse input =
    nothingAfter parseAnyTerm (String.words input)
        |> Maybe.map (\value -> value.parsed)



-- Parsers


parseLiteral : String -> value -> Parser value
parseLiteral literal target input =
    case input of
        word :: restOfInput ->
            if word == literal then
                Just
                    { parsed = target
                    , restOfInput = restOfInput
                    }

            else
                Nothing

        _ ->
            Nothing


parseNo : Parser Term
parseNo =
    parseLiteral "No" No


parseYes : Parser Term
parseYes =
    parseLiteral "Yes" Yes


parseZero : Parser Term
parseZero =
    parseLiteral "Zero" Zero


parseNextTermIf : String -> Parser Term
parseNextTermIf literal input =
    case input of
        word :: restOfInput ->
            if word == literal then
                parseAnyTerm restOfInput
                    |> Maybe.map
                        (\someterm ->
                            { restOfInput = someterm.restOfInput, parsed = someterm.parsed }
                        )

            else
                Nothing

        _ ->
            Nothing


parseOperator : String -> (Term -> Term) -> Parser Term
parseOperator literal target =
    parseNextTermIf literal
        |> map (\parsed -> target parsed)


parseNot : Parser Term
parseNot =
    parseOperator "Not" Not


parseNext : Parser Term
parseNext =
    parseOperator "Next" Next


parsePrev : Parser Term
parsePrev =
    parseOperator "Prev" Prev


parseIsZero : Parser Term
parseIsZero =
    parseOperator "IsZero" IsZero


parseIfThenElse : Parser Term
parseIfThenElse =
    pair (pair parseIf parseThen) parseElse
        |> map
            (\parsed ->
                IfThenElse
                    (Tuple.first (Tuple.first parsed))
                    (Tuple.second (Tuple.first parsed))
                    (Tuple.second parsed)
            )


parseIf : Parser Term
parseIf =
    parseNextTermIf "If"


parseThen : Parser Term
parseThen =
    parseNextTermIf "Then"


parseElse : Parser Term
parseElse =
    parseNextTermIf "Else"


parseAnyTerm : Parser Term
parseAnyTerm =
    [ parseNot
    , parseNo
    , parseYes
    , parseZero
    , parseNext
    , parsePrev
    , parseIsZero
    , parseIfThenElse
    ]
        |> either



-- Combinators


pair : Parser firstValue -> Parser secondValue -> Parser ( firstValue, secondValue )
pair firstParser secondParser input =
    firstParser input
        |> Maybe.andThen
            (\firstOutput ->
                secondParser firstOutput.restOfInput
                    |> Maybe.map (combine firstOutput)
            )


combine : Parsed firstValue -> Parsed secondValue -> Parsed ( firstValue, secondValue )
combine firstParsed secondParsed =
    { restOfInput = secondParsed.restOfInput
    , parsed = ( firstParsed.parsed, secondParsed.parsed )
    }


map : (value -> newValue) -> Parser value -> Parser newValue
map function parser input =
    Maybe.map (mapParsed function) (parser input)


mapParsed : (value -> newValue) -> Parsed value -> Parsed newValue
mapParsed function parsed =
    { restOfInput = parsed.restOfInput, parsed = function parsed.parsed }


either : List (Parser value) -> Parser value
either parsers input =
    case parsers of
        head :: tail ->
            case head input of
                Just parsed ->
                    Just parsed

                Nothing ->
                    either tail input

        [] ->
            Nothing


nothingAfter : Parser value -> Parser value
nothingAfter parser input =
    Maybe.andThen
        (\parsed ->
            if List.isEmpty parsed.restOfInput then
                Just parsed

            else
                Nothing
        )
        (parser input)
