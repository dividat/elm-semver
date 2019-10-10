module Semver exposing
    ( Version, version, isValid
    , compare, lessThan, greaterThan
    , print, parse
    )

{-| Provides basic functionality for handling semantic version numbers. Follows
the Semver 2.0.0 standard strictly. No loose mode, no prefixes to version
numbers.

For the definition of semantic versioning with Semver 2.0.0, see
<http://semver.org>.


# Versions

@docs Version, version, isValid


# Comparing Versions

@docs compare, lessThan, greaterThan


# Handling Version Strings

@docs print, parse, decode, encode

-}

import Char
import Parser exposing ((|.), (|=), Parser)


{-| Represents a version with major, minor and patch number, as well as
optionally a prerelease version and build metadata.
-}
type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    , prerelease : List String
    , build : List String
    }


{-| Creates a version.
-}
version : Int -> Int -> Int -> List String -> List String -> Version
version =
    Version


{-| Checks whether a version is valid according to the specification.
-}
isValid : Version -> Bool
isValid candidate =
    let
        isParseableAs testParser string =
            case Parser.run (testParser |. Parser.end) string of
                Ok _ ->
                    True

                Err _ ->
                    False
    in
    List.all (\n -> n >= 0) [ candidate.major, candidate.minor, candidate.patch ]
        && List.all (isParseableAs preReleaseIdentifier) candidate.prerelease
        && List.all (isParseableAs buildMetadataIdentifier) candidate.build



-- Comparison


{-| Compares two versions for precedence.

Implements the ordering procedure defined in Semver 2.0.0.

-}
compare : Version -> Version -> Order
compare versionA versionB =
    let
        compareIfEQ order comparison =
            case order of
                EQ ->
                    comparison ()

                _ ->
                    order

        parseNat =
            Parser.run (nat |. Parser.end)

        compareIdentifiers identifierA identifierB =
            case ( parseNat identifierA, parseNat identifierB ) of
                ( Ok natA, Ok natB ) ->
                    Basics.compare natA natB

                _ ->
                    Basics.compare identifierA identifierB

        comparePrereleases isFirstComparison identifiersA identifiersB =
            case ( identifiersA, identifiersB ) of
                ( [], [] ) ->
                    EQ

                ( [], _ ) ->
                    if isFirstComparison then
                        GT

                    else
                        LT

                ( _, [] ) ->
                    if isFirstComparison then
                        LT

                    else
                        GT

                ( headA :: tailA, headB :: tailB ) ->
                    compareIfEQ
                        (compareIdentifiers headA headB)
                        (\() -> comparePrereleases False tailA tailB)
    in
    [ \() -> Basics.compare versionA.major versionB.major
    , \() -> Basics.compare versionA.minor versionB.minor
    , \() -> Basics.compare versionA.patch versionB.patch
    , \() -> comparePrereleases True versionA.prerelease versionB.prerelease
    ]
        |> List.foldl (\b a -> compareIfEQ a b) EQ


{-| Shorthand for determining whether `versionA` precedes `versionB`.
-}
lessThan : Version -> Version -> Bool
lessThan versionA versionB =
    compare versionA versionB == LT


{-| Shorthand for determining whether `versionA` is preceded by `versionB`.
-}
greaterThan : Version -> Version -> Bool
greaterThan versionA versionB =
    compare versionA versionB == GT



-- Version Strings


{-| Produce a version's string representation.

The output format is such that

    v |> print |> parse == v

-}
print : Version -> String
print version_ =
    let
        printSeries mark identifiers =
            if List.isEmpty identifiers then
                ""

            else
                mark ++ String.join "." identifiers
    in
    [ String.fromInt version_.major
    , "."
    , String.fromInt version_.minor
    , "."
    , String.fromInt version_.patch
    , printSeries "-" version_.prerelease
    , printSeries "+" version_.build
    ]
        |> String.join ""


{-| Parses a version string.

Parsing fails if the string is not legal according to Semver 2.0.0.

Does not accept loose syntax or prefixes ('v') to the version string.

-}
parse : String -> Maybe Version
parse versionString =
    Parser.run parser versionString
        |> Result.toMaybe


parser : Parser Version
parser =
    Parser.succeed Version
        |= nat
        |. Parser.symbol "."
        |= nat
        |. Parser.symbol "."
        |= nat
        |= Parser.oneOf
            [ identifierSeries preReleaseIdentifier (Parser.symbol "-")
            , Parser.succeed []
            ]
        |= Parser.oneOf
            [ identifierSeries buildMetadataIdentifier (Parser.symbol "+")
            , Parser.succeed []
            ]
        |. Parser.end


{-| Parses only natural numbers (including zero).
-}
nat : Parser Int
nat =
    Parser.succeed identity
        |= numericIdentifier
        |> Parser.andThen
            (\natStr ->
                case String.toInt natStr of
                    Just num ->
                        Parser.succeed num

                    Nothing ->
                        Parser.problem ("Not a natural number: " ++ natStr)
            )


preReleaseIdentifier : Parser String
preReleaseIdentifier =
    Parser.oneOf [ alphanumericIdentifier, numericIdentifier ]


buildMetadataIdentifier : Parser String
buildMetadataIdentifier =
    Parser.oneOf [ alphanumericIdentifier, digits ]


identifierSeries : Parser i -> Parser a -> Parser (List i)
identifierSeries identifier seperator =
    Parser.succeed identity
        |. seperator
        |= (identifier |> Parser.andThen (identifierSeriesTail identifier))


identifierSeriesTail : Parser i -> i -> Parser (List i)
identifierSeriesTail identifier firstItem =
    Parser.loop
        [ firstItem ]
        (\segments ->
            Parser.oneOf
                [ Parser.succeed (\item -> Parser.Loop (item :: segments))
                    |. Parser.chompIf ((==) '.')
                    |= identifier
                , Parser.succeed (Parser.Done (List.reverse segments))
                ]
        )


{-| An alphanumeric identifier, [a-zA-Z0-9-]+, containing at least one non-digit.
-}
alphanumericIdentifier : Parser String
alphanumericIdentifier =
    Parser.getChompedString <|
        -- Delay commit until after something other than a digit has been seen,
        -- this makes the composed parser usable as a oneOf option.
        Parser.backtrackable (Parser.chompWhile isDigit)
            |. Parser.chompIf isNonDigit
            |. Parser.chompWhile isIdentifierCharacter


{-| Either zero or a positive number without leading zero.
-}
numericIdentifier : Parser String
numericIdentifier =
    Parser.oneOf
        [ Parser.getChompedString <|
            Parser.chompIf isPositiveDigit
                |. Parser.chompWhile isDigit
        , Parser.getChompedString <|
            Parser.chompIf ((==) '0')
        ]


{-| A string of digits.
-}
digits : Parser String
digits =
    Parser.getChompedString <|
        Parser.chompIf isDigit
            |. Parser.chompWhile isDigit



-- Character predicates


isDigit : Char -> Bool
isDigit =
    isBetween '0' '9'


isPositiveDigit : Char -> Bool
isPositiveDigit =
    isBetween '1' '9'


isNonDigit : Char -> Bool
isNonDigit c =
    c == '-' || isBetween 'a' 'z' c || isBetween 'A' 'Z' c


isIdentifierCharacter : Char -> Bool
isIdentifierCharacter c =
    isDigit c || isNonDigit c


isBetween : Char -> Char -> Char -> Bool
isBetween low high char =
    let
        code =
            Char.toCode char
    in
    (code >= Char.toCode low) && (code <= Char.toCode high)
