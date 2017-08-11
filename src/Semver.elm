module Semver exposing (Version, version, print, parse, decode, encode, isValid, compare, lessThan, greaterThan)

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
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Parser exposing (Parser, (|=), (|.), oneOrMore, zeroOrMore)


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
isValid version =
    let
        isParseableAs parser string =
            case Parser.run (parser |. Parser.end) string of
                Ok _ ->
                    True

                Err _ ->
                    False
    in
        List.all (\n -> n >= 0) [ version.major, version.minor, version.patch ]
            && List.all (isParseableAs identifier) version.prerelease
            && List.all (isParseableAs identifier) version.build



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
        [ (\() -> Basics.compare versionA.major versionB.major)
        , (\() -> Basics.compare versionA.minor versionB.minor)
        , (\() -> Basics.compare versionA.patch versionB.patch)
        , (\() -> comparePrereleases True versionA.prerelease versionB.prerelease)
        ]
            |> List.foldl (flip compareIfEQ) EQ


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

    (v |> print |> parse == v)

-}
print : Version -> String
print version =
    let
        identifierSeries mark identifiers =
            if List.isEmpty identifiers then
                ""
            else
                mark ++ String.join "." identifiers
    in
        [ toString version.major
        , "."
        , toString version.minor
        , "."
        , toString version.patch
        , identifierSeries "-" version.prerelease
        , identifierSeries "+" version.build
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


{-| Decode a version from a JSON value.
-}
decode : Decoder Version
decode =
    let
        maybeToDecoder =
            Maybe.map JD.succeed
                >> Maybe.withDefault (JD.fail "Invalid version string.")
    in
        JD.string |> JD.andThen (parse >> maybeToDecoder)


{-| Encode a version as JSON.
-}
encode : Version -> JE.Value
encode version =
    version |> print |> JE.string


parser : Parser Version
parser =
    Parser.succeed Version
        |= nat
        |. Parser.symbol "."
        |= nat
        |. Parser.symbol "."
        |= nat
        |= Parser.oneOf [ preReleaseVersion, Parser.succeed [] ]
        |= Parser.oneOf [ buildMetadata, Parser.succeed [] ]
        |. Parser.end


{-| Parses only natural numbers (including zero).
-}
nat : Parser Int
nat =
    let
        isBetween low high char =
            let
                code =
                    Char.toCode char
            in
                (code >= Char.toCode low) && (code <= Char.toCode high)
    in
        Parser.succeed identity
            |= Parser.oneOf
                [ Parser.keep (Parser.Exactly 1) ((==) '0')
                , Parser.succeed (++)
                    |= Parser.keep (Parser.Exactly 1) (isBetween '1' '9')
                    |= Parser.keep zeroOrMore Char.isDigit
                ]
            |> Parser.andThen
                (\natStr ->
                    case String.toInt natStr of
                        Ok nat ->
                            Parser.succeed nat

                        Err err ->
                            Parser.fail err
                )


{-| An alphanumeric identifier, [a-zA-Z0-9-]+.
-}
identifier : Parser String
identifier =
    Parser.keep oneOrMore (\c -> c == '-' || Char.isLower c || Char.isUpper c || Char.isDigit c)


preReleaseVersion : Parser (List String)
preReleaseVersion =
    identifierSeries (Parser.symbol "-")


buildMetadata : Parser (List String)
buildMetadata =
    identifierSeries (Parser.symbol "+")


identifierSeries : Parser a -> Parser (List String)
identifierSeries seperator =
    Parser.succeed (::)
        |. seperator
        |= identifier
        |= Parser.repeat
            zeroOrMore
            (Parser.succeed identity |. Parser.symbol "." |= identifier)
