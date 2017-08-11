module Tests exposing (all)

import Char
import Set exposing (Set)
import Test exposing (..)
import Expect
import Random.Pcg
import Fuzz exposing (..)
import Shrink


--

import Semver


all : Test
all =
    describe "Semver"
        [ describe "checking"
            [ fuzz version "accepts valid versions" <|
                \version ->
                    Expect.true
                        "Expected correctly constructed version to be valid"
                        (version |> Semver.isValid)
            , fuzz2 version illegalIdentifierSeries "rejects invalid identifiers" <|
                \version illegalSeries ->
                    Expect.all
                        [ \idents ->
                            { version | prerelease = idents }
                                |> Semver.isValid
                                |> Expect.false "Expected illegal prerelease identifier to be invalid"
                        , \idents ->
                            { version | build = idents }
                                |> Semver.isValid
                                |> Expect.false "Expected illegal build identifier to be invalid"
                        ]
                        illegalSeries
            , test "rejects negative major version" <|
                \() ->
                    Semver.version -1 0 3 [] []
                        |> Semver.isValid
                        |> Expect.false "Expected invalid"
            , test "rejects negative minor version" <|
                \() ->
                    Semver.version 0 -2 3 [] []
                        |> Semver.isValid
                        |> Expect.false "Expected invalid"
            , test "rejects negative patch version" <|
                \() ->
                    Semver.version 0 3 -3 [] []
                        |> Semver.isValid
                        |> Expect.false "Expected invalid"
            ]
        , describe "parsing"
            [ fuzz version "parse inverts print" <|
                \version ->
                    Expect.equal
                        (version |> Semver.print |> Semver.parse)
                        (Just version)
            , fuzz2 version illegalIdentifierSeries "parse rejects illegal prerelease identifiers" <|
                \version illegalSeries ->
                    Expect.equal
                        ({ version | prerelease = illegalSeries } |> Semver.print |> Semver.parse)
                        Nothing
            , fuzz2 version illegalIdentifierSeries "parse rejects illegal build identifiers" <|
                \version illegalSeries ->
                    Expect.equal
                        ({ version | prerelease = illegalSeries } |> Semver.print |> Semver.parse)
                        Nothing
            ]
        , describe "comparison"
            [ test "compares standard cases as expected" <|
                \() ->
                    Expect.all
                        -- Lifted from node-semver
                        [ (|>) ( "0.0.0", "0.0.0-foo" )
                        , (|>) ( "0.0.1", "0.0.0" )
                        , (|>) ( "1.0.0", "0.9.9" )
                        , (|>) ( "0.10.0", "0.9.0" )
                        , (|>) ( "0.99.0", "0.10.0" )
                        , (|>) ( "2.0.0", "1.2.3" )
                        , (|>) ( "1.2.3", "1.2.3-asdf" )
                        , (|>) ( "1.2.3", "1.2.3-4" )
                        , (|>) ( "1.2.3", "1.2.3-4-foo" )
                        , (|>) ( "1.2.3-5-foo", "1.2.3-5" )
                        , (|>) ( "1.2.3-5", "1.2.3-4" )
                        , (|>) ( "1.2.3-5-foo", "1.2.3-5-Foo" )
                        , (|>) ( "3.0.0", "2.7.2+asdf" )
                        , (|>) ( "1.2.3-a.10", "1.2.3-a.5" )
                        , (|>) ( "1.2.3-a.b", "1.2.3-a.5" )
                        , (|>) ( "1.2.3-a.b", "1.2.3-a" )
                        , (|>) ( "1.2.3-a.b.c.10.d.5", "1.2.3-a.b.c.5.d.100" )
                        , (|>) ( "1.2.3-r2", "1.2.3-r100" )
                        , (|>) ( "1.2.3-r100", "1.2.3-R2" )
                        ]
                        (\( versionA, versionB ) ->
                            Expect.all
                                [ \( vA, vB ) ->
                                    Expect.equal (Just GT) (Maybe.map2 Semver.compare vA vB)
                                        |> Expect.onFail ("expected " ++ versionA ++ " > " ++ versionB)
                                , \( vA, vB ) ->
                                    Expect.equal (Just LT) (Maybe.map2 Semver.compare vB vA)
                                        |> Expect.onFail ("expected " ++ versionB ++ " < " ++ versionA)
                                ]
                                ( Semver.parse versionA, Semver.parse versionB )
                        )
            , fuzz2 version version "EQ only if equal" <|
                \versionA versionB ->
                    Expect.true
                        "expected versions to be equal if compared EQ"
                        (Semver.compare versionA versionB /= EQ || versionA == versionB)
            ]
        ]


nat : Fuzzer Int
nat =
    intRange 0 100


{-| Fuzzer for non-empty identifiers.
-}
identifier : Fuzzer String
identifier =
    Fuzz.map2
        (::)
        (identifierChar |> Fuzz.map String.fromChar)
        (identifierChar |> Fuzz.map String.fromChar |> Fuzz.list)
        |> Fuzz.map (String.join "")


{-| Fuzzer for legal versions.
-}
version : Fuzzer Semver.Version
version =
    Fuzz.constant Semver.version
        |> Fuzz.andMap nat
        |> Fuzz.andMap nat
        |> Fuzz.andMap nat
        |> Fuzz.andMap (list identifier)
        |> Fuzz.andMap (list identifier)


{-| Fuzzer for a series of identifiers of which one is illegal.
-}
illegalIdentifierSeries : Fuzzer (List String)
illegalIdentifierSeries =
    let
        legalOrEmpty =
            (identifierChar |> Fuzz.map String.fromChar |> Fuzz.list)

        isIllegal code =
            not <|
                (Set.member code identifierCharCodes || code == Char.toCode '.' || code == Char.toCode '+')

        illegalChar =
            List.range 10 127
                |> List.filter isIllegal
                |> List.map Char.fromCode
                |> fuzzSample '&'
                |> Fuzz.map String.fromChar

        illegalIdentifier =
            Fuzz.map3
                (\legalPre illegalChar legalPost -> legalPre ++ illegalChar :: legalPost)
                legalOrEmpty
                illegalChar
                legalOrEmpty
                |> Fuzz.map (String.join "")
    in
        Fuzz.map3
            (\pre illegal post -> pre ++ illegal :: post)
            (list identifier)
            illegalIdentifier
            (list identifier)


{-| Codes of valid identifier characters.
-}
identifierCharCodes : Set Int
identifierCharCodes =
    Char.toCode '-'
        :: List.range (Char.toCode '0') (Char.toCode '9')
        ++ List.range (Char.toCode 'A') (Char.toCode 'Z')
        ++ List.range (Char.toCode 'a') (Char.toCode 'z')
        |> Set.fromList


{-| Fuzzer for a valid identifier character.
-}
identifierChar : Fuzzer Char
identifierChar =
    identifierCharCodes
        |> Set.toList
        |> List.map Char.fromCode
        |> fuzzSample 'a'


fuzzSample : a -> List a -> Fuzzer a
fuzzSample fallback samples =
    samples
        |> Random.Pcg.sample
        |> Random.Pcg.map (Maybe.withDefault fallback)
        |> (\gen -> Fuzz.custom gen Shrink.noShrink)
