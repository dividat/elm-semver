module Tests exposing (all)

import Char
import Expect
import Fuzz exposing (..)
import Semver
import Set exposing (Set)
import Test exposing (..)


all : Test
all =
    describe "Semver"
        [ describe "checking"
            [ fuzz versionFuzzer "accepts valid versions" <|
                \version ->
                    Expect.equal True (version |> Semver.isValid)
            , fuzz2 versionFuzzer illegalIdentifierSeries "rejects invalid identifiers" <|
                \version illegalSeries ->
                    Expect.all
                        [ \idents ->
                            { version | prerelease = idents }
                                |> Semver.isValid
                                |> Expect.equal False
                        , \idents ->
                            { version | build = idents }
                                |> Semver.isValid
                                |> Expect.equal False
                        ]
                        illegalSeries
            , test "rejects negative major version" <|
                \() ->
                    Semver.version -1 0 3 [] []
                        |> Semver.isValid
                        |> Expect.equal False
            , test "rejects negative minor version" <|
                \() ->
                    Semver.version 0 -2 3 [] []
                        |> Semver.isValid
                        |> Expect.equal False
            , test "rejects negative patch version" <|
                \() ->
                    Semver.version 0 3 -3 [] []
                        |> Semver.isValid
                        |> Expect.equal False
            ]
        , describe "parsing"
            [ fuzz versionFuzzer "parse inverts print" <|
                \version ->
                    Expect.equal
                        (version |> Semver.print |> Semver.parse)
                        (Just version)
            , fuzz2 versionFuzzer illegalIdentifierSeries "parse rejects illegal prerelease identifiers" <|
                \version illegalSeries ->
                    Expect.equal
                        ({ version | prerelease = illegalSeries } |> Semver.print |> Semver.parse)
                        Nothing
            , fuzz2 versionFuzzer illegalIdentifierSeries "parse rejects illegal build identifiers" <|
                \version illegalSeries ->
                    Expect.equal
                        ({ version | prerelease = illegalSeries } |> Semver.print |> Semver.parse)
                        Nothing
            , test "print inverts parse of big guy" <|
                \() ->
                    let
                        bigGuy =
                            "1.0.0-alpha-a.b-c-somethinglong+build.1-aef.1-its-okay"
                    in
                    Expect.equal
                        (Semver.parse bigGuy |> Maybe.map Semver.print)
                        (Just bigGuy)
            ]
        , describe "hardcoded"
            [ test "accepts known valid strings" <|
                \() ->
                    Expect.all
                        (List.map
                            (\str () -> Expect.notEqual Nothing (Semver.parse str))
                            valid
                        )
                        ()
            , test "rejects known invalid strings" <|
                \() ->
                    Expect.all
                        (List.map
                            (\str () -> Expect.equal Nothing (Semver.parse str))
                            invalid
                        )
                        ()
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
            , fuzz2 versionFuzzer versionFuzzer "EQ only if equal" <|
                \versionA versionB ->
                    if versionA == versionB then
                        Expect.equal EQ (Semver.compare versionA versionB)
                    else
                        Expect.notEqual EQ (Semver.compare versionA versionB)
            ]
        ]


valid : List String
valid =
    [ "1.9.0"
    , "1.10.0"
    , "1.11.0"
    , "1.0.0-alpha"
    , "1.0.0-alpha.1"
    , "1.0.0-0.3.7"
    , "1.0.0-x.7.z.92"
    , "1.0.0-alpha+001"
    , "1.0.0+20130313144700"
    , "1.352.440-111a+exp.sha.5114185"
    , "1.352.440-beta+exp.sha.5114f85"
    , "0.0.4"
    , "1.2.3"
    , "10.20.30"
    , "1.1.2-prerelease+meta"
    , "1.1.2+meta"
    , "1.1.2+meta-valid"
    , "1.0.0-alpha"
    , "1.0.0-beta"
    , "1.0.0-alpha.beta"
    , "1.0.0-alpha.beta.1"
    , "1.0.0-alpha.1"
    , "1.0.0-alpha0.valid"
    , "1.0.0-alpha.0valid"
    , "1.0.0-alpha-a.b-c-somethinglong+build.1-aef.1-its-okay"
    , "1.0.0-rc.1+build.1"
    , "2.0.0-rc.1+build.123"
    , "1.2.3-beta"
    , "10.2.3-DEV-SNAPSHOT"
    , "1.2.3-SNAPSHOT-123"
    , "1.0.0"
    , "2.0.0"
    , "1.1.7"
    , "2.0.0+build.1848"
    , "2.0.1-alpha.1227"
    , "1.0.0-alpha+beta"
    , "1.2.3----RC-SNAPSHOT.12.9.1--.12+788"
    , "1.2.3----R-S.12.9.1--.12+meta"
    , "1.2.3----RC-SNAPSHOT.12.9.1--.12"
    , "1.0.0+0.build.1-rc.10000aaa-kk-0.1"
    , "1.0.0-0A.is.legal"
    , "0.0.0--+-"
    ]


invalid : List String
invalid =
    [ "1.9"
    , "1.01.0"
    , ".11.0"
    , "1.0.0alpha"
    , "1.0.0-alpha._1"
    , "1.0.0-0.03.7"
    , "01.0.0-x.7.z.92"
    , "1.0.0-alpha+**"
    , "1.0.0+"
    , "1.352.440-++exp.sha.5114f85"
    , "1"
    , "1.2"
    , "1.2.3-0123"
    , "1.2.3-0123.0123"
    , "1.1.2+.123"
    , "+invalid"
    , "-invalid"
    , "-invalid+invalid"
    , "-invalid.01"
    , "alpha"
    , "alpha.beta"
    , "alpha.beta.1"
    , "alpha.1"
    , "alpha+beta"
    , "alpha_beta"
    , "alpha."
    , "alpha.."
    , "beta"
    , "1.0.0-alpha_beta"
    , "-alpha."
    , "1.0.0-alpha.."
    , "1.0.0-alpha..1"
    , "1.0.0-alpha...1"
    , "1.0.0-alpha....1"
    , "1.0.0-alpha.....1"
    , "1.0.0-alpha......1"
    , "1.0.0-alpha.......1"
    , "01.1.1"
    , "1.01.1"
    , "1.1.01"
    , "1.2"
    , "1.2.3.DEV"
    , "1.2-SNAPSHOT"
    , "1.2.31.2.3----RC-SNAPSHOT.12.09.1--..12+788"
    , "1.2-RC-SNAPSHOT"
    , "-1.0.3-gamma+b7718"
    , "+justmeta"
    , "9.8.7+meta+meta"
    , "9.8.7-whatever+meta+meta"
    , "99999999999999999999999.999999999999999999.99999999999999999----RC-SNAPSHOT.12.09.1--------------------------------..12"
    ]


nat : Fuzzer Int
nat =
    intRange 0 100


{-| Fuzzer for non-empty identifiers.
-}
identifier : Fuzzer String
identifier =
    Fuzz.map2
        (++)
        (identifierChar |> Fuzz.map String.fromChar)
        (identifierChar |> Fuzz.map String.fromChar |> Fuzz.list |> Fuzz.map (String.join ""))


{-| Fuzzer for non-empty identifiers with at least one non-digit.
-}
alnumIdentifier : Fuzzer String
alnumIdentifier =
    Fuzz.map2
        (\prefix suffix ->
            prefix ++ "m" ++ suffix
        )
        (identifierChar |> Fuzz.map String.fromChar |> Fuzz.list |> Fuzz.map (String.join ""))
        (identifierChar |> Fuzz.map String.fromChar |> Fuzz.list |> Fuzz.map (String.join ""))


{-| Fuzzer for legal versions.
-}
versionFuzzer : Fuzzer Semver.Version
versionFuzzer =
    Fuzz.constant Semver.version
        |> Fuzz.andMap nat
        |> Fuzz.andMap nat
        |> Fuzz.andMap nat
        |> Fuzz.andMap (list (Fuzz.oneOf [ nat |> Fuzz.map String.fromInt, alnumIdentifier ]))
        |> Fuzz.andMap (list identifier)


{-| Fuzzer for a series of identifiers of which one is illegal.
-}
illegalIdentifierSeries : Fuzzer (List String)
illegalIdentifierSeries =
    let
        legalOrEmpty =
            identifierChar |> Fuzz.map String.fromChar |> Fuzz.list

        isIllegal code =
            not <|
                (Set.member code identifierCharCodes || code == Char.toCode '.' || code == Char.toCode '+')

        illegalCharFuzzer =
            List.range 10 127
                |> List.filter isIllegal
                |> List.map Char.fromCode
                |> fuzzSample '&'
                |> Fuzz.map String.fromChar

        illegalIdentifier =
            Fuzz.map3
                (\legalPre illegalChar legalPost -> legalPre ++ illegalChar :: legalPost)
                legalOrEmpty
                illegalCharFuzzer
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
    Fuzz.oneOf (List.map Fuzz.constant samples)
