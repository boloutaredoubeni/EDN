namespace EDN

module Types =

    type Symbol =
        struct
            val Prefix: string
            val Name: string
        end

    type EDN =
        | Nil
        | Boolean of b: bool
        | String of s: string
        | Character of c: char
        | Symbol of symbol: Symbol
        | Keyword of keyword: Symbol
        | Integer of i: bigint
        | Float of f: double
        | List of l: list<EDN>
        | Map of m: Map<EDN, EDN>
        | Set of set: Set<EDN>
        | Tag of tag: Symbol * repr: EDN
        | Discard of element: EDN

module Token =
    let [<Literal>] Nil = "nil"
    let [<Literal>] True = "true"
    let [<Literal>] False = "false"
    let [<Literal>] DoubleQuote = "\""
    let [<Literal>] BackSlash = "\\"
    let [<Literal>] NewLine = '\n'
    let [<Literal>] Tab = '\t'
    let [<Literal>] Space = ' '
    let [<Literal>] EscapeCharaters = "\\nrt\""
    let [<Literal>] CharDoubleQuote = '"'
    let [<Literal>] CharBackSlash = '\\'
    let [<Literal>] NewLineCharString = "newline"
    let [<Literal>] ReturnCharString = "return"
    let [<Literal>] SpaceCharString = "space"
    let [<Literal>] TabCharString = "tab"

open FParsec

module private Lexer =
    open Token

    type UserState = unit

    let skipNil = skipString Nil
    let skipTrue = skipString True >>% true
    let skipFalse = skipString False >>% false
    let skipDoubleQuote = skipString DoubleQuote
    let parseBackSlash = pstring BackSlash

    let parseEscapeCharacter = anyOf EscapeCharaters
    let skipCharBackSlash : Parser<unit, UserState> = skipChar CharBackSlash
    let parseNewLineCharString : Parser<string, UserState> = pstring NewLineCharString
    let parseReturnCharString = pstring ReturnCharString
    let parseTabCharString = pstring TabCharString
    let parseSpaceCharString = pstring SpaceCharString

module private Parser =
    open Lexer
    open Types

    type UserState = unit

    let parser : Parser<EDN, UserState> =
        let pNil = skipNil |>> fun () -> Nil
        let parseBool = skipFalse <|> skipTrue |>> Boolean
        let parseString =
            let normalChar = satisfy (fun ch -> ch <> Token.CharBackSlash && ch <> Token.CharDoubleQuote)
            let unescapedChar =
                let unescape ch =
                    match ch with
                    | 'n' -> '\n'
                    | 'r' -> '\r'
                    | 't' -> '\t'
                    |  c  -> c
                parseBackSlash >>. (parseEscapeCharacter  |>> unescape)
            let parseContents = manyChars (normalChar <|> unescapedChar)
            between skipDoubleQuote skipDoubleQuote parseContents
            |>> String
        let parseCharacter =
            skipCharBackSlash
            >>. choice [
                parseNewLineCharString
                parseReturnCharString
                parseTabCharString
                parseSpaceCharString
                noneOf [
                    ' '
                    '\r'
                    '\t'
                    '\n'
                ] |>> string
            ]
            |>> function
            | Token.NewLineCharString -> Character '\n'
            | Token.ReturnCharString -> Character '\r'
            | Token.SpaceCharString -> Character ' '
            | Token.TabCharString -> Character '\t'
            | s -> Character s.[0]
        choice [
            pNil
            parseBool
            parseString
            parseCharacter
        ]

[<AutoOpen>]
module FParsecUtil =
    let parseResultToFSResult =
        function
        | ParserResult.Success (result, userstate, position) -> Result.Ok (result, userstate, position)
        | Failure (message, error, userstate) -> Result.Error (message, error, userstate)

[<RequireQualifiedAccess>]
module Driver =
    open Parser
    let runParser input = run parser input |> parseResultToFSResult

module Say =
    let hello name =
        sprintf "Hello %s" name

