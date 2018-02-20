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

open FParsec

module private Lexer =
    open Token

    type UserState = unit

    let parseNil = skipString Nil
    let parseTrue = skipString True >>% true
    let parseFalse = skipString False >>% false


module private Parser =
    open Lexer
    open Types

    type UserState = unit

    let parser : Parser<EDN, UserState> =
        let pNil = parseNil |>> fun () -> Nil
        let parseBool = parseFalse <|> parseTrue |>> Boolean
        choice [
            pNil
            parseBool
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

