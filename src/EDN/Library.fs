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


module Say =
    let hello name =
        sprintf "Hello %s" name

