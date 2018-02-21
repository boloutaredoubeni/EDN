module Tests


open Expecto
open EDN

module ParserTests =

    let private (|ParsedResultOk|_|) =
        function
        | Result.Ok (t, _, _) -> Some t
        | _ -> None

    let private testNil =
        test "nil" {
            let input = Token.Nil
            let (ParsedResultOk result) = Driver.runParser input
            let expected = EDN.Types.Nil
            Expect.equal expected result "nil can be parsed"
        }

    let private testTrue =
        test "true" {
            let input = Token.True
            let (ParsedResultOk result) = Driver.runParser input
            let expected = EDN.Types.Boolean true
            Expect.equal expected result "true can be parsed"
        }

    let private testFalse =
        test "false" {
            let input = Token.False
            let (ParsedResultOk result) = Driver.runParser input
            let expected = EDN.Types.Boolean false
            Expect.equal expected result "false can be parsed"
        }

    [<Tests>]
    let parseTests =
        testList "parser tests" [
            testNil
            testTrue
            testFalse
        ]

[<Tests>]
let tests =
  testList "samples" [
    testCase "Say hello all" <| fun _ ->
      let subject = Say.hello "all"
      Expect.equal subject "Hello all" "You didn't say hello"
  ]
