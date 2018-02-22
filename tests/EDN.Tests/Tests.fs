module Tests


open Expecto
open EDN

module ParserTests =

    let private (|ParsedResultOk|_|) =
        function
        | Result.Ok (t, _, _) -> Some t
        | Error (msg, _, _) -> failwith msg

    let private testNil =
        test Token.Nil {
            let input = Token.Nil
            let (ParsedResultOk result) = Driver.runParser input
            let expected = EDN.Types.Nil
            Expect.equal expected result "nil can't be parsed"
        }

    let private testTrue =
        test Token.True {
            let input = Token.True
            let (ParsedResultOk result) = Driver.runParser input
            let expected = EDN.Types.Boolean true
            Expect.equal expected result "true can't be parsed"
        }

    let private testFalse =
        test Token.False {
            let input = Token.False
            let (ParsedResultOk result) = Driver.runParser input
            let expected = EDN.Types.Boolean false
            Expect.equal expected result "false can't be parsed"
        }

    let private testString =
        let hello = "hello world"
        let ``hello world`` = sprintf "\"%s\"" hello
        test ``hello world`` {
            let input = ``hello world``
            let (ParsedResultOk result) = Driver.runParser input
            let expected = EDN.Types.String hello
            Expect.equal expected result "basic string can't be parsed"
        }

    let private testEscape =
        let escape = "print\ta newline\nagain\n"
        test escape {
            let (ParsedResultOk result) = Driver.runParser (sprintf "\"%s\"" escape)
            let expected = EDN.Types.String escape
            Expect.equal expected result "escape characters can't be parsed"
        }

    let private testSpecialChar =
        let space = "\\space"
        test space {
            let (ParsedResultOk result) = Driver.runParser space
            let expected = EDN.Types.Character ' '
            Expect.equal expected result "special character can't be parsed"
        }

    let private testAnyChar =
        let ch = "\\|"
        test ch {
            let (ParsedResultOk result) = Driver.runParser ch
            let expected = EDN.Types.Character '|'
            Expect.equal expected result "character can't be parsed"
        }


    [<Tests>]
    let parseTests =
        testList "parser tests" [
            testNil
            testTrue
            testFalse
            testString
            testEscape
            testSpecialChar
            testAnyChar
        ]

[<Tests>]
let tests =
  testList "samples" [
    testCase "Say hello all" <| fun _ ->
      let subject = Say.hello "all"
      Expect.equal subject "Hello all" "You didn't say hello"
  ]
