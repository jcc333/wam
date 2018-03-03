module ParserTest
    open PrologAST
    open PrologFrontend

    open FParsec
    open FParsec.CharParsers
    open System.Threading

    open NUnit.Framework


        [<Test>]
        let ``Test that a basic integer can be parsed``() =
            let expectedTerm = Some(AtomTerm(Atom.Symbol("atom")))
            let dummy = new obj()
            let parseResult = CharParsers.runParserOnString PrologFrontend.Parser.parser dummy "test-code" "atom"
            let actualTerm =
                match parseResult with
                | Success(term, _, posn) -> Some(term)
                | Failure(str, err, _) -> None

            Assert.AreEqual(expectedTerm, actualTerm)
            
        
       // testList "parsing atoms" [
       //     testCase "parses simple atoms" <| fun _ ->
       // ]

