// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let  whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar  <?> "space1"

    let (.>*>.) p1 p2 = (.>>.) p1 ((>>.) spaces p2) <?> "connective .>*>."
    let (.>*>) p1 p2  = (.>>) p1 ((>>.) spaces p2 ) <?> "connective .>*>"
        
    let (>*>.) p1 p2  = (>>.)  p1 ((>>.) spaces p2) <?> "connective >*>."

    let parenthesise p = (pchar '(' >*>. p) .>*> pchar ')' <?> "parenthesize"

    let pid = 
        let aux list =
            System.String (list |> List.toArray)
        
        (pchar '_' <|> pletter) .>>. many (pchar '_' <|> palphanumeric) |>> (fun (x,y) -> (string x) + (aux y))
    
    let curlyBrackets p = (pchar '{' >*>. p) .>*> pchar '}'
    
    let unop p a  = 
        p >*>. a <?> "unop"
    let binop op p1 p2  =
        (p1 .>*>. op ) .>*>. p2 |>> fun ((x,y),z) -> (x,z) 

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [NParse; ParParse]

    let AexpParse = TermParse 

    let CexpParse = pstring "not implemented"

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    let parseSquareFun _ = failwith "not implemented"

    let parseBoardFun _ = failwith "not implemented"

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let parseBoardProg (bp : boardProg) : board = failwith "not implemented"

