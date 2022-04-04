// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
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

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
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

    //Arith (and char) parse
    let TermParse, tref = createParserForwardedToRef<aExp>() //loosest
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>() //hardest

    let CParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"

    let NParse   = pint32 |>> N <?> "Int"
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul ((N -1), x)) <?> "Neg"
    //let NegParse = (unop (pchar '-') pint32 |>> (fun y -> y * (-1))) |>> N <?> "Neg"
    let PVParse   = unop pPointValue AtomParse |>> PV <?> "PV"
    let IdParse   = pid |>> V <?> "Variable"
    let ParParse = parenthesise TermParse <?> "Par"
    let CharToIntParse = pCharToInt  >*>. (parenthesise CParse) |>> CharToInt <?> "CharToInt"

    do tref := choice [AddParse; SubParse; ProdParse]
    do pref := choice [MulParse; ModParse; DivParse; AtomParse]
    do aref := choice [NegParse; PVParse; NParse; ParParse; CharToIntParse; IdParse]

    let AexpParse = TermParse 

    //Char parse
    let CharParse =   pchar '\'' >>. anyChar .>> pchar '\'' |>> C <?> "Char" 
    let CharValParse = (>*>.) pCharValue ParParse |>> CV <?> "CV"
    let IntToCharParse = pIntToChar >*>. ParParse |>> IntToChar <?> "IntToChar"
    let ToLowerParse = pToLower >*>. (parenthesise CParse) |>> ToLower <?> "ToLower"
    let ToUpperParse = pToUpper >*>. (parenthesise CParse) |>> ToUpper <?> "ToUpper"

    do cref := choice [CharParse; CharValParse; ToUpperParse; ToLowerParse; IntToCharParse]

    let CexpParse = CParse

    //Boolean parse
    let JunctionBoolParse, jref = createParserForwardedToRef<bExp>() //loosest
    let ArithBoolParse, abref = createParserForwardedToRef<bExp>()
    let ValidateBoolParse, vref = createParserForwardedToRef<bExp>() //hardest

    let ConjParse = ArithBoolParse .>*> pstring @"/\" .>*>. JunctionBoolParse |>> Conj <?> "Conj" 
    let DisjParse = ArithBoolParse .>*> pstring @"\/" .>*>. JunctionBoolParse |>> (fun (x,y) -> Not (Conj (Not (x), Not (y)))) <?> "Disj" 
    do jref := choice [ConjParse; DisjParse; ArithBoolParse]

    let EqParse = AtomParse .>*> pchar '=' .>*>. AtomParse  |>> AEq <?> "AEq"
    let UnEqParse = AtomParse .>*> pstring "<>" .>*>. AtomParse |>> (fun (x,y) -> Not (AEq (x,y))) <?> "AUnEq" 
    let LtParse = AtomParse .>*> pchar '<' .>*>. AtomParse  |>> ALt <?> "ALt"
    let LtoEtParse =  TermParse .>*> pstring "<=" .>*>. TermParse |>> (fun (x, y) -> Not (Conj (Not (ALt (x, y)), Not (Not (Not (AEq (x, y))))))) <?> "ALtoEt"
    let GtParse = TermParse .>*> pchar '>' .>*>. TermParse |>> (fun (x, y) ->  (Conj (Not (AEq (x,y)), Not (ALt (x,y))))) <?> "AGt"
    let GtoEtParse =  TermParse .>*> pstring ">=" .>*>. TermParse |>> (fun (x,y) -> Not (ALt (x,y))) <?> "AGt"
    do abref := choice [LtParse; LtoEtParse; GtParse; GtoEtParse; EqParse; UnEqParse; ValidateBoolParse]

    let TParse = pTrue |>> (fun _ -> TT) <?> "True"
    let FParse = pFalse |>> (fun _ -> FF) <?> "False"
    let IsLetterParse = unop pIsLetter (parenthesise CParse) |>> IsLetter <?> "IsLetter"
    let IsVowelParse = unop pIsVowel (parenthesise CParse) |>> IsVowel <?> "IsVowel"
    let IsDigitParse = unop pIsDigit (parenthesise CParse) |>> IsDigit <?> "IsDigit"
    let NotParse = pchar '~' >*>. JunctionBoolParse |>> Not <?> "Not"
    let ParBoolParse = parenthesise JunctionBoolParse <?> "ParBool"

    do vref := choice [TParse; FParse; IsLetterParse; IsLetterParse; IsVowelParse; IsDigitParse; NotParse; ParBoolParse]

    let BexpParse = JunctionBoolParse

    //Statement parse
    let SequenceParse, ssref = createParserForwardedToRef<stm>()
    let StatementParse, sref = createParserForwardedToRef<stm>()

    let SeqParse =  StatementParse .>*> pchar ';' .>*>. SequenceParse |>> Seq <?> "Seq"
    do ssref := choice [SeqParse; StatementParse]

    let AssParse = pid .>*> pstring ":=" .>*>. AexpParse |>> Ass <?> "Ass"
    let DeclareParse = pdeclare >>. spaces1 >*>. pid |>> Declare <?> "Declare"
    let ITEParse = pif >*>. ParBoolParse .>*> pthen .>*>. curlyBrackets SequenceParse .>*> pelse .>*>. curlyBrackets SequenceParse |>> (fun ((x,y),z) -> ITE(x, y, z)) <?> "ITE"
    let ITParse = pif >*>. ParBoolParse .>*> pthen .>*>. curlyBrackets SequenceParse |>> (fun (x,y) -> ITE (x, y, Skip)) <?> "IT"
    let WhileParse =  pwhile .>> spaces1 >>. ParBoolParse .>> spaces1 .>> pdo .>> spaces1 .>>. curlyBrackets SequenceParse |>> While <?> "While"
    
    do sref := choice [AssParse; DeclareParse; WhileParse; ITEParse; ITParse]

    let stmParse = SequenceParse


    (* The rest of your parser goes here *)

    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>

    type boardFun2 = coord -> Result<square option, Error>

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}