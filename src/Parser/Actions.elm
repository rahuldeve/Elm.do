module Tasks.Parser.Actions exposing (..)

import Parser exposing (..)
import Set exposing (..)
import Char exposing (..)
import Maybe exposing (..)

import Tasks.Parser.Model exposing (..)
import Tasks.Entry.Model exposing (..)

spaces : Parser String
spaces =
    keep zeroOrMore (\c -> c == ' ')


space : Parser String
space =
    keep (Exactly 1) (\c -> c == ' ')


isVarChar : Char -> Bool
isVarChar char =
    not <| Set.member char (Set.fromList [ '!', '>', '#', ' ' ])


word : Parser String
word =
    succeed (++)
        |= keep oneOrMore isVarChar
        |= spaces


words : Parser String
words =
    Parser.map (String.join " ") (repeat oneOrMore word)





checkbox : Parser (List Tag)
checkbox =
    succeed ([ TCheckbox ])
        |. symbol "[]"
        |. spaces

bullet : Parser(List Tag)
bullet = 
    succeed [ TBullet ]
        |. symbol "*"
        |. spaces


numbered : Parser (List Tag)
numbered =
    succeed (\x -> [ TNumbered x ])
        |= keep oneOrMore isDigit
        |. symbol "."
        |. spaces


heading : Parser (List Tag)
heading = 
    succeed (\x -> [ THeading <| String.length x ])
        |= keep oneOrMore (\c -> c=='#')
        |. spaces


blank : Parser (List Tag)
blank = 
    succeed [ TBlank ]


due : Parser (List Tag)
due =
    succeed (\x -> [ TDue (String.trim x) ])
        |. spaces
        |. symbol "@"
        |= words


priority : Parser (List Tag)
priority =
    succeed (\x -> [ TPri x ])
        |. spaces
        |= Parser.map List.length (repeat oneOrMore (symbol "!"))


desc : Parser (List Tag)
desc =
    succeed (\x -> [ TDesc (String.trim x) ])
        |= words


opt : Parser (List Tag)
opt =
    Parser.map List.concat <| repeat zeroOrMore (Parser.oneOf [ due, priority ])


withOpt : Parser (List Tag)
withOpt =
    succeed identity
        |. symbol ">>"
        |= opt
        |. spaces


withoutOpt : Parser (List a)
withoutOpt =
    succeed []


entryParser : Parser (List Tag)
entryParser =
    succeed (\x y z -> List.concat [ x, y, z ])
        |= oneOf [ checkbox, bullet, numbered, heading, blank ]
        |= desc
        |= oneOf [ withOpt, withoutOpt ]



lineBreakParser : Parser (List Tag)
lineBreakParser =
    succeed [TLineBreak]
        |. symbol "---"


emptyParser : Parser (List a)
emptyParser =
    succeed []


parseOpts : List Tag -> List Optional
parseOpts xs =
    case xs of
        (TDue due) :: zs ->
            Due due :: parseOpts zs

        (TPri pri) :: zs ->
            Priority pri :: parseOpts zs

        _ ->
            []



constructor : List Tag -> ParseResult
constructor xs =
    case xs of
        (TCheckbox) :: (TDesc desc) :: opts ->
            LineResult { etype = Checkbox, desc = desc, opt = parseOpts opts }

        (TBullet) :: (TDesc desc) :: opts ->
            LineResult { etype = Bullet, desc = desc, opt = parseOpts opts }

        (TNumbered num) :: (TDesc desc) :: opts ->
            LineResult { etype = Numbered num, desc = desc, opt = parseOpts opts }

        (THeading level) :: (TDesc desc) :: opts ->
            LineResult { etype = Heading level, desc = desc, opt = parseOpts opts }

        (TBlank) :: (TDesc desc) :: opts ->
            LineResult { etype = Text, desc = desc, opt = parseOpts opts }

        [TLineBreak] -> LineResult { etype = LineBreak, desc = "---", opt = [] }
            
        -- [TEmpty] -> EmptyResult False

        _ -> LineResult { etype = Empty, desc = "", opt = [] }
            


parser : Parser ParseResult
parser =
    succeed constructor
        |= oneOf [ lineBreakParser, entryParser, emptyParser ]


run : Parser a -> String -> Result Error a
run = Parser.run


parseDescription : String -> Maybe Details
parseDescription str =
    let
        result =
            run parser str
    in
    case result of
        Ok (LineResult details) -> Just details
            -- Line meta details

        -- Ok (EmptyResult props) -> Nothing
            -- Empty meta props

        _                       -> Nothing
            -- Empty meta False
