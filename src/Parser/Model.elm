module Tasks.Parser.Model exposing (..)

import Tasks.Entry.Model exposing (..)

type Tag
    = TCheckbox
    | TBullet
    | TNumbered String
    | THeading Int
    | THorizontalLine
    | TBlank

    | TDesc String
    | TDue String
    | TPri Int

    | TLineBreak
    | TEmpty

type ParseResult = LineResult Details
                --  | EmptyResult Props
