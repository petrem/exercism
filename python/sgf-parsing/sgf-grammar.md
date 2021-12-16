https://www.red-bean.com/sgf/sgf4.html


Conventions
===========

  "..." : terminal symbols
  [...] : option: occurs at most once
  {...} : repetition: any number of times, including zero
  (...) : grouping
    |   : exclusive or
  *italics*: parameter explained at some other place

 eBNF
 ====

  Collection = GameTree { GameTree }
  GameTree   = "(" Sequence { GameTree } ")"
  Sequence   = Node { Node }
  Node       = ";" { Property }
  Property   = PropIdent PropValue { PropValue }
  PropIdent  = UcLetter { UcLetter }
  PropValue  = "[" CValueType "]"
  CValueType = (ValueType | Compose)
  ValueType  = (*None* | *Number* | *Real* | *Double* | *Color* | *SimpleText* |
               *Text* | *Point*  | *Move* | *Stone*)

 property lists
 --------------

  'list of':    PropValue { PropValue }
  'elist of':   ((PropValue { PropValue }) | None)

In other words elist is list or "[]".

Property Value Types
--------------------


  UcLetter   = "A".."Z"
  Digit      = "0".."9"
  None       = ""

  Number     = [("+"|"-")] Digit { Digit }
  Real       = Number ["." Digit { Digit }]

  Double     = ("1" | "2")
  Color      = ("B" | "W")

  SimpleText = { any character (handling see below) }
  Text       = { any character (handling see below) }

  Point      = *game-specific*
  Move       = *game-specific*
  Stone      = *game-specific*

  Compose    = *ValueType* ":" *ValueType*


Derived List of Terminals (by me)
--------------------------------
    - "(", ")"
    - ";"
    - "[", "]"
    - "+", "-"
    - ":"
    - "A".."Z"
    - "0".."9"
    - "" (glorious 𝜖mpty string)

Example
=======

SGF example:

    (;FF[4]C[root](;C[a];C[b](;C[c])
    (;C[d];C[e]))
    (;C[f](;C[g];C[h];C[i])
    (;C[j])))

This tree is written in pre-order as: `(root(ab(c)(de))(f(ghi)(j)))`
