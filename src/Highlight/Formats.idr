module Highlight.Formats

import Highlight.Regions
import Control.Monad.State

import Debug.Trace

||| A description of how to annotate a source file to output to a particular format.
public export
record Format where
  constructor MkFormat
  openTag   : HighlightType -> String
  closeTag  : HighlightType -> String
  escape    : Char -> String
  preamble  : String
  postamble : String

private total
openTagTeX : HighlightType -> String
openTagTeX (Name Function _ _)        = "\\IdrisFunction{"
openTagTeX (Name Constructor _ _)     = "\\IdrisData{"
openTagTeX (Name TypeConstructor _ _) = "\\IdrisType{"
openTagTeX (Bound _)                  = "\\IdrisBound{"
openTagTeX Keyword                    = "\\IdrisKeyword{"

private total
closeTagTeX : HighlightType -> String
closeTagTeX _ = "}"

private total
escapeTeX : Char -> String
escapeTeX '{'  = "\\{"
escapeTeX '}'  = "\\}"
escapeTeX '\\' = "\\textbackslash{}"
escapeTeX '~'  = "\\textasciitilde{}"
escapeTeX '|'  = "\\textbar{}"
escapeTeX '$'  = "\\textdollar{}"
escapeTeX '%'  = "\\textpercent{}"
escapeTeX c    = singleton c

private total
preambleLaTeX : String
preambleLaTeX =
  """\documentclass{article}
\usepackage{fancyvrb}
\usepackage[usenames]{xcolor}
\newcommand{\IdrisData}[1]{\textcolor{red}{#1}}
\newcommand{\IdrisType}[1]{\textcolor{blue}{#1}}
\newcommand{\IdrisBound}[1]{\textcolor{magenta}{#1}}
\newcommand{\IdrisFunction}[1]{\textcolor{green}{#1}}
\newcommand{\IdrisKeyword}[1]{{\underline{#1}}}
\newcommand{\IdrisImplicit}[1]{{\itshape \IdrisBound{#1}}}


\begin{document}
% START CODE
\begin{Verbatim}[commandchars=\\\{\}]
"""

private total
postambleLaTeX : String
postambleLaTeX =
  """\end{Verbatim}
% END CODE
\end{document}
"""

||| LaTeX-style highlights
export total
LaTeX : Format
LaTeX = MkFormat openTagTeX closeTagTeX escapeTeX preambleLaTeX postambleLaTeX

private total
preambleHTML : String
preambleHTML =
  """<!doctype html><html><head><style>.idris-data { color: red; }
.idris-type { color: blue; }
.idris-function {color: green; }
.idris-keyword { font-weight: bold; }
.idris-bound { color: purple; }
.idris-implicit { font-style: italic; }
.idris-underlined { text-decoration: underline; }</style></head><body><!-- START CODE --><pre>"""

private total
postambleHTML : String
postambleHTML = "</pre><!-- END CODE --></body></html>"

private total
openTagHTML : HighlightType -> String
openTagHTML (Name Function d t)        = "<span class=\"idris-function\" title=\"" ++ d ++ "\n" ++ t ++ "\">"
openTagHTML (Name Constructor d t)     = "<span class=\"idris-data\" title=\"" ++ d ++ "\n" ++ t ++ "\">"
openTagHTML (Name TypeConstructor d t) = "<span class=\"idris-type\" title=\"" ++ d ++ "\n" ++ t ++ "\">"
openTagHTML (Bound i)                  = "<span class=\"idris-bound" ++
                                         (if i then " idris-implicit" else "") ++
                                         "\">"
openTagHTML Keyword                    = "<span class=\"idris-keyword\">"

private total
closeTagHTML : HighlightType -> String
closeTagHTML hl = "</span><!-- closing " ++ show hl ++ "-->"

private total
escapeHTML : Char -> String
escapeHTML '&' = "&amp;"
escapeHTML '<' = "&lt;"
escapeHTML '>' = "&gt;"
escapeHTML c = singleton c

||| HTML-style highlights
export total
HTML : Format
HTML = MkFormat openTagHTML closeTagHTML escapeHTML preambleHTML postambleHTML

private total
openTagANSI : HighlightType -> String
openTagANSI (Name Function _ _)        = "\x1b[32m"
openTagANSI (Name Constructor _ _)     = "\x1b[31m"
openTagANSI (Name TypeConstructor _ _) = "\x1b[34m"
openTagANSI (Bound _)                  = "\x1b[35m"
openTagANSI Keyword                    = "\x1b[4m"

private total
closeTagANSI : HighlightType -> String
closeTagANSI _ = "\x1b[0m"

||| ANSI-style highlights
export total
ANSI : Format
ANSI = MkFormat openTagANSI closeTagANSI singleton "" ""

escapeConTeXt : Char -> String
escapeConTeXt '\n' = "\\dontleavehmode\n"
escapeConTeXt ' ' = "\\ "
escapeConTeXt c = escapeTeX c

-- ConTeXt MkIV's \definetyping doesn't handle escape=commands anymore
-- escape=yes or escape={...,...} are problematic because commands may be embedded
private total
preambleConTeXt : String
preambleConTeXt =
  """\def\IdrisData#1{\color[red]{#1}}
\def\IdrisType#1{\color[blue]{#1}}
\def\IdrisBound#1{\color[magenta]{#1}}
\def\IdrisFunction#1{\color[green]{#1}}
\def\IdrisKeyword#1{\underbar{#1}}
\def\IdrisImplicit#1{{\it \IdrisBound{#1}}}

\starttext
{\obeylines\tt
"""

private total
postambleConTeXt : String
postambleConTeXt =
  """}
\stoptext
"""

||| ConTeXt-style highlights
export total
ConTeXt : Format
ConTeXt = MkFormat openTagTeX closeTagTeX escapeConTeXt preambleConTeXt postambleConTeXt

------------------------------------
-- Applying formats to file contents
------------------------------------
private
record HLState where
  constructor MkHlState
  lineNo : Integer
  colNo : Integer
  highlights : List (Region HighlightType)
  openHighlights : List (Region HighlightType)
  output : List String

private
incCol : HLState -> HLState
incCol st = record {colNo = 1 + colNo st} st

private
incLine : HLState -> HLState
incLine st = record {lineNo = 1 + lineNo st} st

private
doOutput : String -> State HLState ()
doOutput str = do st <- get
                  put (record {output = str :: output st} st)

private
openAll : (line, col : Integer) -> Format -> State HLState ()
openAll line col fmt =
    do hls <- highlights <$> get
       let (toOpen, notYet) = List.span (\hl => startLine hl < line ||
                                                (startLine hl == line &&
                                                 startColumn hl <= col))
                                         hls
       traverse_ (doOutput . openTag fmt . metadata) toOpen
       st <- get
       put (record { highlights = notYet
                   , openHighlights = reverse toOpen ++ openHighlights st
                   } st)

||| Close all currently-open highlights
closeAll : (line, col : Integer) -> Format -> State HLState ()
closeAll line col fmt =
    do openHls <- openHighlights <$> get
       let (toClose, notYet) = span (\hl => endLine hl < line ||
                                          (endLine hl == line &&
                                           endColumn hl <= col))
                                    openHls
       traverse_ (doOutput . closeTag fmt . metadata) toClose
       st <- get
       put (record {openHighlights = notYet} st)


||| Apply highlighting to a source file for some output format
export
highlight : Format -> (fileContents : String) -> List (Region HighlightType) -> String
highlight fmt fileContents hls =
    preamble fmt ++
    concat (evalState (highlight' (split isNL fileContents) []) (MkHlState 0 0 (sort hls) [] [])) ++
    postamble fmt
  where highlight' : List String -> List Char -> State HLState (List String)
        highlight' [] [] =
            do traverse_ (doOutput . closeTag fmt . metadata) (openHighlights !get)
               pure (reverse (output !get))
        highlight' lines (c::cs) =
            do modify incCol
               line <- lineNo <$> get
               col <- colNo <$> get
               openAll line col fmt
               closeAll line col fmt
               doOutput $ escape fmt c
               highlight' lines cs
        highlight' (l::lines) [] =
            do modify incCol
               closeAll (lineNo !get) (colNo !get) fmt
               modify incLine
               modify (record {colNo = 0})
               doOutput $ escape fmt '\n'
               highlight' lines (unpack l)
