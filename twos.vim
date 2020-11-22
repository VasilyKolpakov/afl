if exists("b:current_syntax")
  finish
endif


" Keywords
syn keyword twosKeyword def while

syn match twosNumber '[a-zA-Z-_]\w*'

syn match twosNumber '\d\+'
syn match twosNumber '[-+]\d\+'

syn region twosCodeBlock start="{" end="}" fold transparent

syn region twosString start='"' end='"' 

syn match  twosComment      "#.*"

let b:current_syntax = "twos"


highlight link twosIdentifier Function
highlight link twosKeyword Keyword
highlight link twosNumber Constant
highlight link twosString String
highlight link twosCodeBlock Statement
highlight link twosComment Comment
