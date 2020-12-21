if exists("b:current_syntax")
  finish
endif


" Keywords
syn keyword twosKeyword def constant if while and or not r_peek
syn keyword twosKeyword drop swap dup 2dup 2drop 3drop 3dup rot -rot over
syn match   twosKeyword '\v\>r'
syn match   twosKeyword '\vr\>'
syn match   twosKeyword '\vr\@'
syn keyword twosBoolean true false

syn match twosIdentifier '\d*[a-zA-Z-_]\w*' contains=twosKeyword

syn match twosNumber '\d\+\([a-zA-Z]\)\@!'
syn match twosNumber '[-+]\d\+\([a-zA-Z]\)\@!'

syn region twosCodeBlock start="{" end="}" fold transparent

syn region twosString start='"' end='"' 

syn match  twosComment      "#.*"

let b:current_syntax = "twos"


highlight link twosKeyword Keyword
highlight link twosIdentifier Normal
highlight link twosNumber Number
highlight link twosBoolean Boolean
highlight link twosString String
highlight link twosCodeBlock Statement
highlight link twosComment Comment
