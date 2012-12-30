" Vim syntax file
" Language: C-alternative
" Maintainer: Overmind JIANG
" Latest Revision: 29 Dec 2012

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword CaLangKeyword if else while register return jump call switch
syn keyword CaLangKeyword import export
syn keyword CaLangType gcptr i64 i32 i16 i8
syn keyword CaLangConstant __FrameDescr__
syn match CaLangNumber '\<\d\+'
syn region CaLangString start='"' end='"'
syn match CaLangLabel '^\i*:'

" Matches
syn match CaLangComment '//.*$' skipwhite

" Regions
syn region CaLangComment start='/\*' end='\*/'

let b:current_syntax = "CaLang"

hi def link CaLangComment Comment
hi def link CaLangType Type
hi def link CaLangNumber Number
hi def link CaLangString String
hi def link CaLangConstant Constant
hi def link CaLangKeyword Statement
hi def link CaLangLabel Label

