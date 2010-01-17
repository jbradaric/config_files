" File: .vimrc
" Author: Jurica Bradarić
" Description: My .vimrc file
" Last Modified: January 17, 2010
"===================================================================================
" GENERAL SETTINGS
"===================================================================================
"
"-------------------------------------------------------------------------------
" Use Vim settings, rather then Vi settings.
" This must be first, because it changes other options as a side effect.
"-------------------------------------------------------------------------------
set nocompatible
set t_Co=256
if has("gui_running")
    colorscheme wombat
else
    colorscheme desert256
endif
set guifont=Monaco\ 10
"
"-------------------------------------------------------------------------------
" Enable file type detection. Use the default filetype settings.
" Also load indent files, to automatically do language-dependent indenting.
"-------------------------------------------------------------------------------
filetype  plugin on
filetype  indent on
"
"-------------------------------------------------------------------------------
" Switch syntax highlighting on.
"-------------------------------------------------------------------------------
syntax    on       
"-------------------------------------------------------------------------------
" Various settings
"-------------------------------------------------------------------------------
set autoindent                  " copy indent from current line
set autoread                    " read open files again when changed outside Vim
set autowrite                   " write a modified buffer on each :next , ...
set backspace=indent,eol,start  " backspacing over everything in insert mode
set backup                      " keep a backup file
set browsedir=current           " which directory to use for the file browser
set complete+=k                 " scan the files given with the 'dictionary' option
set history=50                  " keep 50 lines of command line history
set undolevels=1000				" lots and lots of undo
"set hlsearch                    " highlight the last used search pattern
set incsearch                   " do incremental searching
set listchars=tab:>.,eol:\$     " strings to use in 'list' mode
set mouse=a                     " enable the use of the mouse
set nowrap                      " do not wrap lines
set popt=left:8pc,right:3pc     " print options
set ruler                       " show the cursor position all the time
set shiftwidth=4                " number of spaces to use for each step of indent
set showcmd                     " display incomplete commands
set smartindent                 " smart autoindenting when starting a new line
set tabstop=4                   " number of spaces that a <Tab> counts for
set softtabstop=4				" make vim see 4 spaces as tab
set expandtab					" insert spaces for tabs
set wildignore=*.bak,*.o,*.e,*~ " wildmenu: ignore these extensions
set wildmenu                    " command-line completion in an enhanced mode
set number						" turn the line numbers on
set ve=block                    " set virtualedit mode to block
"
"-------------------------------------------------------------------------------
" Set the backup and temporary directories
"-------------------------------------------------------------------------------
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp
"
"-------------------------------------------------------------------------------
" Automatically cd into the directory that the file is in
"-------------------------------------------------------------------------------
set autochdir
"
"-------------------------------------------------------------------------------

"-------------------------------------------------------------------------------
"  some additional hot keys
"-------------------------------------------------------------------------------
"    C-S   -  write file without confirmation
"    F3   -  call file explorer Ex
"    F4   -  show tag under cursor in the preview window (tagfile must exist!)
"    F5   -  open quickfix error window
"    F6   -  close quickfix error window
"    F7   -  display previous error
"    F8   -  display next error   
"-------------------------------------------------------------------------------
map   <silent> <C-S>        :write<CR>
map   <silent> <F3>        :Explore<CR>
nmap  <silent> <F4>        :exe ":ptag ".expand("<cword>")<CR>
map   <silent> <F5>        :copen<CR>
map   <silent> <F6>        :cclose<CR>
map   <silent> <F7>        :cp<CR>
map   <silent> <F8>        :cn<CR>
"
imap  <silent> <C-S>   <Esc>:write<CR>a
imap  <silent> <F3>   <Esc>:Explore<CR>
imap  <silent> <F4>   <Esc>:exe ":ptag ".expand("<cword>")<CR>
imap  <silent> <F5>   <Esc>:copen<CR>
imap  <silent> <F6>   <Esc>:cclose<CR>
imap  <silent> <F7>   <Esc>:cp<CR>
imap  <silent> <F8>   <Esc>:cn<CR>
"-------------------------------------------------------------------------------
" Map Ctrl-F12 to build the ctags database in the current directory
"-------------------------------------------------------------------------------
:au Filetype cpp map <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q --languages=c++ .<CR>
:au Filetype php map <C-F12> :!ctags -R --php-kinds=+cf --fields=+iaS --extra=+q --languages=php .<CR>
"
"-------------------------------------------------------------------------------
" Start gvim maximized
"-------------------------------------------------------------------------------
"set lines=44
"set columns=143
"
"-------------------------------------------------------------------------------
" Fast switching between buffers
" The current buffer will be saved before switching to the next one.
" Choose :bprevious or :bnext
"-------------------------------------------------------------------------------
"
" map  <silent> <s-tab>  <Esc>:if &modifiable && !&readonly && 
"     \                  &modified <CR> :write<CR> :endif<CR>:bprevious<CR>
"imap  <silent> <s-tab>  <Esc>:if &modifiable && !&readonly && 
"     \                  &modified <CR> :write<CR> :endif<CR>:bprevious<CR>
" map  <silent> <s-tab>	 <Esc>:tabnext<CR>
"imap  <silent> <s-tab>	 <Esc>:tabnext<CR>
imap  <silent> <F9>		 <Esc>:!g++ % -o %:s/\.cpp//<CR>
 map  <silent> <F9>		 <Esc>:!g++ % -o %:s/\.cpp//<CR>
imap  <silent> <F10>	 <Esc>:!./%:s/\.cpp//<CR>
 map  <silent> <F10>	 <Esc>:!./%:s/\.cpp//<CR>
imap  <C-F11>			 <Esc>:w !diff % -<CR>
 map  <C-F11>            :w !diff % -<CR>
"
"-------------------------------------------------------------------------------
" autocomplete parenthesis, brackets and braces
"-------------------------------------------------------------------------------
"inoremap ( ()<Left>
"inoremap [ []<Left>
"inoremap { {}<Left>
"-------------------------------------------------------------------------------
" add a new line after { and ; 
"-------------------------------------------------------------------------------
"inoremap { {<CR>
"inoremap ; ;<CR>
"
vnoremap ( s()<Esc>P<Right>%
vnoremap [ s[]<Esc>P<Right>%
vnoremap { s{}<Esc>P<Right>%
"-------------------------------------------------------------------------------
" autocomplete quotes (visual and select mode)
"-------------------------------------------------------------------------------
xnoremap  '  s''<Esc>P<Right>
xnoremap  "  s""<Esc>P<Right>
xnoremap  `  s``<Esc>P<Right>
"
"-------------------------------------------------------------------------------
" PhpDoc settings
"-------------------------------------------------------------------------------
source ~/.vim/plugin/php-doc.vim 
inoremap <C-C> <Esc>:call PhpDocSingle()<CR>a
nnoremap <C-C> :call PhpDocSingle()<CR>
vnoremap <C-C> :call PhpDocSingle()<CR>
"-------------------------------------------------------------------------------
" gui options
"-------------------------------------------------------------------------------
if has('gui_running')
	set columns=145			" width of the window
	set lines=46			" height of the window

	set guioptions-=T		" kill toolbar
	set guioptions-=m		" kill menu
	set guioptions-=r		" kill right scrollbar
	set guioptions-=l		" kill left scrollbar
	set guioptions-=L		" kill left scrollbar with multiple buffers
endif
"-------------------------------------------------------------------------------
" omnicomplete setup
"-------------------------------------------------------------------------------
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType php set omnifunc=phpcomplete#Complete
autocmd FileType cpp set omnifunc=cppcomplete#Complete
"-------------------------------------------------------------------------------
" Setup tags and autocmplete for cpp
"-------------------------------------------------------------------------------
let OmniCpp_NamespaceSearch = 1
let OmniCpp_GlobalScopeSearch = 1
let OmniCpp_ShowAccess = 1
let OmniCpp_MayCompleteDot = 1
let OmniCpp_MayCompleteArrow = 1
let OmniCpp_MayCompleteScope = 1
let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]
" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview
"-------------------------------------------------------------------------------
" Setup compiler for c/c++
"-------------------------------------------------------------------------------
au FileType c set makeprg=gcc\ %\ \-o\ %:s/\.c//
au FileType cpp set makeprg=g++\ %\ \-o\ %:s/\.cpp//
"-------------------------------------------------------------------------------
" Haskell setup
"-------------------------------------------------------------------------------
autocmd Bufenter *.hs compiler ghc
let g:haddock_browser = "/opt/swiftfox/swiftfox"
"-------------------------------------------------------------------------------
" Save and restore folds when closing and re-opening the file
au BufWinLeave ?* mkview
au BufWinEnter ?* silent loadview
"-------------------------------------------------------------------------------
" Use Alt-a to open/close blocks of {code}
"-------------------------------------------------------------------------------
syn region myFold start="{" end="}" transparent fold
syn region Comment start="/\*" end="\*/" fold
syn sync fromstart
set foldmethod=syntax
map <A-a> za
imap <A-a> <Esc>za<CR>a
"-------------------------------------------------------------------------------
" Reduce a sequence of blank (;b) lines into a single line
"-------------------------------------------------------------------------------
map ;b		GoZ<Esc>:g/^$/.,/./-j<CR>Gdd
"imap <C-F12>	GoZ<Esc>:g/^$/.,/./-j<CR>Gddi
"-------------------------------------------------------------------------------
" Set ignore case unless the search term is in uppercase
"-------------------------------------------------------------------------------
set ignorecase
set smartcase
"-------------------------------------------------------------------------------
" Quick jumping between splits
"-------------------------------------------------------------------------------
map <C-J>	<C-W>j<C-W>
imap <C-J>	<C-W>j<C-W>
map <C-K>	<C-W>k<C-W>
imap <C-K>	<C-W>k<C-W>
map <C-L>	<C-W>l<C-W>
imap <C-L>	<C-W>l<C-W>
map <C-H>	<C-W>h<C-W>
imap <C-H>	<C-W>h<C-W>
"
"-------------------------------------------------------------------------------
" C-S-E		Split window vertically
" C-S-O		Split window horizontally
"-------------------------------------------------------------------------------
map <C-S-E>		:vsplit<CR>
imap <C-S-E>	<Esc>:vsplit<CR>i
map <C-S-O>		:split<CR>
imap <C-S-O>	<Esc>:split<CR>i
"
"-------------------------------------------------------------------------------
" Settings for Latex-Suite 
"-------------------------------------------------------------------------------
set grepprg=grep\ -nH\ $*
filetype indent on
let g:tex_flavor='latex'
"
"-------------------------------------------------------------------------------
" Closetag and HTML/XML preferences
"-------------------------------------------------------------------------------
let g:closetag_default_xml=1
autocmd FileType html,htmldjango,htmljinja,eruby,mako let b:closetag_html_style=1
autocmd FileType html,xhtml,xml,htmldjango,htmljinja,eruby,mako source ~/.vim/scripts/closetag.vim
autocmd FileType html,xhtml,xml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType vimrc set filetype=vim
"
"-------------------------------------------------------------------------------
" Minibufexplorer settings
"-------------------------------------------------------------------------------
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1 
imap  <C-Tab> <Esc>:bnext!<CR>
"-------------------------------------------------------------------------------
" PHP helpers
"-------------------------------------------------------------------------------
" PHP parser check (Ctrl + L)
:autocmd FileType php noremap <C-L> :!php -l %<CR>
" -------------------------------------------------------------------------------
" Shortcuts to open the Project and Taglist plugins
" -------------------------------------------------------------------------------
imap    <C-F7>  <Esc>:TlistToggle<CR>
nmap    <C-F7>  :TlistToggle<CR>
imap    <C-F6>  <Esc>:Project<CR>
nmap    <C-F6>  <Plug>ToggleProject<CR>
"-------------------------------------------------------------------------------
" C/C++ comments
"-------------------------------------------------------------------------------
autocmd FileType c,cpp,h,hpp set comments=sl:/**,mb:\ *,elx:\ */
