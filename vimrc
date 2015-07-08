" This is Kevin Pratt's .vimrc file
" vim:set ts=2 sts=2 sw=2 expandtab:

if filereadable(expand("~/.vimrc.bundles"))
  source ~/.vimrc.bundles
endif

let g:coffeeCheckHighlightErrorLine = 1
"""""
" Basic editing configuraiton
"""""
set modeline
set nocompatible
set hidden
set history=1000
set expandtab
set shiftwidth=2
set tabstop=2
set softtabstop=2
set autoindent
set laststatus=2
set showmatch
set incsearch
set hlsearch
set ignorecase smartcase
" highlight current line
set cursorline
" highlight 80th column
" if (exists('+colorcolumn'))
"     set colorcolumn=80
"     highlight ColorColumn ctermbg=9
" endif
" who can code with out them
set nu
set cmdheight=2
set switchbuf=useopen
"Setting winheight to be large after vim is started to avoid errors
au VimEnter * set winheight=999
set winheight=6
set winminheight=5
set winwidth=79
" This makes RVM work inside Vim, GRB does not know why
set shell=bash
" Prevent Vim from clobbering the scrollback buffer. See
" http://www.shallowsky.com/linux/noaltscreen.html
set t_ti= t_te=
" keep more context when scrolling of the end of a buffer
set scrolloff=3
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
" allow backspacing over everything in insert mode
set backspace=indent,eol,start
" display incomplete commands
set showcmd
" Enable syntax highlighting
syntax on
" Enable file type detection.
" Use the default filetype settings, so that mail gets 'tw' set to 72,
" 'cindent' is on in C files, etc.
" Also load indent files, to automatically do language-dependent indenting.
filetype plugin indent on
" use emacs-style tab completion when selecting files, etc
set wildmode=longest,list
" make tab completion for files/buffers act like bash
set wildmenu
let mapleader=","

"Enable project specific vimrc files
set exrc
set secure

set wildignore+=*.o,*.obj,.git,tmp/cache/assets,tmp,vendor/assets/bower_components,coverage,paper_clip

" kill the scroll bars
" when using full screen seeing them flash can be troubling.
set go-=r
set go-=L

"""""
" Colors
"""""
"set background=light
set background=dark
" Make Solarized use 16 colors for Terminal support
"let g:solarized_termcolors=16
 colorscheme solarized
"colorscheme slate

"""""
" STATUS LINE
"""""
set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)

"""""
" File types
"""""
au BufRead,BufNewFile *.ejs   setfiletype html

""""""""
" Command-T helpers
""""""""
" nmap <Leader>T :CommandTFlush<CR>
" nmap <Leader>t :CommandT<CR>
" let g:CommandTCancelMap=['<ESC>', '<C-c>']

nmap <Leader>t :CtrlP<CR>
let g:ctrlp_prompt_mappings = {
  \ 'PrtClearCache()':      ['<c-r>'],
  \ }
nmap <Leader>T :call PrtClearCache()<cr>
let g:ctrlp_use_caching=1
"""""
" Misc key maps
"""""
" Move around splits with <c-hjkl>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
" Insert a hash rocket with <c-l>
imap <c-l> <space>=><space>
"Can't be bothered to understand ESC vs <c-c> in insert mode
imap <c-c> <esc>
" Clear the search buffer when hitting return
function! MapCR()
  nnoremap <cr> :nohlsearch<cr>
endfunction
call MapCR()
command W w

"""""
" Tab key - indents or autocomplete text
" this is used instead of <c-n> as it is more inline with other
" editors
"""""
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>

"""""
" Rename Current File
"""""
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>n :call RenameFile()<cr>

"""""
" Clear end of line white space
"""""
nnoremap W :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>:w<CR>

"""""
" scss / css
"""""
au BufNewFile,BufReadPost *.scss setl shiftwidth=2 expandtab tabstop=2 softtabstop=2
au BufNewFile,BufReadPost *.css setl shiftwidth=2 expandtab tabstop=2 softtabstop=2

"""""
" CoffeeScript
"""""
au BufNewFile,BufReadPost *.coffee setl shiftwidth=2 expandtab tabstop=2 softtabstop=2

"""""
" Ruby
"""""
au BufNewFile,BufReadPost *.rb setl shiftwidth=2 expandtab tabstop=2 softtabstop=2
au BufNewFile,BufReadPost *.rabl setl shiftwidth=2 expandtab tabstop=2 softtabstop=2
au BufNewFile,BufReadPost *.axlsx setl shiftwidth=2 expandtab tabstop=2 softtabstop=2
autocmd BufNewFile,BufRead *.axlsx set ft=ruby

"""""
" JSON
"""""
let g:vim_json_syntax_conceal = 0
autocmd BufNewFile,BufRead *.json set ft=json
au BufNewFile,BufReadPost *.json setl shiftwidth=2 expandtab tabstop=2 softtabstop=2 tw=80
"
"""""
" Java
"""""
autocmd BufNewFile,BufRead *.java set ft=java
au BufNewFile,BufReadPost *.java setl shiftwidth=4 expandtab tabstop=4 softtabstop=4 tw=80
"""""
" Puppet
" Helps with tags identifying scoped identifiers
"""""
set iskeyword=-,:,@,48-57,_,192-255

"""""
" Markdown
" Tim Pope suggestions
"""""
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
