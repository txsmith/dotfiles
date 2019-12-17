call plug#begin('~/.local/share/nvim/plugged')
  Plug 'neovimhaskell/haskell-vim'
  Plug 'scrooloose/nerdtree'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'AlessandroYorba/Sierra'
  Plug 'scrooloose/nerdcommenter'
  Plug 'jiangmiao/auto-pairs'
  Plug 'lervag/vimtex', { 'commit': 'e323c55e9669c1adb205295b06ec4463bae5b637' }
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'SirVer/ultisnips'
  Plug 'honza/vim-snippets'
  Plug 'wincent/command-t'
  Plug '907th/vim-auto-save'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf' }
  Plug 'junegunn/fzf.vim'
  Plug 'drewtempelmeyer/palenight.vim'
  Plug 'hzchirs/vim-material'
  Plug 'tpope/vim-surround'
call plug#end()

silent !stty -ixon

noremap <silent> <Leader>q :qa<CR>
noremap <silent> <C-H> <C-W>h
noremap <silent> <C-J> <C-W>j
noremap <silent> <C-K> <C-W>k
noremap <silent> <C-L> <C-W>l
noremap <silent> <C-E> :NERDTreeToggle<CR>

" Map Ctrl+/ to toggle comments
nmap <C-_> <Plug>NERDCommenterToggle<CR>
vmap <C-_> <Plug>NERDCommenterToggle<CR>gv

" Close buffer but keep window on Leader-d
nmap <silent> <leader>d :bp\|bd #<CR>

" Copy and paste from system clip
noremap <Leader>y "+y
noremap <Leader>p "+p

" Clear search highlight
noremap <silent> <leader>h :noh<CR>
" Open FZF
noremap <Leader>f :Files<CR>

let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit' }

noremap <silent> <Leader><Space> :Buffers<CR>
noremap <silent> <Leader>r :History<CR>

" Cycle through auto-complete with Tab
inoremap <expr><C-Space> pumvisible() ? "\<C-n>" : "\<C-Space>"
" Apply snippet completion with Ctrl+Space
" let g:UltiSnipsExpandTrigger="<C-Space>"

set number
set ts=2
set sts=2
set sw=2
set expandtab

set breakindent
set linebreak
set showbreak=↳
set breakindentopt=shift:2,min:40,sbr

syntax on
let g:material_style='dark'
" let g:palenight_terminal_italics=1
let g:airline_theme='material'
let g:airline_powerline_fonts = 1
set termguicolors
set background=dark
colorscheme vim-material

" Customize fzf colors to match your color scheme
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1
let $FZF_DEFAULT_COMMAND = 'ag -g ""'

set spelllang=en

let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let g:NERDDefaultAlign = 'left'

fun! <SID>StripTrailingWhitespaces()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l, c)
endfun
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

let g:auto_save = 1  " enable AutoSave on Vim startup
let g:deoplete#enable_at_startup = 1

call deoplete#custom#var('omni', 'input_patterns', {
  \ 'tex': g:vimtex#re#deoplete
  \})

let g:vimtex_compiler_latexmk = {
    \ 'options' : [
    \   '-pdf',
    \   '-shell-escape',
    \   '-verbose',
    \   '-file-line-error',
    \   '-synctex=1',
    \   '-interaction=nonstopmode',
    \ ],
    \}
let g:vimtex_compiler_progname= "nvr"
let g:tex_flavor = 'latex'
let g:vimtex_view_method = 'zathura'
let g:vimtex_quickfix_mode = 2
let g:vimtex_quickfix_open_on_warning = 0
let g:vimtex_quickfix_ignored_warnings = [
  \ 'Underfull',
  \ 'Overfull',
  \ 'specifier changed to',
  \ ]

let g:java_highlight_functions = 1
