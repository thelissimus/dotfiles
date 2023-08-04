lua vim.loader.enable()

filetype plugin indent on
syntax on
language en_US.utf8

set nocompatible
set encoding=utf-8
set fileencoding=utf-8
set background=dark
set completeopt=menuone,noinsert,noselect,preview
set shortmess+=c
set diffopt+=vertical
set signcolumn=number
set colorcolumn=80
set splitright
set splitbelow
set number
set relativenumber
set noshowmode
set cursorline
set termguicolors
set nowrap
set list
set listchars+=tab:··›,space:·,trail:•
set noexpandtab
set tabstop=4
set shiftwidth=4
set smartindent
set ignorecase
set smartcase
set incsearch
set nohlsearch
set autoread
set hidden
set nobackup
set nowritebackup
set noswapfile
set undofile
set noerrorbells
set updatetime=50
set scrolloff=8
set sidescroll=4
set laststatus=3
set pumheight=10
set spelllang=en_us
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
set nofoldenable

let mapleader = " "
let maplocalleader = " "

nmap <F5> :set list!<CR>
nmap <F7> :setlocal spell!<CR>
nmap <F8> :TagbarToggle<CR>
nmap <leader>h :bprevious<CR>
nmap <leader>l :bnext<CR>

nnoremap <M-j> :resize -2<CR>
nnoremap <M-k> :resize +2<CR>
nnoremap <M-h> :vertical resize -5<CR>
nnoremap <M-l> :vertical resize +5<CR>
nnoremap <leader>n :NvimTreeToggle<CR>
nnoremap <leader>u :UndotreeToggle<CR>
nnoremap <leader><space> :Files<CR>
nnoremap <leader>ff :Rg<CR>
nnoremap <leader>bb :Buffers<CR>

tnoremap <Esc> <C-\><C-n>

inoremap <Esc> <nop>
inoremap jk <Esc>
inoremap kj <Esc>

let g:EditorConfig_exclude_patterns = ['fugitive://.*']

let g:gruvbox_material_background = 'medium'
let g:gruvbox_material_foreground = 'mix'
let g:gruvbox_material_disable_italic_comment = 1
let g:gruvbox_material_enable_bold = 1
let g:gruvbox_material_enable_italic = 0
let g:gruvbox_material_spell_foreground = 'colored'
let g:gruvbox_material_lightline_disable_bold = 1
colorscheme gruvbox-material

let g:lightline = {
  \ 'enable': { 'statusline': 1, 'tabline': 0 },
  \ 'colorscheme': 'gruvbox_material',
  \ 'active': {
  \ 	'left': [
  \ 		['mode', 'paste'],
  \ 		['gitbranch', 'readonly', 'filename'],
  \		['lsp_info', 'lsp_hints', 'lsp_errors', 'lsp_warnings', 'lsp_ok'],
  \		['lsp_status']
  \ 	],
  \ },
  \ 'component_function': {
  \ 	'gitbranch': 'FugitiveHead',
  \ 	'filename': 'LightlineFilename'
  \ },
  \ }
call lightline#lsp#register()

function! LightlineFilename()
  let filename = expand('%:t') !=# '' ? expand('%:t') : '[Buffer]'
  let modified = &modified ? ' +' : ''
  return filename . modified
endfunction

autocmd! FileType fzf tnoremap <buffer> <esc> <c-c>
autocmd BufNewFile,BufRead *.service set ft=systemd
autocmd BufNewFile,BufRead * setlocal formatoptions-=o formatoptions+=r
