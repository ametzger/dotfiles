{
  pkgs,
  lib,
  config,
  ...
}: {
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withRuby = true;
    withPython3 = true;
    coc = {enable = false;};

    plugins = with pkgs.vimPlugins; [
      Navigator-nvim
      catppuccin-nvim
      indent-blankline-nvim
      is-vim
      lsp_signature-nvim
      nerdcommenter
      nvim-lspconfig
      nvim-treesitter
      plenary-nvim
      popup-nvim
      telescope-nvim
      vim-abolish
      vim-argumentative
      vim-nix
      vim-surround
      vim-terraform
    ];

    extraLuaConfig =
      ''

        vim.g.mapleader = ' '

        vim.opt.backup         = false
        vim.opt.clipboard      = 'unnamedplus'
        vim.opt.compatible     = false
        vim.opt.encoding       = 'utf-8'
        vim.opt.expandtab      = true
        vim.opt.foldenable     = false
        vim.opt.hlsearch       = true
        vim.opt.ignorecase     = true
        vim.opt.inccommand     = 'nosplit'
        vim.opt.incsearch      = true
        vim.opt.laststatus     = 2
        vim.opt.lazyredraw     = true
        vim.opt.mouse          = 'a'
        vim.opt.number         = true
        vim.opt.relativenumber = true
        vim.opt.ruler          = true
        vim.opt.scrolloff      = 5
        vim.opt.shiftwidth     = 2
        vim.opt.shortmess      = 'A'
        vim.opt.showcmd        = true
        vim.opt.showmatch      = true
        vim.opt.showmode       = true
        vim.opt.sidescroll     = 1
        vim.opt.sidescrolloff  = 7
        vim.opt.smartcase      = true
        vim.opt.smartindent    = true
        vim.opt.splitbelow     = true
        vim.opt.splitright     = true
        vim.opt.tabstop        = 2
        vim.opt.undodir        = '~/.vim-undo'
        vim.opt.undofile       = true
        vim.opt.undolevels     = 1000
        vim.opt.undoreload     = 10000
        vim.opt.wildignore     = '*.o,*.obj,*~'
        vim.opt.wildmode       = 'longest:list,full'
        vim.opt.wrap           = false

        vim.cmd[[colorscheme catppuccin-macchiato]]

        -- keybinds
        vim.cmd[[command! -bar -bang Q quit<bang>]]

        -- windmove-like shift-arrow movements
        vim.api.nvim_set_keymap("i", "<S-Left>", "<Esc><C-w>h", { noremap = true })
        vim.api.nvim_set_keymap("i", "<S-Up>", "<Esc><C-w>j", { noremap = true })
        vim.api.nvim_set_keymap("i", "<S-Down>", "<Esc><C-w>k", { noremap = true })
        vim.api.nvim_set_keymap("i", "<S-Right>", "<Esc><C-w>l", { noremap = true })
        vim.api.nvim_set_keymap("n", "<S-Left>", "<C-w>h", { noremap = true })
        vim.api.nvim_set_keymap("n", "<S-Up>", "<C-w>j", { noremap = true })
        vim.api.nvim_set_keymap("n", "<S-Down>", "<C-w>k", { noremap = true })
        vim.api.nvim_set_keymap("n", "<S-Right>", "<C-w>l", { noremap = true })

        -- emacs binds in insert mode
        vim.api.nvim_set_keymap("i", "<C-b>", "<Left>", { noremap = false })
        vim.api.nvim_set_keymap("i", "<C-f>", "<Right>", { noremap = false })
        vim.api.nvim_set_keymap("i", "<C-a>", "<Home>", { noremap = false })
        vim.api.nvim_set_keymap("i", "<C-e>", "<End>", { noremap = false })
        vim.api.nvim_set_keymap("i", "<C-d>", "<Del>", { noremap = false })
        vim.api.nvim_set_keymap("i", "<C-h>", "<BS>", { noremap = false })
        vim.api.nvim_set_keymap("i", "<C-w>", "<Esc>ddi", { noremap = false })
        vim.api.nvim_set_keymap("i", "<C-_>", "<Esc>ui", { noremap = false })
        vim.api.nvim_set_keymap("i", "<M-d>", "<Esc>dwi", { noremap = false })
        vim.api.nvim_set_keymap("i", "<M-b>", "<Esc>bi", { noremap = false })
        vim.api.nvim_set_keymap("i", "<M-f>", "<Esc>wi", { noremap = false })
        vim.api.nvim_set_keymap("i", "<C-k>", "l<Esc>d$a", { noremap = true })

        -- emacs binds in command mode
        vim.api.nvim_set_keymap("c", "<C-p>", "<Up>", { noremap = false })
        vim.api.nvim_set_keymap("c", "<C-n>", "<Down>", { noremap = false })
        vim.api.nvim_set_keymap("c", "<C-b>", "<Left>", { noremap = false })
        vim.api.nvim_set_keymap("c", "<C-f>", "<Right>", { noremap = false })
        vim.api.nvim_set_keymap("c", "<C-a>", "<Home>", { noremap = false })
        vim.api.nvim_set_keymap("c", "<C-e>", "<End>", { noremap = false })
        vim.api.nvim_set_keymap("c", "<C-d>", "<Del>", { noremap = true })
        vim.api.nvim_set_keymap("c", "<C-h>", "<BS>", { noremap = true })
        vim.api.nvim_set_keymap("c", "<C-k>", "<C-f>D<C-c><C-c>:<Up>", { noremap = true })

        -- indentation
        vim.cmd[[autocmd FileType ruby setlocal shiftwidth=2 tabstop=2]]
        vim.cmd[[autocmd FileType python setlocal shiftwidth=4 tabstop=4]]
        vim.cmd[[autocmd FileType html setlocal shiftwidth=4 tabstop=4]]

        -- telescope
        vim.api.nvim_set_keymap("n", "<C-p>", "<cmd>Telescope find_files<cr>", { noremap = true })
        vim.api.nvim_set_keymap("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { noremap = true })
        vim.api.nvim_set_keymap("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", { noremap = true })
        vim.api.nvim_set_keymap("n", "<leader>fb", "<cmd>Telescope buffers<cr>", { noremap = true })
        vim.api.nvim_set_keymap("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", { noremap = true })

        -- NERDCommenter
        vim.g.NERDDefaultAlign = 'left'
        vim.g.NERDSpaceDelims = 1
      '';
  };
}
