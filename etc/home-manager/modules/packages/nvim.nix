{ pkgs
, lib
, config
, ...
}: {
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withRuby = true;
    withPython3 = true;
    coc = { enable = false; };

    plugins = with pkgs.vimPlugins; [
      Navigator-nvim
      gitlinker-nvim
      indent-blankline-nvim
      is-vim
      lsp_signature-nvim
      neogit
      nerdcommenter
      nord-nvim
      nvim-lspconfig
      (nvim-treesitter.withPlugins (
        plugins: with plugins; [
          tree-sitter-dockerfile
          tree-sitter-elixir
          tree-sitter-html
          tree-sitter-hcl
          tree-sitter-json
          tree-sitter-lua
          tree-sitter-nix
          tree-sitter-python
          tree-sitter-rust
          tree-sitter-toml
          tree-sitter-yaml
        ]
      ))
      nvim-treesitter-parsers.terraform
      plenary-nvim
      popup-nvim
      telescope-nvim
      vim-abolish
      vim-argumentative
      vim-nix
      vim-surround
      vim-terraform
      vim-tmux-navigator
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
        vim.opt.syntax         = off
        vim.opt.tabstop        = 2
        vim.opt.undodir        = vim.fn.expand('~/.vim-undo')
        vim.opt.undofile       = true
        vim.opt.undolevels     = 1000
        vim.opt.undoreload     = 10000
        vim.opt.wildignore     = '*.o,*.obj,*~'
        vim.opt.wildmode       = 'longest:list,full'
        vim.opt.wrap           = false

        -- vim.cmd[[colorscheme nord]]
        vim.g.nord_bold = false
        vim.g.nord_italic = false
        require('nord').set()

        -- keybinds
        vim.cmd[[command! -bar -bang Q quit<bang>]]

        -- windmove-like shift-arrow movements
        local noremap_silent = { noremap = true, silent = true }
        vim.api.nvim_set_keymap("i", "<S-Left>", "<Esc>:<C-U>TmuxNavigateLeft<cr>", noremap_silent)
        vim.api.nvim_set_keymap("i", "<S-Right>", "<Esc>:<C-U>TmuxNavigateRight<cr>", noremap_silent)
        vim.api.nvim_set_keymap("i", "<S-Up>", "<Esc>:<C-U>TmuxNavigateUp<cr>", noremap_silent)
        vim.api.nvim_set_keymap("i", "<S-Down>", "<Esc>:<C-U>TmuxNavigateDown<cr>", noremap_silent)
        vim.api.nvim_set_keymap("n", "<S-Left>", ":<C-U>TmuxNavigateLeft<cr>", noremap_silent)
        vim.api.nvim_set_keymap("n", "<S-Right>", ":<C-U>TmuxNavigateRight<cr>", noremap_silent)
        vim.api.nvim_set_keymap("n", "<S-Up>", ":<C-U>TmuxNavigateUp<cr>", noremap_silent)
        vim.api.nvim_set_keymap("n", "<S-Down>", ":<C-U>TmuxNavigateDown<cr>", noremap_silent)

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

        -- swap between vertical/horizontal splits
        -- C-w - : vertical to horizontal ( | -> -- )
        vim.api.nvim_set_keymap("i", "<C-w>-", "<Esc><C-w>t<C-w>K", { noremap = true })
        vim.api.nvim_set_keymap("n", "<C-w>-", "<C-w>t<C-w>K", { noremap = true })

        -- C-w | or C-w \ : horizontal to vertical ( -- -> | )
        vim.api.nvim_set_keymap("i", "<C-w>|", "<Esc><C-w>t<C-w>H", { noremap = true })
        vim.api.nvim_set_keymap("n", "<C-w>|", "<C-w>t<C-w>H", { noremap = true })
        vim.api.nvim_set_keymap("i", "<C-w>\\", "<Esc><C-w>t<C-w>H", { noremap = true })
        vim.api.nvim_set_keymap("n", "<C-w>\\", "<C-w>t<C-w>H", { noremap = true })

        -- auto-resize when switching tmux panes
        local wr_group = vim.api.nvim_create_augroup('WinResize', { clear = true })
        vim.api.nvim_create_autocmd(
            'VimResized',
            {
                group = wr_group,
                pattern = '*',
                command = 'wincmd =',
                desc = 'Automatically resize windows when the host window size changes.'
            }
        )

        -- telescope
        vim.api.nvim_set_keymap("n", "<C-p>", "<cmd>Telescope find_files<cr>", { noremap = true })
        vim.api.nvim_set_keymap("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { noremap = true })
        vim.api.nvim_set_keymap("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", { noremap = true })
        vim.api.nvim_set_keymap("n", "<leader>fb", "<cmd>Telescope buffers<cr>", { noremap = true })
        vim.api.nvim_set_keymap("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", { noremap = true })

        -- NERDCommenter
        vim.g.NERDDefaultAlign = 'left'
        vim.g.NERDSpaceDelims = 1

        -- neogit
        local neogit = require('neogit')
        neogit.setup {}
        vim.api.nvim_set_keymap("n", "<leader>gg", "<cmd>Neogit<cr>", { noremap = true})

        -- treesitter

        require('nvim-treesitter.configs').setup {
          highlight = {
            enable = true,              -- false will disable the whole extension
            -- disable = { "c", "rust" },  -- list of language that will be disabled
            -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
            -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
            -- Using this option may slow down your editor, and you may see some duplicate highlights.
            -- Instead of true it can also be a list of languages
            additional_vim_regex_highlighting = false,
          },
        }

        -- vim-terraform
        vim.cmd([[let g:terraform_fmt_on_save=1]])
        vim.cmd([[let g:terraform_align=1]])

        -- python
        vim.cmd [[autocmd BufWritePost *.py silent! !~/.local/bin/black --quiet <afile>]]

        -- lsp
        -- `on_attach` callback will be called after a language server
        -- instance has been attached to an open buffer with matching filetype
        -- here we're setting key mappings for hover documentation, goto definitions, goto references, etc
        -- you may set those key mappings based on your own preference
        local on_attach = function(client, bufnr)
          local opts = { noremap=true, silent=true }

          vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
          vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
          vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
          vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
          vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
          vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
          vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>cr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
          vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
          vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>cf', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
          vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>cd', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
          vim.api.nvim_buf_set_keymap(bufnr, 'n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
          vim.api.nvim_buf_set_keymap(bufnr, 'n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
        end

        -- auto format on save
        vim.cmd [[autocmd BufWritePre *.nix lua vim.lsp.buf.format()]]

        local lspconfig = require('lspconfig')

        -- setting up the elixir language server
        -- you have to manually specify the entrypoint cmd for elixir-ls
        lspconfig.elixirls.setup {
          cmd = { "${pkgs.elixir-ls}/lib/language_server.sh" },
          on_attach = on_attach
        }

        lspconfig.pyright.setup {
          cmd = { "${pkgs.nodePackages.pyright}/bin/pyright-langserver", "--stdio" },
          on_attach = on_attach
        }

        lspconfig.terraformls.setup {
          cmd = { "${pkgs.terraform-ls}/bin/terraform-ls", "serve" },
          on_attach = on_attach,
        }

        lspconfig.terraform_lsp.setup {
          cmd = { "${pkgs.terraform-lsp}/bin/terraform-lsp" },
          on_attach = on_attach
        }

        lspconfig.tflint.setup {
          cmd = { '${pkgs.tflint}/bin/tflint', '--langserver' }
        }

        lspconfig.nil_ls.setup {
          cmd = { '${pkgs.nil}/bin/nil' },
          settings = {
            ['nil'] = {
              formatting = {
                command = { "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt" },
              },
            }
          }
        }

        -- gitlinker
        require('gitlinker').setup()
      '';
  };
}
