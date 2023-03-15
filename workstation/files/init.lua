-- Make sure paq is installed (Linux & MacOS)
-- git clone --depth=1 https://github.com/savq/paq-nvim.git ~/.local/share/nvim/site/pack/paqs/start/paq-nvim

-- Make sure paq is installed (Windows)
-- git clone https://github.com/savq/paq-nvim.git "$env:LOCALAPPDATA\nvim-data\site\pack\paqs\start\paq-nvim"

-- Use paq to install plugins
require "paq" {
	"savq/paq-nvim";
	"nvim-treesitter/nvim-treesitter";
	"neovim/nvim-lspconfig";
	"nvim-lua/plenary.nvim";
	"nvim-telescope/telescope.nvim";
	"nvim-orgmode/orgmode.nvim";
	"ojroques/nvim-hardline";
	"junegunn/goyo.vim";
	"junegunn/limelight.vim";
	"lukas-reineke/indent-blankline.nvim";
	"chentau/marks.nvim";
	"ishan9299/nvim-solarized-lua"
}

-- Language Servers
-- Python
require'lspconfig'.pylsp.setup{}


-- Treesitter
require('orgmode').setup_ts_grammar()

require'nvim-treesitter.configs'.setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = {'org'}, -- Required since TS highlighter doesn't support all syntax features (conceal)
  },
  -- One of "all", "maintained" (parsers with maintainers), or a list of languages
  ensure_installed = "maintained",
}

-- orgmode.nvim
require'orgmode'.setup {
	org_agenda_files = '~/Org/*';
	org_default_notes_file = '~/Org/notes.org'
}

-- hardline status and tab line
require'hardline'.setup {
	theme = 'default';
}

require'marks'.setup {
	mappings = {
		toggle = "m,";
		delete_line = "dm.";
		next = "m<down>";
		prev = "m<up>"
	}
}



-- My Settings
vim.opt.scrolloff = 5
vim.opt.sidescroll= 1
vim.opt.number = true

vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.expandtab = true

vim.o.bg = 'dark'
vim.opt.undofile = true

vim.opt.wrap = true
vim.opt.linebreak = true

-- Key maps
vim.g.mapleader = ' '

local map = vim.api.nvim_set_keymap

map('n', 'j', 'gj', {noremap=true})
map('n', 'k', 'gk', {noremap=true})
map('n', '<Down>', 'gj', {noremap=true})
map('n', '<Up>', 'gk', {noremap=true})



-- File type settings
-- yaml

vim.api.nvim_exec([[ autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab indentkeys-=0# indentkeys-=<:> ]], false)
