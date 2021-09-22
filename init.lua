-- Use paq to install plugins
require "paq" {
	"savq/paq-nvim";
	"nvim-treesitter/nvim-treesitter";
	"neovim/nvim-lspconfig";
	"nvim-telescope/telescope.nvim";
	"kristijanhusak/orgmode.nvim";
	"ojroques/nvim-hardline";
	"junegunn/goyo.vim";
	"junegunn/limelight.vim";
	"lukas-reineke/indent-blankline.nvim"
}

-- tree-sitter 
local ts = require 'nvim-treesitter.configs'
ts.setup {
	ensure_installed = 'maintained',
	highlight = {enable = true}
}

-- hardline status and tab line
require('hardline').setup {
	theme = 'default'
}

-- Org-mode vim

-- My Settings
vim.o.scrolloff = 5
vim.o.number = true
