-- Make sure paq is installed
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/paqs/start/paq-nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', '--depth=1', 'https://github.com/savq/paq-nvim.git', install_path})
end

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

