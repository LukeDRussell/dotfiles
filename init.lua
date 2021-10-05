-- Use paq to install plugins
require "paq" {
	"savq/paq-nvim";
	"nvim-treesitter/nvim-treesitter";
	"neovim/nvim-lspconfig";
	"nvim-lua/plenary.nvim";
	"nvim-telescope/telescope.nvim";
	"kristijanhusak/orgmode.nvim";
	"ojroques/nvim-hardline";
	"junegunn/goyo.vim";
	"junegunn/limelight.vim";
	"lukas-reineke/indent-blankline.nvim"
}

-- hardline status and tab line
require('hardline').setup {
	theme = 'default'
}

-- Org-mode vim

-- My Settings
vim.opt.scrolloff = 5
vim.opt.sidescroll= 5
vim.opt.number = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2

-- yaml
-- expandtab indentkeys-=0# indentkeys-=<:>

