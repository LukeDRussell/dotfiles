-- Make sure paq is installed
-- git clone --depth=1 https://github.com/savq/paq-nvim.git ~/.local/share/nvim/site/pack/paqs/start/paq-nvim

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


-- hardline status and tab line
require'hardline'.setup {
	theme = 'default'
}

require'marks'.setup {
	mappings = {
		toggle = "m,";
		delete_line = "dm.";
		next = "m<down>";
		prev = "m<up>"
	}
}

-- orgmode.nvim
require'orgmode'.setup {
	org_agenda_files = '~/Notes/org/*';
	org_default_notes_file = '~/Notes/org/notes.org'
}



-- My Settings
vim.opt.scrolloff = 5
vim.opt.sidescroll= 1
vim.opt.number = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.o.bg = 'dark'
vim.opt.undofile = true

-- File type settings
-- yaml

vim.api.nvim_exec([[ autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab indentkeys-=0# indentkeys-=<:> ]], false)
