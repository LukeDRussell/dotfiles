-- Neovim config: plain settings, no plugin framework, so it works on a fresh
-- machine. Linked to ~/.config/nvim on Linux/macOS and %LOCALAPPDATA%\nvim on
-- Windows.

-- Leader first, so any mapping defined below uses it
vim.g.mapleader = " "

vim.opt.number = true
vim.opt.scrolloff = 5
vim.opt.sidescroll = 1

vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.expandtab = true

vim.opt.background = "dark"
vim.opt.undofile = true

vim.opt.wrap = true
vim.opt.linebreak = true

-- Move by screen line, so wrapped lines behave like separate lines
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "<Down>", "gj")
vim.keymap.set("n", "<Up>", "gk")

-- YAML: don't re-indent when typing # or :
vim.api.nvim_create_autocmd("FileType", {
  pattern = "yaml",
  callback = function()
    vim.opt_local.indentkeys:remove({ "0#", "<:>" })
  end,
})
