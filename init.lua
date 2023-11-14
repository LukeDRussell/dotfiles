if vim.loader and vim.fn.has "nvim-0.9.1" == 1 then vim.loader.enable() end

for _, source in ipairs {
  "astronvim.bootstrap",
  "astronvim.options",
  "astronvim.lazy",
  "astronvim.autocmds",
  "astronvim.mappings",
} do
  local status_ok, fault = pcall(require, source)
  if not status_ok then vim.api.nvim_err_writeln("Failed to load " .. source .. "\n\n" .. fault) end
end

if astronvim.default_colorscheme then
  if not pcall(vim.cmd.colorscheme, astronvim.default_colorscheme) then
    require("astronvim.utils").notify(
      ("Error setting up colorscheme: `%s`"):format(astronvim.default_colorscheme),
      vim.log.levels.ERROR
    )
  end
end

require("astronvim.utils").conditional_func(astronvim.user_opts("polish", nil, false), true)

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
