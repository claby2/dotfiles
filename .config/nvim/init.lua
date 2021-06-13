require "plugins"
require "mappings"
require "treesitter"
require "lsp"

local scopes = {o = vim.o, b = vim.bo, w = vim.wo}

local function opt(scope, key, value) scopes[scope][key] = value end

opt("o", "number", true)
opt("o", "relativenumber", true)
opt("o", "tabstop", 4)
opt("o", "shiftwidth", 4)
opt("o", "expandtab", true)
opt("o", "termguicolors", true)
opt("o", "hidden", true)
opt("o", "smartcase", true)
opt("o", "backspace", "indent,eol,start")
opt("o", "endofline", false)
opt("o", "splitbelow", true)
opt("o", "splitright", true)
opt("o", "spelllang", "en_us")
opt("o", "laststatus", 2)
opt("o", "scrolloff", 0)
opt("o", "mouse", "a")
opt("o", "inccommand", "nosplit")

vim.cmd([[colorscheme ayu]])
