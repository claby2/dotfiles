local packer = require("packer")
local use = packer.use

require("packer").startup(function()
    use "wbthomason/packer.nvim"
    use "nvim-lua/plenary.nvim"
    use "nvim-lua/popup.nvim"

    -- Language
    use "nvim-treesitter/nvim-treesitter"
    use "hrsh7th/vim-vsnip"
    use "hrsh7th/nvim-compe"
    use "claby2/genfmt.vim"
    use "onsails/lspkind-nvim"
    use "neovim/nvim-lspconfig"
    use "kabouzeid/nvim-lspinstall"

    -- Tree
    use "kyazdani42/nvim-tree.lua"
    use "kyazdani42/nvim-web-devicons"
    use "ryanoasis/vim-devicons"

    -- Telescope
    use "nvim-telescope/telescope.nvim"

    -- Theme
    use "ayu-theme/ayu-vim"

    -- Misc
    use "lewis6991/gitsigns.nvim"
    use "itchyny/lightline.vim"
    use "preservim/nerdcommenter"
end)

-- nvim-compe
require("compe").setup {
    enabled = true,
    autocomplete = true,
    debug = false,
    min_length = 1,
    preselect = 'enable',
    throttle_time = 80,
    source_timeout = 200,
    incomplete_delay = 400,
    max_abbr_width = 100,
    max_kind_width = 100,
    max_menu_width = 100,
    documentation = true,
    source = {path = true, buffer = true, calc = true, nvim_lsp = true, nvim_lua = true, vsnip = true}
}
local t = function(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end
local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end
_G.tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t "<C-n>"
    elseif vim.fn.call("vsnip#available", {1}) == 1 then
        return t "<Plug>(vsnip-expand-or-jump)"
    elseif check_back_space() then
        return t "<Tab>"
    else
        return vim.fn['compe#complete']()
    end
end
_G.s_tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t "<C-p>"
    elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
        return t "<Plug>(vsnip-jump-prev)"
    else
        -- If <S-Tab> is not working in your terminal, change it to <C-h>
        return t "<S-Tab>"
    end
end

-- genfmt.vim
ClangFormat = function()
    local function has_file(file) return vim.fn.len(vim.fn.findfile(file, vim.fn.expand("%:p:h") .. ";")) end
    if has_file(".clang-format") or has_file("_clang-format") then
        return "clang-format --assume-filename=" .. vim.fn.expand("%:t") .. " -style=file"
    end
    return "clang-format --assume-filename=" .. vim.fn.expand("%:t") ..
               " --style=\"{BasedOnStyle: Google, IndentWidth: 4}\""
end
CmakeFormat = function() return "cmake-format" .. vim.fn.expand("%:t") end
Prettier = function() return "prettier --stdin-filepath" .. vim.fn.expand("%:p") end
vim.api.nvim_set_var("genfmt_formatters", {
    python = "yapf",
    cpp = ClangFormat(),
    java = ClangFormat(),
    javascript = Prettier(),
    typescriptreact = Prettier(),
    typescript = Prettier(),
    cmake = CmakeFormat(),
    rust = "rustfmt --edition 2018",
    haskell = "stylish-haskell",
    markdown = "remark --no-color --silent",
    gdscript3 = "gdformat -",
    go = "gofmt",
    lua = "lua-format --column-limit=120",
    sh = "shfmt",
    zsh = "shfmt"
})
vim.api.nvim_set_var("genfmt_enable_fallback", true)

-- telescope.nvim
local actions = require("telescope.actions")
require("telescope").setup {
    defaults = {mappings = {i = {["<leader><tab>"] = actions.close}, n = {["<leader><tab>"] = actions.close}}}
}

-- gitsigns.nvim
require("gitsigns").setup()

-- lightline.vim
vim.api.nvim_set_var("lightline", {
    colorscheme = "ayu",
    active = {
        left = {{"mode"}, {"readonly", "filename", "modified", "spell"}},
        right = {{"lineinfo"}, {"fileformat", "filetype"}}
    },
    separator = {left = "", right = ""},
    subseparator = {left = "", right = ""}
})
