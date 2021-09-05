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
    use "mhartington/formatter.nvim"
    use "onsails/lspkind-nvim"
    use "neovim/nvim-lspconfig"

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
    use "hoob3rt/lualine.nvim"
    use "preservim/nerdcommenter"
    use "lervag/vimtex"
    use "norcalli/nvim-colorizer.lua"
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

-- formatter.nvim
local clang_format = function()
    return {exe = "clang-format", args = {"--style=file", "--fallback-style=Google"}, stdin = true}
end
local prettier_format = function()
    return {
        exe = "prettier",
        args = {"--stdin-filepath", vim.fn.fnameescape(vim.api.nvim_buf_get_name(0))},
        stdin = true
    }
end
local shfmt = function() return {exe = "shfmt", stdin = true} end
require("formatter").setup({
    logging = true,
    filetype = {
        python = {function() return {exe = "yapf", stdin = true} end},
        c = {clang_format},
        cpp = {clang_format},
        java = {clang_format},
        javascript = {prettier_format},
        typescriptreact = {prettier_format},
        typescript = {prettier_format},
        cmake = {function() return {exe = "cmake-format", stdin = true} end},
        rust = {function() return {exe = "rustfmt", args = {"--edition", 2018, "--emit=stdout"}, stdin = true} end},
        haskell = {function() return {exe = "stylish-haskell", stdin = true} end},
        markdown = {function() return {exe = "remark", args = {"--no-color", "--silent"}, stdin = true} end},
        go = {function() return {exe = "gofmt", stdin = true} end},
        lua = {function() return {exe = "lua-format", args = {"--column-limit=120"}, stdin = true} end},
        sh = {shfmt},
        zsh = {shfmt},
        tex = {
            function()
                return {exe = "latexindent", args = {"-sl", "-g /dev/stderr", "2>/dev/null"}, stdin = true}
            end
        }
    }
})

-- telescope.nvim
require("telescope").setup {}

-- gitsigns.nvim
require("gitsigns").setup {
    keymaps = {
        ["n <leader>hr"] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
        ["n <leader>hp"] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
        ["n <leader>hb"] = '<cmd>lua require"gitsigns".blame_line()<CR>'
    }
}

-- lualine.nvim
require("lualine").setup {
    options = {theme = "ayu_dark"},
    sections = {lualine_b = {"filename"}, lualine_c = {"branch"}},
    extensions = {"nvim-tree"}
}

-- vimtex
vim.api.nvim_set_var("tex_flavor", "latex")
vim.api.nvim_set_var("vimtex_view_method", "zathura")
vim.api.nvim_set_var("vimtex_quickfix_mode", 0)

-- nvim-colorizer.lua
require("colorizer").setup {"*", "xdefaults"}
