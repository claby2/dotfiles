local function map(mode, lhs, rhs, opts)
    local options = {noremap = true}
    if opts then options = vim.tbl_extend("force", options, opts) end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Clear hlsearch highlights
map("n", "<esc><esc>", "<cmd>nohls<CR>")

-- Toggle spell checking
map("n", "<leader>s", "<cmd>set spell!<CR>")

-- Redraw screen
map("n", "<leader>r", "<cmd>redraw!<CR>")

-- nvim-tree.lua bind
map("n", "<leader>t", "<cmd>NvimTreeToggle<CR>")

-- telescope.nvim bind
map("n", "<leader><tab>", [[<cmd>lua require("telescope.builtin").find_files{follow = true}<CR>]])

-- genfmt.vim bind
map("n", "<leader>f", "<cmd>GenfmtFormat<CR>")

-- Switch between window splits easily
map("n", "<C-H>", "<C-W>h")
map("n", "<C-J>", "<C-W>j")
map("n", "<C-K>", "<C-W>k")
map("n", "<C-L>", "<C-W>l")

-- compe binds
map("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
map("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
map("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
map("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
