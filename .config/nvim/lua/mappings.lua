local map = vim.keymap.set

map("n", "<esc>", "<cmd>nohls<cr>", { desc = "Clear search highlights" })

map("n", "<leader>s", "<cmd>set spell!<cr>", { desc = "Toggle spell checking" })
map("n", "<leader>f", [[<cmd> lua require("conform").format()<cr>]], { desc = "Format current buffer" })
map(
	"n",
	"<leader><tab>",
	[[<cmd>lua require("telescope.builtin").find_files{follow = true}<cr>]],
	{ desc = "Find files with Telescope" }
)
map("n", "<leader>t", "<cmd>NvimTreeToggle<cr>", { desc = "Toggle NvimTree" })

-- Restart LSP
map("n", "<leader>lr", "<cmd>LspRestart<cr>", { desc = "Restart LSP" })

-- Switch between window splits easily
map("n", "<C-H>", "<C-W>h")
map("n", "<C-J>", "<C-W>j")
map("n", "<C-K>", "<C-W>k")
map("n", "<C-L>", "<C-W>l")

map(
	"i",
	"<C-J>",
	'copilot#Accept("<CR>")',
	{ noremap = true, silent = true, expr = true, replace_keycodes = false, desc = "Accept copilot completion" }
)
