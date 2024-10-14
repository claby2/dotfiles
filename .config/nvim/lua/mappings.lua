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
	"<C-l>",
	'copilot#Accept("<CR>")',
	{ noremap = true, silent = true, expr = true, replace_keycodes = false, desc = "Accept copilot completion" }
)

-- I don't think Copilot has a way to toggle itself, so this is a workaround
vim.cmd("Copilot disable")
local copilot_enabled = false
vim.api.nvim_create_user_command("ToggleCopilot", function()
	if copilot_enabled then
		vim.cmd("Copilot disable")
		copilot_enabled = false
		print("Cplt disabled")
	else
		vim.cmd("Copilot enable")
		copilot_enabled = true
		print("Cplt enabled")
	end
end, {})
map("n", "<leader>cp", "<cmd>ToggleCopilot<cr>", { desc = "Toggle Copilot" })
