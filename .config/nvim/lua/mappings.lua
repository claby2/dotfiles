local map = vim.keymap.set

map("n", "<esc>", "<cmd>nohls<cr>", { desc = "Clear search highlights" })

map("n", "<leader>s", "<cmd>set spell!<cr>", { desc = "Toggle spell checking" })
map("n", "<leader>f", [[<cmd> lua require("conform").format()<cr>]], { desc = "Format current buffer" })

map("n", "<leader><tab>", [[<cmd>:FzfLua files<cr>]], { desc = "Find files" })
map("n", "<leader>rg", [[<cmd>:FzfLua grep_visual<cr>]], { desc = "Find files" })
map("n", "<leader>t", "<cmd>NvimTreeToggle<cr>", { desc = "Toggle NvimTree" })

-- Restart LSP
map("n", "<leader>lr", "<cmd>LspRestart<cr>", { desc = "Restart LSP" })

-- LSP mappings
map("n", "gD", vim.lsp.buf.declaration, { desc = "Go to declaration" })
map("n", "K", vim.lsp.buf.hover, { desc = "Show hover info" })
map("n", "gd", vim.lsp.buf.definition, { desc = "Go to definition" })
map("n", "gi", vim.lsp.buf.implementation, { desc = "Go to implementation" })
map("n", "gr", vim.lsp.buf.references, { desc = "Show references" })
map("n", "<leader>D", vim.lsp.buf.type_definition, { desc = "Go to type definition" })
map("n", "<leader>ca", vim.lsp.buf.code_action, { desc = "Show code actions" })
map("n", "<leader>rn", vim.lsp.buf.rename, { desc = "Rename symbol" })

-- Diagnostic mappings
map("n", "<leader>e", vim.diagnostic.open_float, { desc = "Show diagnostics" })
map("n", "[d", vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic" })
map("n", "]d", vim.diagnostic.goto_next, { desc = "Go to next diagnostic" })

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

-- Dismiss notifications
map("n", "<leader>d", [[<cmd>lua require("notify").dismiss()<cr>]])

-- Toggle virtual lines for diagnostics
map("n", "<leader>v", function()
	local new_config = not vim.diagnostic.config().virtual_lines
	vim.diagnostic.config({ virtual_lines = new_config })
end)
