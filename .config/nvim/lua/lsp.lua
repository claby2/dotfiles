local on_attach = function(_, bufnr)
	local function buf_set_keymap(...)
		vim.api.nvim_buf_set_keymap(bufnr, ...)
	end
	local function buf_set_option(...)
		vim.api.nvim_buf_set_option(bufnr, ...)
	end

	-- Enable completion
	buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

	local opts = { noremap = true, silent = true }

	buf_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
	buf_set_keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
	buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
	buf_set_keymap("n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
	buf_set_keymap("n", "<leader>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
	buf_set_keymap("n", "<leader>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
	buf_set_keymap("n", "<leader>wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
	buf_set_keymap("n", "<leader>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
	buf_set_keymap("n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
	buf_set_keymap("n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
	buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
	buf_set_keymap("n", "<leader>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
	buf_set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
	buf_set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
	buf_set_keymap("n", "<leader>q", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)
end

local setup_servers = function()
	local servers = {
		"ccls",
		"hls",
		"rust_analyzer",
		"sumneko_lua",
		"texlab",
		"tsserver",
		"pyright",
	}
	local lspconfig = require("lspconfig")
	local capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())
	for _, lsp in ipairs(servers) do
		if lsp == "ccls" then
			lspconfig[lsp].setup({
				on_attach = on_attach,
				capabilities = capabilities,
				-- Prevent ccls from creating .ccls-cache directory in current/working directory.
				init_options = {
					cache = { directory = vim.fn.expand("$HOME/.cache/ccls/") },
				},
			})
		elseif lsp == "rust_analyzer" then
			lspconfig[lsp].setup({
				on_attach = on_attach,
				capabilities = capabilities,
				settings = {
					["rust-analyzer"] = {
						checkOnSave = {
							command = "clippy",
						},
						diagnostics = {
							disabled = {
								--  Keep ignoring mismatched-arg-count until https://github.com/rust-analyzer/rust-analyzer/issues/8654 is resolved.
								"mismatched-arg-count",
							},
						},
					},
				},
			})
		elseif lsp == "sumneko_lua" then
			lspconfig[lsp].setup({
				on_attach = on_attach,
				capabilities = capabilities,
				cmd = { "lua-language-server" },
				root_dir = function()
					return vim.loop.cwd()
				end,
				settings = {
					Lua = { diagnostics = { globals = { "vim" } } },
					workspace = {
						library = {
							[vim.fn.expand("$VIMRUNTIME/lua")] = true,
							[vim.fn.expand("$VIMRUNTIME/luavim/lsp")] = true,
						},
					},
					telemetry = { enable = false },
				},
			})
		else
			lspconfig[lsp].setup({
				on_attach = on_attach,
				capabilities = capabilities,
			})
		end
	end
end

setup_servers()
