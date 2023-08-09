vim.diagnostic.config({
	severity_sort = true,
	update_in_insert = true,
	float = { border = "rounded", source = "always" },
})

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
	buf_set_keymap("n", "<leader>e", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
	buf_set_keymap("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
	buf_set_keymap("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
	buf_set_keymap("n", "<leader>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)
end

local set_floating_preview_borders = function()
	local border = {
		{ "╭" },
		{ "─" },
		{ "╮" },
		{ "│" },
		{ "╯" },
		{ "─" },
		{ "╰" },
		{ "│" },
	}
	vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = border })
	vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = border })
end

local setup_servers = function()
	set_floating_preview_borders()
	local servers = {
		"astro",
		"ccls",
		"hls",
		"pyright",
		"rust_analyzer",
		"texlab",
		"tsserver",
		"gopls",
		"cssls",
	}
	local lspconfig = require("lspconfig")
	local util = require("lspconfig").util
	local capabilities = require("cmp_nvim_lsp").default_capabilities()
	for _, lsp in ipairs(servers) do
		if lsp == "astro" then
			lspconfig[lsp].setup({
				on_attach = on_attach,
				capabilities = capabilities,
				cmd = { "astro-ls", "--stdio" },
				filetypes = { "astro" },
				root_dir = function(fname)
					return util.root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git")(fname)
				end,
				docs = {
					description = "https://github.com/withastro/language-tools",
					root_dir = [[root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git")]],
				},
				settings = {},
			})
		elseif lsp == "ccls" then
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
		else
			lspconfig[lsp].setup({
				on_attach = on_attach,
				capabilities = capabilities,
			})
		end
	end
end

setup_servers()
