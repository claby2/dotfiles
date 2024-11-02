local M = {}
local map = vim.keymap.set

M.on_attach = function(_, bufnr)
	local function opts(desc)
		return { buffer = bufnr, noremap = true, silent = true, desc = "LSP " .. desc }
	end

	-- Buffer mappings
	map("n", "gD", vim.lsp.buf.declaration, opts("Go to declaration"))
	map("n", "K", vim.lsp.buf.hover, opts("Show hover info"))
	map("n", "gd", vim.lsp.buf.definition, opts("Go to definition"))
	map("n", "gi", vim.lsp.buf.implementation, opts("Go to implementation"))
	map("n", "gr", vim.lsp.buf.references, opts("Show references"))
	map("n", "<leader>D", vim.lsp.buf.type_definition, opts("Go to type definition"))
	map("n", "<leader>ca", vim.lsp.buf.code_action, opts("Show code actions"))
	map("n", "<leader>rn", vim.lsp.buf.rename, opts("Rename symbol"))

	-- Diagnostic mappings
	map("n", "<leader>e", vim.diagnostic.open_float, opts("Show diagnostics"))
	map("n", "[d", vim.diagnostic.goto_prev, opts("Go to previous diagnostic"))
	map("n", "]d", vim.diagnostic.goto_next, opts("Go to next diagnostic"))
end

-- Disable semanticTokens
M.on_init = function(client, _)
	if client.supports_method("textDocument/semanticTokens") then
		client.server_capabilities.semanticTokensProvider = nil
	end

	-- Workaround: https://github.com/neovim/neovim/issues/30985#issuecomment-2447329525
	for _, method in ipairs({ "textDocument/diagnostic", "workspace/diagnostic" }) do
		local default_diagnostic_handler = vim.lsp.handlers[method]
		vim.lsp.handlers[method] = function(err, result, context, config)
			if err ~= nil and err.code == -32802 then
				return
			end
			return default_diagnostic_handler(err, result, context, config)
		end
	end
end

M.capabilities = vim.lsp.protocol.make_client_capabilities()

-- Helper function to setup LSP servers
M.setup_server = function(server_name, config)
	local lspconfig = require("lspconfig")
	config = vim.tbl_deep_extend("force", {
		on_attach = M.on_attach,
		on_init = M.on_init,
		capabilities = M.capabilities,
	}, config or {})
	lspconfig[server_name].setup(config)
end

M.setup = function()
	-- Setup servers with common configurations
	M.setup_server("pyright")
	M.setup_server("rust_analyzer")
	M.setup_server("gopls")
	M.setup_server("ccls")
	M.setup_server("ocamllsp")
	M.setup_server("lua_ls", {
		settings = {
			Lua = {
				diagnostics = {
					globals = { "vim" },
				},
			},
		},
	})
end

return M
