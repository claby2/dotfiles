local M = {}

M.setup = function()
	require("conform").setup({
		formatters_by_ft = {
			lua = { "stylua" },
			python = { "black" },
			go = { "gofmt" },
			rust = { "rustfmt" },
			sh = { "shfmt" },
			zsh = { "shfmt" },
			ocaml = { "ocamlformat" },
			cpp = { "clang-format" },
			c = { "clang-format" },
			astro = { "prettier" },
			json = { "prettier" },
			markdown = { "prettier" },
			typescript = { "prettier" },
			typescriptreact = { "prettier" },
			typst = { "typstyle" },
		},
	})
end

return M
