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
			typescript = { "prettier" },
			astro = { "prettier" },
			json = { "prettier" },
			markdown = { "prettier" },
		},
	})
end

return M
