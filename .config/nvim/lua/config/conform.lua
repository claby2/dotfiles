M = {}

M.setup = function()
	require("conform").setup({
		formatters_by_ft = {
			lua = { "stylua" },
			python = { "black" },
			go = { "gofmt" },
			rust = { "rustfmt" },
			sh = { "shfmt" },
            zsh = { "shfmt" },
		},
	})
end

return M
