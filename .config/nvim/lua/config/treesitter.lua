local M = {}

M.setup = function()
	local ts_config = require("nvim-treesitter.configs")

	ts_config.setup({
		ensure_installed = {
			"python",
			"lua",
			"rust",
			"go",
			"c",
			"cpp",
			"ocaml",
		},
	})
end

return M
