local ts_config = require("nvim-treesitter.configs")

ts_config.setup({
	ensure_installed = {
		"astro",
		"bash",
		"c",
		"c_sharp",
		"cpp",
		"css",
		"gdscript",
		"go",
		"haskell",
		"html",
		"java",
		"javascript",
		"json",
		"latex",
		"lua",
		"python",
		"regex",
		"rust",
		"toml",
		"tsx",
		"typescript",
	},
	highlight = { enable = vim.fn.has("nvim-0.9") == 1, use_languagetree = true },
})
