local ts_config = require("nvim-treesitter.configs")

ts_config.setup({
	ensure_installed = {
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
	highlight = { enable = true, use_languagetree = true },
})
