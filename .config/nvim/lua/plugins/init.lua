return {
	{ "nvim-telescope/telescope.nvim" },
	{
		"stevearc/conform.nvim",
		config = require("config.conform").setup,
	},
	{
		"nvim-tree/nvim-tree.lua",
		config = function()
			require("nvim-tree").setup({})
		end,
	},
	{
		"github/copilot.vim",
		config = function()
			vim.g.copilot_no_tab_map = true
		end,
	},
	{
		"Shatur/neovim-ayu",
		config = function()
			vim.cmd("colorscheme ayu-mirage")
		end,
	},
	{
		"neovim/nvim-lspconfig",
		config = require("config.lspconfig").setup,
	},
	{
		"nvim-treesitter/nvim-treesitter",
		config = require("config.treesitter").setup,
	},
}
