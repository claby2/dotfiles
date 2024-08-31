return {
	{ "nvim-telescope/telescope.nvim" },
	{
		"mhartington/formatter.nvim",
		config = function()
			require("formatter").setup({
				logging = true,
				filetype = {
					lua = {
						require("formatter.filetypes.lua").stylua,
					},
				},
			})
		end,
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
		config = function()
			require("config.lspconfig").setup()
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter",
		config = function()
			require("config.treesitter").setup()
		end,
	},
}
