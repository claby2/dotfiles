return {
	{ "nvim-telescope/telescope.nvim", config = require("config.telescope").setup },
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
	{
		"akinsho/toggleterm.nvim",
		version = "*",
		opts = {
			open_mapping = [[<leader><space>]],
			direction = "float",
			float_opts = {
				border = "curved",
			},
		},
	},
	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter",
		dependencies = {
			{
				"hrsh7th/cmp-nvim-lsp",
				"hrsh7th/cmp-buffer",
				"hrsh7th/cmp-path",
				"hrsh7th/cmp-cmdline",
			},
		},
		opts = function()
			return require("config.cmp")
		end,
	},
	{
		"lervag/vimtex",
		event = { "BufReadPre *.tex", "BufNewFile *.tex" },
		init = function()
			local view_method = "zathura"
			if vim.fn.has("mac") == 1 then
				view_method = "skim"
			end
			vim.g.vimtex_view_method = view_method
		end,
	},
	{
		"lewis6991/gitsigns.nvim",
		event = "BufRead",
		config = require("config.gitsigns").setup,
	},
	{
		"Julian/lean.nvim",
		event = { "BufReadPre *.lean", "BufNewFile *.lean" },

		dependencies = {
			"neovim/nvim-lspconfig",
			"nvim-lua/plenary.nvim",
			-- you also will likely want nvim-cmp or some completion engine
		},

		-- see details below for full configuration options
		opts = {
			lsp = {},
			mappings = true,
		},
	},
}
