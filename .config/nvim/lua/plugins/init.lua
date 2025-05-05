return {
	{ "echasnovski/mini.statusline", version = "*", opts = {} },
	{
		"ibhagwan/fzf-lua",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = {},
	},
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {},
	},
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
			local colors = require("ayu.colors")
			colors.generate(true)
			vim.cmd("colorscheme ayu-mirage")
		end,
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
		init = function()
			require("config.gitsigns").setup()
			require("gitsigns").setup({
				sign_priority = 20,
			})
		end,
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
	{
		"quarto-dev/quarto-nvim",
		dependencies = {
			"jmbuhr/otter.nvim",
			"nvim-treesitter/nvim-treesitter",
		},
	},
	{
		"kaarmu/typst.vim",
		ft = "typst",
		init = function() end,
	},
	{
		"chomosuke/typst-preview.nvim",
		ft = "typst",
		version = "1.*",
		opts = {
			-- Typst preview is weird on safari, so use chrome
			-- NOTE: I think this only works on mac, should probably make it more general
			open_cmd = 'open -a "Google Chrome" %s',
		},
	},
}
