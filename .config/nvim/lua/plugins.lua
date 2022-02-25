local packer = require("packer")
local use = packer.use

require("packer").startup(function()
	use("wbthomason/packer.nvim")
	use("nvim-lua/plenary.nvim")
	use("nvim-lua/popup.nvim")

	-- Language
	use("nvim-treesitter/nvim-treesitter")
	use("neovim/nvim-lspconfig")
	use("mhartington/formatter.nvim")
	use("onsails/lspkind-nvim")
	use("j-hui/fidget.nvim")

	-- Completion
	use("hrsh7th/nvim-cmp")
	use("hrsh7th/cmp-nvim-lsp")
	use("hrsh7th/cmp-nvim-lua")
	use("hrsh7th/cmp-buffer")
	use("hrsh7th/cmp-path")
	use("L3MON4D3/LuaSnip")
	use("saadparwaiz1/cmp_luasnip")

	-- Tree
	use("kyazdani42/nvim-tree.lua")
	use("kyazdani42/nvim-web-devicons")
	use("ryanoasis/vim-devicons")

	-- Telescope
	use("nvim-telescope/telescope.nvim")

	-- Theme
	use("ayu-theme/ayu-vim")

	-- Misc
	use("akinsho/toggleterm.nvim")
	use("baskerville/vim-sxhkdrc")
	use("elkowar/yuck.vim")
	use("hoob3rt/lualine.nvim")
	use("lervag/vimtex")
	use("lewis6991/gitsigns.nvim")
	use("norcalli/nvim-colorizer.lua")
	use("preservim/nerdcommenter")
end)

-- nvim-cmp
local cmp = require("cmp")
local lspkind = require("lspkind")

cmp.setup({
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body)
		end,
	},
	mapping = {
		["<C-u>"] = cmp.mapping.scroll_docs(-4),
		["<C-d>"] = cmp.mapping.scroll_docs(4),
		["<Tab>"] = cmp.mapping.select_next_item(),
		["<S-Tab>"] = cmp.mapping.select_prev_item(),
		["<CR>"] = cmp.mapping.confirm({
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		}),
	},
	formatting = {
		format = lspkind.cmp_format({
			with_text = true,
			-- Show completion source.
			menu = {
				buffer = "[Buffer]",
				luasnip = "[Snip]",
				nvim_lsp = "[LSP]",
				nvim_lua = "[Lua]",
				path = "[Path]",
			},
		}),
	},
	sources = {
		{ name = "nvim_lsp" },
		{ name = "nvim_lua" },
		{ name = "luasnip" },
		{ name = "buffer" },
		{ name = "path" },
	},
})

-- fidget.nvim
require("fidget").setup({})

-- formatter.nvim
local clang_format = function()
	return {
		exe = "clang-format",
		args = { "--style=file", "--fallback-style=Google" },
		stdin = true,
	}
end
local prettier_format = function()
	return {
		exe = "prettier",
		args = {
			"--stdin-filepath",
			vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)),
		},
		stdin = true,
	}
end
local shfmt = function()
	return { exe = "shfmt", stdin = true }
end
require("formatter").setup({
	logging = true,
	filetype = {
		python = {
			function()
				return { exe = "yapf", stdin = true }
			end,
		},
		c = { clang_format },
		cpp = { clang_format },
		java = { clang_format },
		javascript = { prettier_format },
		typescriptreact = { prettier_format },
		typescript = { prettier_format },
		markdown = { prettier_format },
		yaml = { prettier_format },
		html = { prettier_format },
		cmake = {
			function()
				return { exe = "cmake-format", stdin = true }
			end,
		},
		rust = {
			function()
				return {
					exe = "rustfmt",
					args = { "--edition", 2021, "--emit=stdout" },
					stdin = true,
				}
			end,
		},
		haskell = {
			function()
				return { exe = "hindent", stdin = true }
			end,
		},
		go = {
			function()
				return { exe = "gofmt", stdin = true }
			end,
		},
		lua = {
			function()
				return { exe = "stylua", stdin = false }
			end,
		},
		sh = { shfmt },
		zsh = { shfmt },
		tex = {
			function()
				return {
					exe = "latexindent",
					args = { "-sl", "-g /dev/stderr", "2>/dev/null" },
					stdin = true,
				}
			end,
		},
	},
})

-- nvim-tree.lua
require("nvim-tree").setup({})

-- telescope.nvim
require("telescope").setup({})

-- toggleterm.nvim
require("toggleterm").setup({
	open_mapping = [[<leader><space>]],
	direction = "float",
	float_opts = {
		border = "curved",
		winblend = 10,
	},
})

-- lualine.nvim
require("lualine").setup({
	options = { theme = "ayu_mirage" },
	sections = { lualine_b = { "filename" }, lualine_c = { "branch" } },
	extensions = { "nvim-tree" },
})

-- gitsigns.nvim
require("gitsigns").setup({
	keymaps = {
		["n <leader>hr"] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
		["n <leader>hp"] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
		["n <leader>hb"] = '<cmd>lua require"gitsigns".blame_line()<CR>',
	},
})

-- vimtex
vim.api.nvim_set_var("tex_flavor", "latex")
vim.api.nvim_set_var("vimtex_view_method", "zathura")
vim.api.nvim_set_var("vimtex_quickfix_mode", 0)

-- nvim-colorizer.lua
require("colorizer").setup({ "*", "xdefaults" })
