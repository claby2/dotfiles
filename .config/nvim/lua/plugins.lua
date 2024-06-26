local packer = require("packer")
local use = packer.use

require("packer").startup(function()
	use("wbthomason/packer.nvim")
	use("nvim-lua/plenary.nvim")
	use("nvim-lua/popup.nvim")

	-- Language
	use("nvim-treesitter/nvim-treesitter")
	use({ "neovim/nvim-lspconfig", tag = vim.fn.has("nvim-0.8") == 1 and "*" or "v0.1.6" })
	use("mhartington/formatter.nvim")
	use("onsails/lspkind-nvim")
	use({ "j-hui/fidget.nvim", tag = "legacy", disable = vim.fn.has("nvim-0.9") == 0 })
	use("NoahTheDuke/vim-just")
	use("rachitnigam/pyret-lang.vim")

	-- Completion
	use("hrsh7th/nvim-cmp")
	use("hrsh7th/cmp-nvim-lsp")
	use("hrsh7th/cmp-nvim-lua")
	use("hrsh7th/cmp-buffer")
	use("hrsh7th/cmp-path")
	use("L3MON4D3/LuaSnip")
	use("saadparwaiz1/cmp_luasnip")

	-- Tree
	use({ "kyazdani42/nvim-tree.lua", tag = vim.fn.has("nvim-0.8") == 1 and "*" or "compat-nvim-0.7" })
	use("kyazdani42/nvim-web-devicons")
	use("ryanoasis/vim-devicons")

	-- Telescope
	use({ "nvim-telescope/telescope.nvim", tag = vim.fn.has("nvim-0.9.0") == 1 and "*" or "0.1.2" })

	-- Theme
	use({
		"Shatur/neovim-ayu",
		commit = vim.fn.has("nvim-0.8") == 1 and "HEAD" or "0198dcf2d5585742220e21e002f095464874e19e",
	})

	-- Misc
	use("aklt/plantuml-syntax")
	use({"akinsho/toggleterm.nvim", tag = vim.fn.has("nvim-0.8") == 1 and "*" or "v2.7.0"})
	use("baskerville/vim-sxhkdrc")
	use("elkowar/yuck.vim")
	use("hoob3rt/lualine.nvim")
	use("lervag/vimtex")
	use({ "lewis6991/gitsigns.nvim", tag = vim.fn.has("nvim-0.8") == 1 and "*" or "v0.6" })
	use("norcalli/nvim-colorizer.lua")
	use("preservim/nerdcommenter")
	use("github/copilot.vim")
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
if vim.fn.has("nvim-0.9") == 1 then
	require("fidget").setup({})
end

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
		javascriptreact = { prettier_format },
		typescriptreact = { prettier_format },
		typescript = { prettier_format },
		markdown = { prettier_format },
		astro = {
			function()
				return {
					exe = "npx",
					args = {
						"prettier",
						"--stdin-filepath",
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)),
						"--plugin=prettier-plugin-astro",
					},
					stdin = true,
				}
			end,
		},
		ocaml = {
			function()
				return {
					exe = "ocamlformat",
					args = {
						"--enable-outside-detected-project",
						"--name",
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)),
						"-",
					},
					stdin = true,
				}
			end,
		},
		yaml = { prettier_format },
		html = { prettier_format },
		css = { prettier_format },
		sql = {
			function()
				return { exe = "pg_format --inplace -", stdin = true }
			end,
		},
		scheme = {
			function()
				return { exe = "raco fmt --width 70", stdin = true }
			end,
		},
		lisp = {
			function()
				return { exe = "raco fmt --width 70", stdin = true }
			end,
		},
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
				return { exe = "gofumpt", stdin = true }
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
					args = { "-g", "/dev/null" },
					stdin = true,
				}
			end,
		},
	},
})

-- nvim-tree.lua
require("nvim-tree").setup({})

-- telescope.nvim
require("telescope").setup({
	pickers = {
		find_files = {
			hidden = true,
			file_ignore_patterns = { ".git/" },
		},
	},
})

-- neovim-ayu
require("ayu").setup({
	mirage = true,
	overrides = {},
})
require("ayu").colorscheme()

-- toggleterm.nvim
require("toggleterm").setup({
	open_mapping = [[<leader><space>]],
	direction = "float",
	float_opts = {
		border = "curved",
	},
})

-- lualine.nvim
require("lualine").setup({
	sections = {
		lualine_a = { "mode" },
		lualine_b = { "filename" },
		lualine_c = { "branch" },
		lualine_x = { "filetype" },
		lualine_y = { "diagnostics" },
		lualine_z = {},
	},
	extensions = { "nvim-tree" },
	options = {
		theme = "auto",
		section_separators = { left = "", right = "" },
	},
})

-- gitsigns.nvim
require("gitsigns").setup({
	on_attach = function(bufnr)
		local gs = package.loaded.gitsigns
		local function map(mode, l, r, opts)
			opts = opts or {}
			opts.buffer = bufnr
			vim.keymap.set(mode, l, r, opts)
		end
		map("n", "<leader>hr", gs.reset_hunk)
		map("n", "<leader>hp", gs.preview_hunk)
		map("n", "<leader>hb", function()
			gs.blame_line({ full = true })
		end)
	end,
})

-- vimtex
vim.api.nvim_set_var("tex_flavor", "latex")
vim.api.nvim_set_var("vimtex_view_method", "zathura")
vim.api.nvim_set_var("vimtex_quickfix_mode", 0)
vim.api.nvim_set_var("vimtex_syntax_enabled", 0)

-- nvim-colorizer.lua
require("colorizer").setup({ "*", "xdefaults" })

-- nerdcommenter
vim.api.nvim_set_var("NERDCustomDelimiters", {
	pyret = {
		left = "# ",
	},
})

-- copilot.vim
vim.g.copilot_no_tab_map = true
vim.api.nvim_set_keymap("i", "<C-J>", 'copilot#Accept("<CR>")', { silent = true, expr = true })
