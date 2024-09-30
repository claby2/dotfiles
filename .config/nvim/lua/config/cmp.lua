local cmp = require("cmp")

local scroll_amount = 4

local options = {
	mapping = {
		["<C-d>"] = cmp.mapping.scroll_docs(-scroll_amount),
		["<C-u>"] = cmp.mapping.scroll_docs(scroll_amount),
		["<C-j>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
		["<C-k>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
		["<CR>"] = cmp.mapping.confirm({
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		}),
	},
	sources = {
		{ name = "nvim_lsp" },
		{ name = "buffer" },
		{ name = "path" },
		{ name = "cmdline" },
	},
}
return options
