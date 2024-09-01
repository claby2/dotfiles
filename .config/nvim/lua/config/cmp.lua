local cmp = require("cmp")

local scroll_amount = 4

local options = {
	mapping = {
		["<C-d>"] = cmp.mapping.scroll_docs(-scroll_amount),
		["<C-u>"] = cmp.mapping.scroll_docs(scroll_amount),
		["<Tab>"] = cmp.mapping.select_next_item(),
		["<S-Tab>"] = cmp.mapping.select_prev_item(),
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
