local h = {}
_G._ghostfork = h
_G.h = h

function h.find_block_at_cursor()
  local node = vim.treesitter.get_node()
  if node == nil then return nil end
  -- a block is a list of multiple LINESTRING:s
  if node:type() ~= "LINESTRING" then return nil end
  local blk = node:parent()
  local first, _, last, _ = blk:range()
  local text = vim.api.nvim_buf_get_lines(0, first, last+1, true)
  return first, text
end
