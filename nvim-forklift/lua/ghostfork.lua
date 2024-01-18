local h = {}
_G._ghostfork = h
_G.h = h

h.ns = vim.api.nvim_create_namespace('ghostfork')

function h.find_block_at_cursor()
  local node = vim.treesitter.get_node()
  if node == nil then return nil end
  -- a block is a list of multiple LINESTRING:s
  if node:type() ~= "LINESTRING" then return nil end
  local blk = node:parent()
  local first, _, last, _ = blk:range()
  local text = vim.api.nvim_buf_get_lines(0, first, last+1, true)
  local strtext = {}
  for _, line in ipairs(text) do
    local _, send = string.find(line, '^%s+\\\\')
    strtext[#strtext + 1] = string.sub(line, send+1)
  end
  return first, strtext
end

function h.try_compile(text)
  local fname = vim.fn.tempname()
  -- TODO: stdin!
  local f = io.open(fname, 'w')
  f:write(text)
  f:close()

  local data = vim.system({'./zig-out/bin/run', fname}):wait()
  return data
end

function h.parse_diag(stderr)
  -- TODO: make output of CFOScript errors more structured :P
  lines = vim.split(stderr, "\n")
  local row,col
  for i,l in ipairs(lines) do
    local strrow, strcol = string.match(l, "fail at (%d+):(%d+)$")
    if strrow then
      row,col = tonumber(strrow), tonumber(strcol)
      break
    end
  end

  dtext = table.concat(lines, '|', 1, math.min(3, #lines))

  return row, col, dtext
end

function h.check_block_at_cursor()
  local line, data = h.find_block_at_cursor()
  if line == nil then
    print("no BOLK")
    return
  end

  data = table.concat(data, '\n') .. '\n'

  vim.api.nvim_buf_clear_namespace(0, h.ns, 0, -1)

  local res = h.try_compile(data)

  if res.code == 0 then
    print("OK")
    return
  end

  h.all_data = res.stderr

  local row, col, dtext = h.parse_diag(res.stderr)
  if row ~= nil then
    row = line + row - 1
    local text = vim.api.nvim_buf_get_lines(0, row, row+1, true)[1]
    local _, send = string.find(text, '^%s+\\\\')
    if send then
      col = col + send - 1
    end

    vim.api.nvim_buf_set_extmark(0, h.ns, row, col, {virt_text={{'  ■ '..dtext, 'DiagnosticError'}}, hl_group='Underlined', end_col=col+1})
  end

  return row, col, dtext
end

function h.reg_autocmd()
  -- only clear for current buffer
  local id = vim.api.nvim_create_augroup("ghostfork", {clear=false})
  vim.api.nvim_clear_autocmds({buffer=0, group=id})
  vim.api.nvim_create_autocmd({"TextChanged", "TextChangedI"}, {
    buffer = 0,
    group = id,
    callback = function()
      local status, res = pcall(h.check_block_at_cursor)
      if not status then
        print("ERROR: _ghostfork.lasterror")
        h.lasterror = res
      end
    end
  })
end
