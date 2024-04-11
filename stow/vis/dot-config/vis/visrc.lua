require("vis")
require("plugins/vis-commentary")

--- Extract the file name part of a path
-- @param path string: File path
-- @return string: Substring of path that is the file
local function get_name(path)
  return path:match("[^/]+$")
end

--- Extract the file suffix part of a path
-- @param path string: File path
-- @return string: Substring of path that is the file suffix
local function get_suffix(path)
  return path:match("[^.]+$")
end

--- Default init settings
local function init_settings()
  vis:command("set change-256colors off")
  -- Text wrap operator, somewhat like vim's gq<motion>
  if os.execute("command -v par >/dev/null 2>&1") then
    vis:operator_new("gq", function(file, range, pos)
      local status, out, err = vis:pipe(file, range, "par")
      if not status then
        vis:info(err)
      else
        file:delete(range)
        file:insert(range.start, out)
      end
      return range.start -- new cursor location
    end, "Formatting operator, filter range through par(1)")
  end
end

--- Default win settings
-- @param win Window: Window object
local function win_settings(win)
  vis:command("set autoindent on")
  vis:command("set numbers on")
  vis:command("set show-tabs on")
end

--- Filetype-specific settings
-- @param file File: File object
local function filetype_settings(file)
  path = file.path

  -- No need to do anything for unnamed files like the help buffer
  if path == nil then
    return
  end

  name = get_name(path)
  suffix = get_suffix(path)

  if name:match("[Mm]akefile") then
    vis:command("set tabwidth 8")
    vis:command("set expandtab off")
  elseif suffix:match("go") then
    vis:command("set tabwidth 4")
    vis:command("set expandtab on")
  elseif suffix:match("jl") then
    vis:command("set tabwidth 4")
    vis:command("set expandtab on")
  elseif suffix:match("lua") then
    vis:command("set tabwidth 2")
    vis:command("set expandtab on")
  elseif suffix:match("py") then
    vis:command("set tabwidth 4")
    vis:command("set expandtab on")
    vis:command("set cursorline 80")
  end
end

vis.events.subscribe(vis.events.INIT, init_settings)
vis.events.subscribe(vis.events.WIN_OPEN, win_settings)
vis.events.subscribe(vis.events.FILE_OPEN, filetype_settings)
