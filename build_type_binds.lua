function widget:GetInfo()
  return {
    name = "Build type binds (zxcv)",
    desc = "Nothing is copied from anywhere ʘ‿ʘ",
    author = "-",
    date = "mar, 2018",
    license = "GNU GPL, v2 or later",
    layer = 99,
    enabled = true
  }
end

local TraceScreenRay = Spring.TraceScreenRay
local GetMouseState = Spring.GetMouseState
local GetCmdDescIndex = Spring.GetCmdDescIndex
local SetActiveCommand = Spring.SetActiveCommand
local GetSelectedUnits = Spring.GetSelectedUnits
local GetUnitDefID = Spring.GetUnitDefID
local log = Spring.Echo
local UnitDefs = UnitDefs

local current_bo_index = 0
local selectedUnits
local buildOptionMap = {}

local terrain_categories = {
  'all',
  'ground',
  'water',
}

local type_categories = {
  'metal',
  'energy',
  'military',
  'construction',
}

local previous_type = type_categories[1]

local keys = {
  [122] = type_categories[1],
  [120] = type_categories[2],
  [99] = type_categories[3],
  [118] = type_categories[4],
}

function widget:Initialize()
  --  if Spring.GetSpectatingState() or Spring.IsReplay() then
  --    widgetHandler:RemoveWidget()
  --  end

  for source_udefid, def in pairs(UnitDefs) do
    local bo = def.buildOptions

    for i = 1, #bo do
      local target_udefid = bo[i]
      local target_udef = UnitDefs[bo[i]]
      --      log('bo', i, #bo, target_udef.humanName)

      if (target_udef.metalMake > 0 or
              target_udef.makesMetal > 0 or
              target_udef.metalStorage > 0 or
              getMetalMakingEfficiency(target_udefid) > 0 or
              target_udef.extractsMetal > 0) and target_udef.buildSpeed == 0 and #target_udef.weapons == 0 then
        save_to_build_option_map(source_udefid, type_categories[1], target_udefid, target_udef)
      end
      if (target_udef.energyStorage > 0 or
              target_udef.windGenerator > 0 or
              target_udef.tidalGenerator > 0 or
              target_udef.energyMake > 0) and target_udef.buildSpeed == 0 and #target_udef.weapons == 0 then
        save_to_build_option_map(source_udefid, type_categories[2], target_udefid, target_udef)
      end
      if (target_udef.radarRadius > 0 or
              target_udef.sonarRadius > 0 or
              target_udef.jammerRadius > 0 or
              target_udef.sonarJamRadius > 0 or
              target_udef.stealth or
              target_udef.sonarStealth or
              target_udef.seismicRadius > 0 or
              target_udef.canCloak or
              #target_udef.weapons > 0) and #target_udef.buildOptions == 0 then
        save_to_build_option_map(source_udefid, type_categories[3], target_udefid, target_udef)
      end
      if target_udef.isBuilder or
              target_udef.buildSpeed > 0 or
              #target_udef.buildOptions > 0 then
        save_to_build_option_map(source_udefid, type_categories[4], target_udefid, target_udef)
      end
    end

    --    buildOptionMapSourceIds[#buildOptionMapSourceIds + 1] = source_udefid
    sort_bo_map()
  end
end


function sort_metal(a, b)
  return (a.udef.extractsMetal * 100 + getMetalMakingEfficiency(a.id)) > (b.udef.extractsMetal * 100 + getMetalMakingEfficiency(b.id))
end

function sort_energy(a, b)
  return a.udef.energyMake > b.udef.energyMake
end

-- crashes spring
function sort_military(a, b)
  return (a.udef.weapons[1] == nil and 0 or 100) > (b.udef.weapons[1] == nil and 0 or 100)
end


function sort_bo_map()
  for _, map in pairs(buildOptionMap) do
    --  for n = 1, #buildOptionMapSourceIds do
    --    local map = buildOptionMap[buildOptionMapSourceIds[n]]
    if map ~= nil then
      for i = 1, #terrain_categories do
        table.sort(map.metal[terrain_categories[i]], sort_metal)
        --      if #map.metal[terrain_categories[i]] > 0 then
        --      end
        --      table.sort(map.metal[terrain_categories[i]], sort_metal)
        --        table.sort(map.military[terrain_categories[i]], sort_military)
        --      if #map.military[terrain_categories[i]] > 0 then
        --      end
      end
    end
  end
end


function deepcopy(orig)
  local orig_type = type(orig)
  local copy
  if orig_type == 'table' then
    copy = {}
    for orig_key, orig_value in next, orig, nil do
      copy[deepcopy(orig_key)] = deepcopy(orig_value)
    end
    setmetatable(copy, deepcopy(getmetatable(orig)))
  else -- number, string, boolean, etc
    copy = orig
  end
  return copy
end


--function save_to_global_sort(category, target_udefid, target_udef)
--  global_sort[category] = global_sort[category] or {}
--  global_sort[category][target_udefid] = {
--    id = target_udefid,
--    udef = target_udef
--  }
--end

function save_to_build_option_map(source_udefid, category, target_udefid, target_udef)
  local terrain_types = {
    all = {},
    ground = {},
    water = {},
  }
  buildOptionMap[source_udefid] = buildOptionMap[source_udefid] or {
    metal = deepcopy(terrain_types),
    energy = deepcopy(terrain_types),
    military = deepcopy(terrain_types),
    construction = deepcopy(terrain_types),
  }
  local bo = {
    parent_id = source_udefid,
    id = target_udefid,
    udef = target_udef
  }
  local all = buildOptionMap[source_udefid][category].all
  all[#all + 1] = bo
  if target_udef.minWaterDepth < 0 then
    local ground = buildOptionMap[source_udefid][category].ground
    ground[#ground + 1] = bo
  end
  if target_udef.minWaterDepth > 0 then
    local water = buildOptionMap[source_udefid][category].water
    water[#water + 1] = bo
  end
end


function widget:CommandsChanged()
  current_bo_index = 0
end

function widget:MousePress(x, y, button)
  current_bo_index = 0
end

function widget:MouseRelease(x, y, button)
  current_bo_index = 0
end

function widget:KeyPress(key, mods, isRepeat)

  local type = keys[key]
  if key == 27 then
    current_bo_index = 0
  elseif type ~= nil and not mods['ctrl'] then
    local selected = GetSelectedUnits()

    local currentPower = 0
    local hasCon = false
    local currentUdefId
    local done_udefids = {}

    for i = 1, #selected do
      local udefid = GetUnitDefID(selected[i])

      if not done_udefids[udefid] and buildOptionMap[udefid] then
        local udef = UnitDefs[udefid]
        local power = udef.power

        -- prio moving units, then by power
        if hasCon and udef.speed > 0 and power > currentPower then
          currentUdefId = udefid
          currentPower = power
        elseif not hasCon and power > currentPower then
          currentUdefId = udefid
          currentPower = power
          hasCon = udef.speed > 0
        elseif not hasCon and udef.speed > 0 then
          currentUdefId = udefid
          currentPower = power
          hasCon = true
        end
      end
      done_udefids[udefid] = true
    end
    if currentUdefId ~= nil then
      cycle_bo(currentUdefId, type, mods['shift'])
    end
  end
end

function cycle_bo(udefid, type, reverse)
  local elevation = get_elevation()
  local terrain = terrain_categories[1]

  if elevation ~= nil then
    if elevation > 0 then
      terrain = terrain_categories[2]
    else
      terrain = terrain_categories[3]
    end
  end

  local bos = buildOptionMap[udefid][type][terrain]
  if bos == nil or #bos == 0 then
    return
  end

  --  log('cycle bo', UnitDefs[udefid].humanName, type, current_bo_index, bos)
  if type ~= previous_type then
    current_bo_index = 1
  else
    current_bo_index = current_bo_index + (reverse and -1 or 1)

    --  log('bos count', type, terrain, #bos)
    if current_bo_index > #bos then
      current_bo_index = 1
    elseif current_bo_index < 1 then
      current_bo_index = #bos
    end
  end
  previous_type = type

  --  log('bos', type, terrain, #bos, current_bo_index)
  --  log('bos', type, terrain, #bos, bos[current_bo_index].udef.humanName)

  --  log('SetActiveCommand', 'bos[i]', bos[current_bo_index].id)
  SetActiveCommand(GetCmdDescIndex(-bos[current_bo_index].id))
end

function get_elevation()
  local mouseX, mouseY = GetMouseState()
  local desc, coords = TraceScreenRay(mouseX, mouseY, true)

  if desc == nil then
    return nil
  end

  return coords[3]
end

function getMetalMakingEfficiency(unitDefID)
  if not WG.energyConversion then
    log("Widget: build_type_binds.lua needs WG.energyConversion to function properly. Try enabling widget 'Energy Conversion Info'")
    return 0
  end
  if not unitDefID then
    return 0
  end
  local makerDef = WG.energyConversion.convertCapacities[unitDefID]
  if makerDef ~= nil then
    return makerDef.e
  else
    return 0
  end
end

-- for debug
function table.has_value(tab, val)
  for i = 1, #tab do
    if tab[i] == val then
      return true
    end
  end
  return false
end

function table.full_of(tab, val)
  for i = 1, #tab do
    if tab[i] ~= val then
      return false
    end
  end
  return true
end

-- for printing tables
function table.val_to_str(v)
  if "string" == type(v) then
    v = string.gsub(v, "\n", "\\n")
    if string.match(string.gsub(v, "[^'\"]", ""), '^"+$') then
      return "'" .. v .. "'"
    end
    return '"' .. string.gsub(v, '"', '\\"') .. '"'
  else
    return "table" == type(v) and table.tostring(v) or tostring(v)
  end
end

function table.key_to_str(k)
  if "string" == type(k) and string.match(k, "^[_%a][_%a%d]*$") then
    return k
  else
    return "[" .. table.val_to_str(k) .. "]"
  end
end

function str_table(tbl)
  return table.tostring(tbl)
end

function table.tostring(tbl)
  local result, done = {}, {}
  if type(elem) ~= 'table' then
    return tbl
  end
  for k, v in ipairs(tbl) do
    table.insert(result, table.val_to_str(v))
    done[k] = true
  end
  for k, v in pairs(tbl) do
    if not done[k] then
      table.insert(result, table.key_to_str(k) .. "=" .. table.val_to_str(v))
    end
  end
  return "{" .. table.concat(result, ",") .. "}"
end
