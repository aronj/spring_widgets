function widget:GetInfo()
  return {
    name = "con queue commands + geos",
    desc = "some commands for editing constructor build queue, and automatic geo toggling",
    author = "-",
    date = "dec, 2016",
    license = "GNU GPL, v3 or later",
    layer = 99,
    enabled = true
  }
end

local GetSelectedUnits = Spring.GetSelectedUnits
local GetUnitCommands = Spring.GetUnitCommands
local GiveOrderToUnit = Spring.GiveOrderToUnit
local GetTeamUnits = Spring.GetTeamUnits
local log = Spring.Echo
local GetUnitDefID = Spring.GetUnitDefID
local GiveOrderToUnit = Spring.GiveOrderToUnit
local GiveOrderToUnitMap = Spring.GiveOrderToUnitMap
local GetAllyTeamList = Spring.GetAllyTeamList
local GetMyTeamID = Spring.GetMyTeamID
local GetSelectedUnits = Spring.GetSelectedUnits
local GetTeamResources = Spring.GetTeamResources
local GetTeamResources = Spring.GetTeamResources
local SetTeamRulesParam = Spring.SetTeamRulesParam
local GetTeamRulesParam = Spring.GetTeamRulesParam
local GetTeamUnits = Spring.GetTeamUnits
local GetUnitCommands = Spring.GetUnitCommands
local GetUnitDefID = Spring.GetUnitDefID
local GetUnitHealth = Spring.GetUnitHealth
local GetUnitIsBuilding = Spring.GetUnitIsBuilding
local GetUnitPosition = Spring.GetUnitPosition
local GetUnitResources = Spring.GetUnitResources
local GetUnitsInCylinder = Spring.GetUnitsInCylinder
local GetUnitsInSphere = Spring.GetUnitsInSphere
local GiveOrderToUnit = Spring.GiveOrderToUnit
local regularizedResourceDerivativesMetal = {true}
local regularizedResourceDerivativesEnergy = {true}
local regularizedPositiveEnergy = true
local regularizedNegativeEnergy = false
local geos = {}
local mainIterationModuloLimit = 5
local myTeamId = GetMyTeamID()
local geos_on = true

function widget:Initialize()
  if Spring.GetSpectatingState() or Spring.IsReplay() then
    widgetHandler:RemoveWidget()
  end
--  SetTeamRulesParam(myTeamId, 'mmLevel', 0.2)
end

function widget:KeyPress(key, mods, isRepeat)
  if (key == 100 or key == 115 or key == 97) and mods['alt'] then -- 'd' shift from queue
    local selected_units = GetSelectedUnits()
    for i, unit_id in ipairs(selected_units) do
      local cmd_queue = GetUnitCommands(unit_id)
      if cmd_queue and #cmd_queue > 1 then
        if key == 100 then -- d
          removeFirstCommand(unit_id)
        elseif key == 115 then -- s
          removeLastCommand(unit_id)

        elseif key == 97 and #cmd_queue > 1 then -- a
            reverseQueue(unit_id)
        end
        -- does not seem to stop when removing to an empty queue, therefore:
        if #cmd_queue == 2 then
          GiveOrderToUnit(unit_id, CMD.INSERT, { -1, CMD.STOP, CMD.OPT_SHIFT }, { "alt" })
        end
      end
    end
  end
end

function removeFirstCommand(unit_id)
  local cmd_queue = GetUnitCommands(unit_id)
  if cmd_queue[2]['id'] == 70 then
    -- remove real command before empty one
    GiveOrderToUnit(unit_id, CMD.REMOVE, { cmd_queue[2].tag }, { nil })
  end
  GiveOrderToUnit(unit_id, CMD.REMOVE, { cmd_queue[1].tag }, { nil })
end

function removeLastCommand(unit_id)
  local cmd_queue = GetUnitCommands(unit_id)
  local remove_cmd = cmd_queue[#cmd_queue]
  -- empty commands are somehow put between cmds,
  -- but not by the "space/add to start of cmdqueue" widget
  if remove_cmd['id'] == 70 then
    -- remove real command before empty one
    GiveOrderToUnit(unit_id, CMD.REMOVE, { cmd_queue[#cmd_queue - 1].tag }, { nil })
  end
  -- remove the last command
  GiveOrderToUnit(unit_id, CMD.REMOVE, { cmd_queue[#cmd_queue].tag }, { nil })
end

function updateGeoDefs()
  geos = {}
  for _, unitId in ipairs(GetTeamUnits(Spring.GetMyTeamID())) do
    local unitDefId = GetUnitDefID(unitId)
    local udef = unitDef(unitId)
    if udef.name:find('geo') or udef.humanName:find('[Gg]eo') then

      local m = udef.makesMetal - udef.metalUpkeep
      local e = udef.energyUpkeep
      local eff = m/e
      geos[unitId] = {m, e, eff, true}
    end
  end
  table.sort(geos, function(a,b) return a[2] < b[2] end)
end

local function setGeos()
  updateGeoDefs()
  GiveOrderToUnitMap(geos, CMD.ONOFF, { geos_on and 1 or 0 }, {} )
end

function widget:GameFrame(n)
  if n % mainIterationModuloLimit == 0 then
    local mm_level = GetTeamRulesParam(myTeamId, 'mmLevel')
    local e_curr, e_max, e_pull, e_inc, e_exp = GetTeamResources(myTeamId, 'energy')
    local energyLevel = e_curr/e_max
    local isPositiveEnergyDerivative = e_inc > (e_pull+e_exp)/2

    table.insert(regularizedResourceDerivativesEnergy, 1, isPositiveEnergyDerivative)
    if #regularizedResourceDerivativesEnergy > 7 then
      table.remove(regularizedResourceDerivativesEnergy)
    end

    regularizedPositiveEnergy = table.full_of(regularizedResourceDerivativesEnergy, true)
    regularizedNegativeEnergy = table.full_of(regularizedResourceDerivativesEnergy, false)

    if not geos_on and regularizedPositiveEnergy and energyLevel > mm_level then
      geos_on = true
      setGeos()
    elseif geos_on and energyLevel < mm_level then
      geos_on = false
      setGeos()
    end
  end
end


function unitDef(unitId)
  return UnitDefs[GetUnitDefID(unitId)]
end




function removeNextCommand()

end

-- TODO
function reverseQueue(unit_id)

--  local states = Spring.GetUnitStates(targetID)

--  if (states ~= nil) then
--    Spring.GiveOrderToUnit(unitID, CMD.FIRE_STATE, { states.firestate }, 0)
--    Spring.GiveOrderToUnit(unitID, CMD.MOVE_STATE, { states.movestate }, 0)
--    Spring.GiveOrderToUnit(unitID, CMD.REPEAT,     { states['repeat']  and 1 or 0 }, 0)
--    Spring.GiveOrderToUnit(unitID, CMD.ONOFF,      { states.active     and 1 or 0 }, 0)
--    Spring.GiveOrderToUnit(unitID, CMD.CLOAK,      { states.cloak      and 1 or 0 }, 0)
--    Spring.GiveOrderToUnit(unitID, CMD.TRAJECTORY, { states.trajectory and 1 or 0 }, 0)
--  end

  local queue = Spring.GetCommandQueue(unit_id);
--  local build_queue = Spring.GetRealBuildQueue(unit_id)


  if queue then
    -- rm queue
    for k,v in ipairs(queue) do  --  in order
--    GiveOrderToUnit(unit_id, CMD.INSERT, { -1, CMD.STOP, CMD.OPT_SHIFT }, { "alt" })
    end

--    for int k,v in ipairs(queue) do  --  in order
--    for k,v in ipairs(queue) do  --  in order
    for i=#queue, 1, -1 do
      local v = queue[i]
      local options = v.options
      if not options.internal then
        local new_options = {}
        if (options.alt)   then table.insert(new_options, "alt")   end
        if (options.ctrl)  then table.insert(new_options, "ctrl")  end
        if (options.right) then table.insert(new_options, "right") end
        table.insert(new_options, "shift")
        --        Spring.GiveOrderToUnit(unit_id, v.id, v.params, options.coded)
        log(v.id)
        log(v.params)
--        table.insert(v.params, 1, 0)
        log(v.params)
        log(options.coded)
        Spring.GiveOrderToUnit(unit_id, v.id,-1, v.params, options.coded)
      end
    end
  end

  if (build_queue ~= nil) then
    for udid,buildPair in ipairs(build_queue) do
      local udid, count = next(buildPair, nil)
      Spring.AddBuildOrders(unit_id, udid, count)
    end
  end

end


function log(s)
  Spring.Echo(s)
end

function table.has_value(tab, val)
    for _, value in ipairs (tab) do
        if value == val then
            return true
        end
    end
    return false
end

function table.full_of(tab, val)
    for _, value in ipairs (tab) do
        if value ~= val then
            return false
        end
    end
    return true
end

-- for printing tables
function table.val_to_str(v)
  if "string" == type(v) then
    v = string.gsub(v, "\n", "\\n" )
    if string.match(string.gsub(v,"[^'\"]",""), '^"+$' ) then
      return "'" .. v .. "'"
    end
    return '"' .. string.gsub(v,'"', '\\"' ) .. '"'
  else
    return "table" == type(v) and table.tostring(v) or
      tostring(v)
  end
end

function table.key_to_str(k)
  if "string" == type(k) and string.match(k, "^[_%a][_%a%d]*$" ) then
    return k
  else
    return "[" .. table.val_to_str(k) .. "]"
  end
end

function table.tostring(tbl)
  local result, done = {}, {}
  for k, v in ipairs(tbl ) do
    table.insert(result, table.val_to_str(v) )
    done[ k ] = true
  end
  for k, v in pairs(tbl) do
    if not done[ k ] then
      table.insert(result,
        table.key_to_str(k) .. "=" .. table.val_to_str(v) )
    end
  end
  return "{" .. table.concat(result, "," ) .. "}"
end
