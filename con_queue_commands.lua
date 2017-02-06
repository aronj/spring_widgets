function widget:GetInfo()
  return {
    name = "con queue commands",
    desc = "some commands for editing constructor build queue",
    author = "-",
    date = "dec, 2016",
    license = "GNU GPL, v3 or later",
    layer = 99,
    enabled = false
  }
end

local GetSelectedUnits = Spring.GetSelectedUnits
local GetUnitCommands = Spring.GetUnitCommands
--local GetUnitDefID = Spring.GetUnitDefID
local GiveOrderToUnit = Spring.GiveOrderToUnit
--local GiveOrderArrayToUnitArray = Spring.GiveOrderArrayToUnitArray
local log = Spring.Echo

function widget:Initialize()
  if Spring.GetSpectatingState() or Spring.IsReplay() then
    widgetHandler:RemoveWidget()
  end
end

function widget:KeyPress(key, mods, isRepeat)
  if (key == 100 or key == 115 or key == 97) and mods['alt'] then -- 'd' shift from queue
    local selected_units = GetSelectedUnits()
    for i, unit_id in ipairs(selected_units) do
      local cmdQueue = GetUnitCommands(unit_id)
--      log('table.tostring(cmdQueue)')
--      log(#cmdQueue)
--      log(table.tostring(cmdQueue))
      if cmdQueue and #cmdQueue > 1 then
        if key == 100 then -- d
--           log('shift/rm first')
          local remove_cmd = cmdQueue[1]
--          log(table.tostring(remove_cmd))
          -- dunno why need nil ctrl doesnt work when patrolish
          GiveOrderToUnit(unit_id, CMD.REMOVE, { remove_cmd.tag }, { nil })
        elseif key == 115 then -- s
--           log('pop')
          local remove_cmd = cmdQueue[#cmdQueue]
          -- empty commands are somehow put between cmds,
          -- but not by the "space/add to start of cmdqueue" widget
          -- log('remove_cmd[id] ' .. remove_cmd['id'])
          if remove_cmd['id'] == 70 then
            -- remove real command before empty one
            GiveOrderToUnit(unit_id, CMD.REMOVE, { cmdQueue[#cmdQueue - 1].tag }, { nil })
          end
          -- remove the last command
          GiveOrderToUnit(unit_id, CMD.REMOVE, { cmdQueue[#cmdQueue].tag }, { nil })

        elseif key == 97 and #cmdQueue > 1 then -- a
          log('not implemented yet')
          -- a = cmdQueue[1]
          -- b = cmdQueue[#cmdQueue]
          for i=1, #cmdQueue, 1 do
            log('before '..table.tostring(cmdQueue[i]))
            cmdQueue[i][1] = 0
            log('after '..table.tostring(cmdQueue[i]))
            log('repair '.. CMD.REPAIR)
            GiveOrderToUnit(unit_id, CMD.INSERT, {0, cmdQueue[i], cmdQueue[i].params, cmdQueue[i].option }, { nil })

          end


          -- log(table.tostring(a))
          -- log(table.tostring(b))
          -- cmdQueue.remove()
          -- GiveOrderArrayToUnitArray({unit_id}, cmdQueue)
        end

        -- does not seem to stop when removing to an empty queue, therefore:
        if #cmdQueue == 2 then
          GiveOrderToUnit(unit_id, CMD.INSERT, { -1, CMD.STOP, CMD.OPT_SHIFT }, { "alt" })
        end
      end
    end
  end
end

function removeNextCommand()

end

-- GiveOrderToUnit(builderID, CMD.INSERT, {0, CMD.REPAIR, CMD.OPT_CTRL, candidateID}, {"alt"})


-- function doFastForwardDecision(builderID, targetID, cmdQueueTag, cmdQueueTagg)
--   selectedUnits = GetSelectedUnits()
--   local totalBuildSpeed = getBuildersBuildSpeed(getUnitsBuildingUnit(targetID))
--   local secondsLeft = getBuildTimeLeft(targetID)
--   local unitDef = UnitDefs[GetUnitDefID(targetID)]
--   if isTimeToMoveOn(secondsLeft, builderID, unitDef, totalBuildSpeed) and isResourceToMoveOn(secondsLeft, unitDef, totalBuildSpeed) then
--     moveOnFromBuilding(builderID, targetID, cmdQueueTag, cmdQueueTagg)
--   end
-- end

-- local cmdQueue = GetUnitCommands(builderID, 3);

-- GiveOrderToUnit(builderID, CMD.REMOVE, {cmdQueueTag}, {nil})


-- for logging
function table.val_to_str(v)
  if "string" == type(v) then
    v = string.gsub(v, "\n", "\\n")
    if string.match(string.gsub(v, "[^'\"]", ""), '^"+$') then
      return "'" .. v .. "'"
    end
    return '"' .. string.gsub(v, '"', '\\"') .. '"'
  else
    return "table" == type(v) and table.tostring(v) or
            tostring(v)
  end
end

function table.key_to_str(k)
  if "string" == type(k) and string.match(k, "^[_%a][_%a%d]*$") then
    return k
  else
    return "[" .. table.val_to_str(k) .. "]"
  end
end

function table.tostring(tbl)
  local result, done = {}, {}
  for k, v in ipairs(tbl) do
    table.insert(result, table.val_to_str(v))
    done[k] = true
  end
  for k, v in pairs(tbl) do
    if not done[k] then
      table.insert(result,
        table.key_to_str(k) .. "=" .. table.val_to_str(v))
    end
  end
  return "{" .. table.concat(result, ",") .. "}"
end
