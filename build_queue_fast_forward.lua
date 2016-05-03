function widget:GetInfo()
    return {
        name = "build queue fast forward",
        desc = "Lots of code from gui_build_costs.lua Milan Satala and also some from ecostats.lua by Jools, iirc",
        author = "-",
        date = "feb, 2016",
        license = "GNU GPL, v2 or later",
        layer = 99,
        enabled = true
    }
end

local builders = {}
local commanderBuildSpeed = 100
local myTeamID = Spring.GetMyTeamID()
local willStall = false
local selectedUnits = nil


local metalMakers = {}
local possibleMetalMakersUpkeep = 0
local possibleMetalMakersProduction = 0
local releasedMetal = 0

local GetAllyTeamList = Spring.GetAllyTeamList
local GetMyTeamID = Spring.GetMyTeamID
local GetSelectedUnits = Spring.GetSelectedUnits
local GetTeamUnits = Spring.GetTeamUnits
local GetUnitCommands = Spring.GetUnitCommands
local GetUnitDefID = Spring.GetUnitDefID
local GetUnitHealth = Spring.GetUnitHealth
local GetUnitIsBuilding = Spring.GetUnitIsBuilding
local GetUnitResources = Spring.GetUnitResources
local GiveOrderToUnit = Spring.GiveOrderToUnit
local GetTeamResources = Spring.GetTeamResources
local GetTeamRulesParam = Spring.GetTeamRulesParam

local tidalStrength = Game.tidal
local windMin = Game.windMin
local windMax = Game.windMax

local log = Spring.Echo
local t0
local totalSavedTime = 0
local abandonedTargetIDs = {}

function widget:Initialize()
    if Spring.GetSpectatingState() or Spring.IsReplay() then
        widgetHandler:RemoveWidget()
    end

    registerUnits()
end

function registerUnits()
    builders = {}
    metalMakers = {}
    possibleMetalMakersUpkeep = 0
    possibleMetalMakersProduction = 0

    local myUnits = GetTeamUnits(myTeamID)
    for _, unitID in ipairs(myUnits) do
        local unitDefID = GetUnitDefID(unitID)
        registerUnit(unitID, unitDefID, teamID)
    end
end



function registerUnit(unitID, unitDefID, unitTeam)

  if not unitDefID then
    return
  end

  local unitDef = UnitDefs[unitDefID]

  if isBuilder(unitDef) then

    builders[unitID] = {["buildSpeed"] = unitDef.buildSpeed, originalBuildSpeed = unitDef.buildSpeed, ['unitDef'] = unitDef, ["targetID"] = nil, ["guards"] = {}, ['previousBuilding'] = nil}
    -- log(UnitDefs[unitDefID].humanName .. " registered")

    if unitDef.customParams.iscommander then
      commanderBuildSpeed = unitDef.buildSpeed
    end

  -- elseif unitTeam == myTeamID and isMetalMaker(unitDef) then
    -- registerMetalMaker(unitID, unitDef)
  -- else
    -- return
  -- end
  end

end



function widget:UnitCreated(unitID, unitDefID, unitTeam)
  registerUnit(unitID, unitDefID, unitTeam)
end

function widget:UnitGiven(unitID, unitDefID, unitTeam, oldTeam)
  if unitTeam == myTeamID then
    if builders[unitID] then
      builders[unitID].owned = true
    else
      -- local unitDef = UnitDefs[unitDefID]
      -- if isMetalMaker(unitDef) then
        -- registerMetalMaker(unitID, unitDef)
      -- end
    end
  end
end


function getBuildersBuildSpeed(selectedBuilders)
  local totalSpeed = 0

  for _, unitID in pairs(selectedBuilders) do
    local targetID = builders[unitID].targetID
    if not targetID or not isAlreadyInTable(targetID, selectedBuilders) then
      totalSpeed = totalSpeed + builders[unitID].buildSpeed
    end
  end

  return totalSpeed
end


function getBuildTimeLeft(unitID)

  -- local health, maxHealth, paralyzeDamage, capture, build = GetUnitHealth(unitID)
  local _, _, _, _, build = GetUnitHealth(unitID)

  -- local alreadyBuilding = {}
  local currentBuildSpeed = 0
  -- local releasedExpenditures = getSelectedUnitsUpkeep()  --{metal = 0, energy = 0}

  for builderID, _ in pairs(builders) do
    local targetID = GetUnitIsBuilding(builderID)
    if targetID == unitID and builderID ~= unitID then
      -- alreadyBuilding[builderID] = builderID
      currentBuildSpeed = currentBuildSpeed + builders[builderID].originalBuildSpeed

      -- if selectedBuilders[builderID] or isAlreadyInTable(builderID, selectedBuilders) then
        -- selectedBuilders[builderID] = nil
      -- end
    end
  end

  local unitDef = UnitDefs[GetUnitDefID(unitID)]

  local buildLeft = (1 - build) * unitDef.buildTime

  local time =  buildLeft / currentBuildSpeed

  return time
end


function getTargetsBuild(unitID)
  local targetID = builders[unitID].targetID
  if targetID then
    return getTargetsBuild(targetID)
  else
    return GetUnitIsBuilding(unitID)
  end
end



function getUnitsBuildingUnit(unitID)
  local building = {}

  for builderID, _ in pairs(builders) do
    local targetID = GetUnitIsBuilding(builderID)
    if targetID == unitID then
      building[builderID] = builderID
    end
  end

  return building
end


function isBuilder(unitDef)
  if (unitDef.isBuilder and unitDef.canAssist) or unitDef.isFactory then
    return true
  else
    return false
  end
end



function widget:GameFrame(n)
  if n % 5 == 0 then
    for builderID, _ in pairs(builders) do

      -- fast forward
      local cmdQueue = GetUnitCommands(builderID, 3);
      if cmdQueue and #cmdQueue > 2 and cmdQueue[3].id < 0 then
        -- next command is build command
        local targetID = GetUnitIsBuilding(builderID)
        if targetID then
          -- target id recieved
          if not abandonedTargetIDs[targetID] then
            -- target has not previously been abandoned
            previousBuilding = builders[builderID].previousBuilding
            if not previousBuilding then
              -- this is the first building in the queue
              -- t0 = Spring.GetTimer()
              -- totalSavedTime = 0
              doFastForwardDecision(builderID, cmdQueue[1].tag, cmdQueue[2].tag, targetID)
            else
              local _, _, _, _, prevBuild = GetUnitHealth(previousBuilding)
              local _, _, _, _, currBuild = GetUnitHealth(targetID)
              if prevBuild == nil or prevBuild == 1 then
                -- previous building is done
                doFastForwardDecision(builderID, cmdQueue[1].tag, cmdQueue[2].tag, targetID)
              -- elseif (currBuild ~= nil and currBuild < 1) and isWithinBuildRange(builderID, previousBuilding) then
              --   -- current building has been started but previous is not, and it is within range
              --   GiveOrderToUnit(builderID, CMD.INSERT, {0, CMD.REPAIR, previousBuilding}, {"ctrl"})
              -- end
            end
          end
        end
      end

      -- mm/e switcher
      targetID = GetUnitIsBuilding(builderID)
      if (targetID) then
        targetMM, targetE = getResourceProperties(targetID)
        hasUnusedMMs, isEnergyStalling, isEnergyLeaking = getResourceStatus()

        -- log(targetMM, targetE, hasUnusedMMs, isEnergyStalling, isEnergyLeaking)
        if (targetMM >= 0 and hasUnusedMMs) or (targetE < 0 and not isEnergyStalling) then
          -- v('energy', builderID)
        elseif targetE > 0 and hasUnusedMMs then
          -- builderForceResourceAssist('metalmakers', builderID)
        end


      end
    end
  end


  if n % 1000 == 0 then
    for k,v in pairs(abandonedTargetIDs) do
      -- this probably doesn't work at all since
      local _, _, _, _, build = GetUnitHealth(k)
      if build == nil or build == 1 then
        table.remove(abandonedTargetIDs, k)
      end
    end
  end
end

function isWithinBuildRange(builderID, previousBuilding)
  if UnitDefs[GetUnitDefID(builderID)].buildDistance >

  return false
end

function getResourceStatus()
  mm_level = Spring.GetTeamRulesParam(myTeamID, 'mmLevel')
  e_curr, e_max, e_pull, e_inc, e_exp = Spring.GetTeamResources(myTeamID, 'energy')

  -- log('e_pull '.. e_pull .. ' e_inc ' .. e_inc .. ' e_exp ' ..  e_exp)
  isPositiveEnergyDerivative = e_inc > (e_pull+e_exp)/2
  energyLevel = e_curr/e_max/(9001/9001)

  return e_curr/e_max < mm_level*1.06, energyLevel > 0.99 and not isPositiveEnergyDerivative, energyLevel < 0.01 and isPositiveEnergyDerivative
end

function getResourceProperties(unitID)
  local unitDefID = GetUnitDefID(targetID)
  local unitDef = UnitDefs[unitDefID]

  local metalMakingEfficiency = getMetalMakingEfficiency(unitDefID)

  local energyMaking = getEout(unitDef)
  return metalMakingEfficiency, energyMaking
end

function getMetalMakingEfficiency(unitDefID)
  if WG.energyConversion.convertCapacities[unitDefID] then
    return WG.energyConversion.convertCapacities[unitDefID].e
  else
    return 0
  end
end

function getEout(unitDef )
  local totalEOut = unitDef.energyMake or 0

  -- if negsolar[unitDef.name] then
      -- totalEOut = totalEOut + math.abs(unitDef.energyUpkeep)
  -- end

  totalEOut = totalEOut + -1*unitDef.energyUpkeep

  if unitDef.tidalGenerator > 0 and tidalStrength > 0 then
      local mult = 1 -- DEFAULT
      if unitDef.customParams then
          mult = unitDef.customParams.energymultiplier or mult
      end
      totalEOut = totalEOut +(tidalStrength * mult)
  end

  if unitDef.windGenerator > 0 then
      local mult = 1 -- DEFAULT
      if unitDef.customParams then
          mult = unitDef.customParams.energymultiplier or mult
      end

      local unitWindMin = math.min(windMin, unitDef.windGenerator)
      local unitWindMax = math.min(windMax, unitDef.windGenerator)
      totalEOut = totalEOut + (((unitWindMin + unitWindMax) / 2 ) * mult)
  end
  return totalEOut
end

function doFastForwardDecision(builderID, cmdQueueTag, cmdQueueTagg, targetID)
  selectedUnits = GetSelectedUnits()
  local totalBuildSpeed = getBuildersBuildSpeed(getUnitsBuildingUnit(targetID))
  local secondsLeft = getBuildTimeLeft(targetID)
  local unitDef = UnitDefs[GetUnitDefID(targetID)]
  if isTimeToMoveOn(secondsLeft, builderID, unitDef, totalBuildSpeed) and isResourceToMoveOn(secondsLeft, unitDef, totalBuildSpeed) then
    GiveOrderToUnit(builderID, CMD.REMOVE, {i,cmdQueueTag}, {"ctrl"})
    GiveOrderToUnit(builderID, CMD.REMOVE, {i,cmdQueueTagg}, {"ctrl"})
    builders[builderID].previousBuilding = targetID
    abandonedTargetIDs[targetID] = true
  end
end

function isTimeToMoveOn(secondsLeft, builderID, unitDef, totalBuildSpeed)
  local plannerBuildSpeed = builders[builderID].originalBuildSpeed
  local plannerBuildShare = plannerBuildSpeed / totalBuildSpeed
  local unslowness = 45/unitDef.speed
  -- log("plannerBuild calc " .. plannerBuildShare .. " = " .. plannerBuildSpeed .. " / " .. totalBuildSpeed)
  if ((plannerBuildShare < 0.75 and secondsLeft < 1.2*unslowness) or (plannerBuildShare < 0.5 and secondsLeft < 3.4*unslowness) or (plannerBuildShare < 0.15 and secondsLeft < 10*unslowness) or (plannerBuildShare < 0.05 and secondsLeft < 20*unslowness)) then
    totalSavedTime = totalSavedTime + secondsLeft
    -- log(string.format('Con moved on, %.0f%% buildshare and %.1f sec left, saved %.0f moving sec, lost %.0f con total sec', plannerBuildShare*100, secondsLeft, totalSavedTime, Spring.DiffTimers(Spring.GetTimer(), t0)))
    return true
    -- return false
  else
    return false
  end
end


function isResourceToMoveOn(secondsLeft, unitDef, currentBuildSpeed)


  local speed = unitDef.buildTime / currentBuildSpeed
  local metal = unitDef.metalCost / speed
  local energy = unitDef.energyCost / speed

  local releasedExpenditures = getSelectedUnitsUpkeep()

  calcResourceChange("energy", energy, secondsLeft, releasedExpenditures.energy)
  calcResourceChange("metal", metal, secondsLeft, releasedExpenditures.metal)

  if (willStall == true) then
    return false
  else
    return true
  end

end

function calcResourceChange(type, consumption, secondsLeft, releasedExpenditures)
  local currentChange, lvl, storage, _, alreadyInStall = getMyResources(type)

  local changeWhenBuilding = currentChange - consumption + releasedExpenditures

  if metalMakersControlled and type == "metal" then
    changeWhenBuilding = changeWhenBuilding - releasedMetal
  end

  releasedMetal = 0
  if metalMakersControlled and type == "energy" and possibleMetalMakersUpkeep > 0 then
    local metalMakersUpkeep = getMetalMakersUpkeep()
    if changeWhenBuilding < 0 then
      changeWhenBuilding = changeWhenBuilding + metalMakersUpkeep

      local releasedEnergy = 0
      if changeWhenBuilding > 0 then
        releasedEnergy = changeWhenBuilding
        changeWhenBuilding = 0
      else
        releasedEnergy = metalMakersUpkeep
      end
      releasedMetal = possibleMetalMakersProduction * releasedEnergy / possibleMetalMakersUpkeep
    end
  end

  local after = lvl + secondsLeft * changeWhenBuilding

  if consumption < 1 or (not alreadyInStall and after > 0) then
    willStall = false

    if changeWhenBuilding < 0 then
      if after < 0 then
        after = 0
      end
    else
      if after > storage then
        after = storage
      end
    end

  else
    willStall = true
  end


end



function getMyResources(type)
  local lvl, storage, pull, inc, exp, share, sent , recieved = GetTeamResources(myTeamID, type)

  if not inc then
    Spring.Echo("ERROR", myTeamID, type)
    myTeamID = Spring.GetMyTeamID()
    return
  end

  local total = recieved
  local exp = 0
  local units = GetTeamUnits(myTeamID)

  if type == "metal" then
    for _, unitID in ipairs(units) do
      local metalMake, metalUse, energyMake, energyUse = GetUnitResources(unitID)
      total = total +  metalMake - metalUse
      exp = exp + metalUse
    end
  else
    for _, unitID in ipairs(units) do
      local metalMake, metalUse, energyMake, energyUse = GetUnitResources(unitID)
      total = total +  energyMake - energyUse
      exp = exp + energyUse
    end
  end

  local alreadyInStall = pull > exp and lvl < pull

  return total, lvl, storage, exp, alreadyInStall
end


function getSelectedUnitsUpkeep()
  local alreadyCounted = {}

  local metal = 0
  local energy = 0

  for _, unitID in ipairs(selectedUnits) do
    if builders[unitID] then
      local metalUse, energyUse = traceUpkeep(unitID, alreadyCounted)
      metal = metal + metalUse
      energy = energy + energyUse
    end
  end
  return {["metal"] = metal, ["energy"] = energy}
end


function traceUpkeep(unitID, alreadyCounted)
  if alreadyCounted[unitID] then
    return 0, 0
  end

  local metalMake, metal, energyMake, energy = GetUnitResources(unitID)
  for _, guardID in ipairs(builders[unitID].guards) do
    if builders[guardID].owned then
      local guarderMetal, guarderEnergy = traceUpkeep(guardID, alreadyCounted)
      metal = metal + guarderMetal
      energy = energy + guarderEnergy
    end
  end

  alreadyCounted[unitID] = unitID

  return metal - metalMake + builders[unitID].unitDef.metalMake, energy - energyMake + builders[unitID].unitDef.energyMake
end








-- for logging
function table.val_to_str (v )
  if "string" == type(v ) then
    v = string.gsub(v, "\n", "\\n" )
    if string.match(string.gsub(v,"[^'\"]",""), '^"+$' ) then
      return "'" .. v .. "'"
    end
    return '"' .. string.gsub(v,'"', '\\"' ) .. '"'
  else
    return "table" == type(v ) and table.tostring(v ) or
      tostring(v )
  end
end

function table.key_to_str (k )
  if "string" == type(k ) and string.match(k, "^[_%a][_%a%d]*$" ) then
    return k
  else
    return "[" .. table.val_to_str(k ) .. "]"
  end
end

function table.tostring(tbl )
  local result, done = {}, {}
  for k, v in ipairs(tbl ) do
    table.insert(result, table.val_to_str(v ) )
    done[ k ] = true
  end
  for k, v in pairs(tbl ) do
    if not done[ k ] then
      table.insert(result,
        table.key_to_str(k ) .. "=" .. table.val_to_str(v ) )
    end
  end
  return "{" .. table.concat(result, "," ) .. "}"
end