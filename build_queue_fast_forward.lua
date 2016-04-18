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
local log = Spring.Echo
local t0
local totalSavedTime = 0

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
    if (n % 5 == 0) then

      for builderID, _ in pairs(builders) do
        local cmdQueue = GetUnitCommands(builderID, 3);
        if cmdQueue and #cmdQueue>2 then
          cmdID = cmdQueue[3].id
          if cmdID < 0 then

            local targetID = GetUnitIsBuilding(builderID)
            if targetID then
              previousBuilding = builders[builderID].previousBuilding
              if not previousBuilding then
                t0 = Spring.GetTimer()
                totalSavedTime = 0
                doFastForwardDecision(builderID, cmdQueue[1].tag, cmdQueue[2].tag, targetID)
              else
                local _, _, _, _, build = GetUnitHealth(previousBuilding)
                if build == nil or build == 1 then
                  doFastForwardDecision(builderID, cmdQueue[1].tag, cmdQueue[2].tag, targetID)
                end
              end
            end
          end
        end
      end

    end
end

function doFastForwardDecision(builderID, cmdQueueTag, cmdQueueTagg, targetID)
  selectedUnits = GetSelectedUnits()
  local totalBuildSpeed = getBuildersBuildSpeed(getUnitsBuildingUnit(targetID))
  local secondsLeft = getBuildTimeLeft(targetID)
  if isTimeToMoveOn(secondsLeft, builderID, totalBuildSpeed) and isResourceToMoveOn(secondsLeft, targetID, totalBuildSpeed) then
    builders[builderID].previousBuilding = targetID
    GiveOrderToUnit(builderID, CMD.REMOVE, {i,cmdQueueTag}, {"ctrl"})
    GiveOrderToUnit(builderID, CMD.REMOVE, {i,cmdQueueTagg}, {"ctrl"})
  end
end

function isTimeToMoveOn(secondsLeft, builderID, totalBuildSpeed)
  local plannerBuildSpeed = builders[builderID].originalBuildSpeed
  local plannerBuildShare = plannerBuildSpeed / totalBuildSpeed
  -- log("plannerBuild calc " .. plannerBuildShare .. " = " .. plannerBuildSpeed .. " / " .. totalBuildSpeed)
  if ((plannerBuildShare < 0.75 and secondsLeft < 1.2) or (plannerBuildShare < 0.5 and secondsLeft < 3.4) or (plannerBuildShare < 0.15 and secondsLeft < 10) or (plannerBuildShare < 0.05 and secondsLeft < 20)) then
    totalSavedTime = totalSavedTime + secondsLeft
    log(string.format('Con moved on, %.0f%% buildshare and %.1f sec left, saved %.0f moving sec, lost %.0f con total sec', plannerBuildShare*100, secondsLeft, totalSavedTime, Spring.DiffTimers(Spring.GetTimer(), t0)))
    return true
    -- return false
  else
    return false
  end
end


function isResourceToMoveOn(secondsLeft, targetID, currentBuildSpeed)
  local unitDef = UnitDefs[GetUnitDefID(targetID)]

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


