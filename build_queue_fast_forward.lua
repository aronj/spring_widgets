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

local GetAllyTeamList = Spring.GetAllyTeamList
local GetMyTeamID = Spring.GetMyTeamID
local GetSelectedUnits = Spring.GetSelectedUnits
local GetTeamResources = Spring.GetTeamResources
local GetTeamResources = Spring.GetTeamResources
local GetTeamRulesParam = Spring.GetTeamRulesParam
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

local abandonedTargetIDs = {}
local builders = {}
local commanderBuildSpeed = 100
local conversionLevelHistory = {}
local log = Spring.Echo
local mainIterationModuloLimit = 5
local regularizedResourceDerivativesMetal = {1}
local regularizedResourceDerivativesEnergy = {1}
local metalMakers = {}
local myTeamId = Spring.GetMyTeamID()
local possibleMetalMakersProduction = 0
local possibleMetalMakersUpkeep = 0
local releasedMetal = 0
local selectedUnits
local t0 = Spring.GetTimer()
local tidalStrength = Game.tidal
local totalSavedTime = 0
local willStall = false
local windMax = Game.windMax
local windMin = Game.windMin
local regularizedPositiveMetal = true
local regularizedPositiveEnergy = true
local regularizedNegativeMetal = false
local regularizedNegativeEnergy = false
--local metalLevel = 0
--local positiveMMLevel = 0

local positiveMMLevel = 0
local tooLittleMMs = true
local tooMuchMMs = false
local metalLevel = 0.5
local isPositiveMetalDerivative = false
local energyLevel = 0.5
local isPositiveEnergyDerivative = false
local isEnergyStalling = false
local isMetalLeaking = true
local isEnergyLeaking = true


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

    local myUnits = GetTeamUnits(myTeamId)
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

  if unitDef.isBuilder and unitDef.canAssist then

    builders[unitID] = {["buildSpeed"] = unitDef.buildSpeed, originalBuildSpeed = unitDef.buildSpeed, ['unitDef'] = unitDef, ["targetId"] = nil, ["guards"] = {},
      ['previousBuilding'] = nil}
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
  if unitTeam == myTeamId then
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
    local targetId = builders[unitID].targetId
    if not targetId or not isAlreadyInTable(targetId, selectedBuilders) then
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

  for builderId, _ in pairs(builders) do
    local targetId = GetUnitIsBuilding(builderId)
    if targetId == unitID and builderId ~= unitID then
      -- alreadyBuilding[builderId] = builderId
      currentBuildSpeed = currentBuildSpeed + builders[builderId].originalBuildSpeed

      -- if selectedBuilders[builderId] or isAlreadyInTable(builderId, selectedBuilders) then
        -- selectedBuilders[builderId] = nil
      -- end
    end
  end

  local unitDef = UnitDefs[GetUnitDefID(unitID)]

  local buildLeft = (1 - build) * unitDef.buildTime

  local time =  buildLeft / currentBuildSpeed

  return time
end


function getTargetsBuild(unitID)
  local targetId = builders[unitID].targetId
  if targetId then
    return getTargetsBuild(targetId)
  else
    return GetUnitIsBuilding(unitID)
  end
end


function getUnitsBuildingUnit(unitID)
  local building = {}

  for builderId, _ in pairs(builders) do
    local targetId = GetUnitIsBuilding(builderId)
    if targetId == unitID then
      building[builderId] = builderId
    end
  end

  return building
end


function widget:GameFrame(n)
  if n % mainIterationModuloLimit == 0 then
    builderIteration(n)
  end

  -- if t1 then
  --   log(string.format('%.1f seconds since last action', Spring.DiffTimers(Spring.GetTimer(), t1)))
  --   if Spring.DiffTimers(Spring.GetTimer(), t1) > 30 then
  --     -- log(Spring.DiffTimers(Spring.GetTimer(), t1))
  --     t0 = Spring.GetTimer()
  --   end
  -- end

  -- if n % 96 == 0 then
    -- log(string.format('%.1f seconds from t0', Spring.DiffTimers(Spring.GetTimer(), t0)))
  -- end

  if n % 1000 == 0 then
    for k, v in pairs(abandonedTargetIDs) do
      local _, _, _, _, build = GetUnitHealth(k)
      if build == nil or build == 1 then
        table.remove(abandonedTargetIDs, k)
      end
    end
  end
end

function builderIteration(n)
  for builderId, _ in pairs(builders) do
    local targetId = GetUnitIsBuilding(builderId)
    local cmdQueue = GetUnitCommands(builderId, 3)

    -- dont wait
    if cmdQueue and #cmdQueue > 0 and cmdQueue[1].id == 5 and (isMetalLeaking or isEnergyLeaking) then
      GiveOrderToUnit(builderId, CMD.REMOVE, {nil}, {"ctrl"})
    end

    if targetId then
      -- target id recieved

      local builderDef = UnitDefs[GetUnitDefID(builderId)]
      local targetDefID = GetUnitDefID(targetId)
      local targetDef = UnitDefs[targetDefID]

      if n % mainIterationModuloLimit * 30 == 0 then
        table.insert(regularizedResourceDerivativesMetal, 1, isPositiveMetalDerivative)
        table.insert(regularizedResourceDerivativesEnergy, 1, isPositiveEnergyDerivative)
        if table.getn(regularizedResourceDerivativesMetal) > 7 then
          table.remove(regularizedResourceDerivativesMetal)
          table.remove(regularizedResourceDerivativesEnergy)
        end
        regularizedPositiveMetal = table.full_of(regularizedResourceDerivativesMetal, true)
        regularizedPositiveEnergy = table.full_of(regularizedResourceDerivativesEnergy, true)
        regularizedNegativeMetal = table.full_of(regularizedResourceDerivativesMetal, false)
        regularizedNegativeEnergy = table.full_of(regularizedResourceDerivativesMetal, false)
      end
      updateFastResourceStatus()

      if regularizedPositiveMetal then
        -- log('m+')
      end
      if regularizedPositiveEnergy then
        -- log('e+')
      end

      -- queue fast forwarder
      if cmdQueue then

        cmdQueue = purgeCompleteRepairs(builderId, cmdQueue)

        if #cmdQueue > 2 and cmdQueue[3].id < 0 then
          -- next command is build command
          if not abandonedTargetIDs[targetId] then
            -- target has not previously been abandoned
            local previousBuilding = builders[builderId].previousBuilding
            if not previousBuilding then
              -- this is the first building for builder
              -- t0 = Spring.GetTimer()
              -- totalSavedTime = 0
              doFastForwardDecision(builderId, targetId, cmdQueue[1].tag, cmdQueue[2].tag)

            else
              local _, _, _, _, prevBuild = GetUnitHealth(previousBuilding)
              local _, _, _, _, currBuild = GetUnitHealth(targetId)
              if prevBuild == nil or prevBuild == 1 then
                -- previous building is gone/done
                doFastForwardDecision(builderId, targetId, cmdQueue[1].tag, cmdQueue[2].tag)
              end
            end
          end
        end
      end


      -- prepare outside command queue heuristical candidates/targets
      local mpx, _, mpz = GetUnitPosition(builderId, true)
      local neighbours = GetUnitsInCylinder(mpx, mpz, builderDef.buildDistance, myTeamId)

      local candidateNeighbours = {}
      for i, candidateId in ipairs(neighbours) do
        local _, _, _, _, candidateBuild = GetUnitHealth(candidateId)
        if candidateBuild ~= nil and candidateBuild < 1 and candidateId ~= builderId then
          table.insert(candidateNeighbours, candidateId)
        end
      end


      -- mm/e switcher
      local targetUnitMM, targetUnitE = getUnitResourceProperties(targetDefID, targetDef)



      -- log(targetMM, targetE, tooMuchMMs, isEnergyStalling, isEnergyLeaking)
      -- m is increasing, e is not decreasing
      -- if metalLevel > 0.5 and regularizedPositiveMetal and not regularizedNegativeEnergy then
      --   log('find buildpower unit')
      --   builderForceAssist(0, builderId, targetDef, neighbours, targetMM, targetE)
      -- end

      -- not building buildpower unit or
      -- or building buildpower unit, m not increasing
      -- if unitDef.buildpower <= 0 or (unitDef.buildpower > 0 and not regularizedPositiveMetal) then
        -- e is not increasing, building a mm, under mm level
        -- e is not increasing, building a non-e building
--          log('cmdQueue')
--          log(table.tostring(cmdQueue))
--      log('force assist conditions e')
      --[[log(table.tostring({
      not regularizedPositiveEnergy,
          not isEnergyLeaking,
          '(',
          targetUnitMM >= 0,
          not positiveMMLevel,
          'or',
          targetUnitE < 0,
          isEnergyStalling,
          ')',
--          targetUnitMM >= 0 and not positiveMMLevel,
--          targetUnitE < 0 and isEnergyStalling,
        }))
      log('force assist conditions m')
      log(table.tostring({
        targetUnitE > 0 ,positiveMMLevel ,regularizedPositiveEnergy
      }))
]]
      -- refresh for possible target change
      targetId = GetUnitIsBuilding(builderId)

      if n % (mainIterationModuloLimit * 3) == 0 then
        if not regularizedPositiveEnergy and not isEnergyLeaking and ((targetUnitMM >= 0 and not positiveMMLevel) or (targetUnitE < 0 and isEnergyStalling)) then
          builderForceAssist('energy', builderId, targetId, targetDef, candidateNeighbours, targetUnitMM, targetUnitE)
        -- building e building,
        elseif targetUnitE > 0 and positiveMMLevel and regularizedPositiveEnergy then
          builderForceAssist('mm', builderId, targetId, targetDef, candidateNeighbours, targetUnitMM, targetUnitE)
        end
      end
      -- end

      -- refresh for possible target change
      targetId = GetUnitIsBuilding(builderId)

      -- easy finish neighbour
      local _, _, _, _, targetBuild = GetUnitHealth(targetId)
      for _, candidateId in ipairs(candidateNeighbours) do
        local candidateDef = unitDef(candidateId)
        -- same type and not actually same buildings
        if candidateId ~= targetId and candidateDef == targetDef then
          local _, _, _, _, candidateBuild = GetUnitHealth(candidateId)
          if candidateBuild and candidateBuild < 1 and candidateBuild > targetBuild then
            local targetBuildTimeLeft = getBuildTimeLeft(targetId)
            -- candidate is better
--            if targetBuildTimeLeft*2 > 0.35 and candidateBuild > targetBuild then
--            log('candidateBuild')
--            log(candidateBuild)
--            log('targetBuild')
--            log(targetBuild)
            if candidateBuild > targetBuild then
              repair(builderId, candidateId)
              break
            end
          end
        end
      end

    end
  end
end




function purgeCompleteRepairs(builderId, cmdQueue)
  local cmdq = deepcopy(cmdQueue)
  local shitFound = true
  while shitFound do
    shitFound = false
--    log('while shit ')
--    log(shitFound)
--    log(table.tostring(cmdQueue))
    for _, cmd in ipairs(cmdQueue) do
      if cmd.id == 40 then
--        log('shit?')
        local _, _, _, _, targetBuild = GetUnitHealth(cmd.params[1])
--        log(targetBuild)
        if not targetBuild or targetBuild == 1 then
          shitFound = true
--          log('removing cmd '.. cmd.tag)
          GiveOrderToUnit(builderId, CMD.REMOVE, {cmd.tag}, {"ctrl"})
        end
      end
    end
    cmdq = GetUnitCommands(builderId, 3)
    break
  end
  return cmdq
end


function builderForceAssist(assistType, builderId, targetId, targetDef, neighbours, targetMM, targetE)
  foundBuildPowerUnit = false
  if (metalLevel > 0.8 or regularizedPositiveMetal) and (positiveMMLevel or not regularizedNegativeEnergy) then
    -- log('looking for buildpower')
    for _, candidateId in ipairs(neighbours) do
      local _, _, _, _, candidateBuild = GetUnitHealth(candidateId)
      if candidateBuild ~= nil and candidateBuild < 1 then

        local candidateDefID = GetUnitDefID(candidateId)
        local candidateDef = UnitDefs[candidateDefID]
        if candidateDef.buildSpeed ~= nil then
          -- log('build ' .. candidateBuild .. ' buildpower ' .. candidateDef.buildSpeed)
        end
        if candidateDef.buildSpeed ~= nil and candidateDef.buildSpeed > 0 then
          GiveOrderToUnit(builderId, CMD.INSERT, {0, CMD.REPAIR, CMD.OPT_CTRL, candidateId}, {"alt"})
          foundBuildPowerUnit = true
          -- log('found buildpower')
          break
        end
      end
    end
  elseif targetDef.buildSpeed > 0 then
    -- log('maybe switch from buildpower building')
    local cmdQueue = GetUnitCommands(builderId, 3);
--    if cmdQueue and #cmdQueue > 2 then
--      local _, _, _, _, targetBuild = GetUnitHealth(targetId)
      -- log('queue ids ', cmdQueue[1].id, cmdQueue[2].id, cmdQueue[3].id, 'target', targetId, targetBuild)
--    end
    if cmdQueue and #cmdQueue > 2 and cmdQueue[2].id < 0 then
      -- log('continue on build queue instead of buildpower unit')
      moveOnFromBuilding(builderId, targetId, cmdQueue[1].tag)
    end
  end

  if foundBuildPowerUnit == false then
    local bestCandidate = getBestCandidate(neighbours, assistType, targetE, targetMM)

    if bestCandidate ~= false and bestCandidate ~= targetId then
--      log('bestCandidate '..bestCandidate)
      repair(builderId, bestCandidate)
    end

  end
end

function repair(builderId, targetId)
  GiveOrderToUnit(builderId, CMD.INSERT, {0, CMD.REPAIR, CMD.OPT_CTRL, targetId}, {"alt"})
end

function getBestCandidate(candidatesOriginal, assistType, targetE, targetMM)
  if #candidatesOriginal == 0 then
    return false
  end
  local candidates = deepcopy(candidatesOriginal)

  for i, candidateId in ipairs(candidates) do
    local cdefid = GetUnitDefID(candidateId)
--    log(table.tostring(UnitDefs[cdefid]))
    candidates[i] = {candidateId, cdefid, UnitDefs[cdefid]}
  end

  if assistType == 'energy' then
    table.sort(candidates, function(a,b)
      local aWillStall = buildingWillStall(a[1])
      local bWillStall = buildingWillStall(b[1])
--      log(a[3]['humanName']..' will stall')
--      log(aWillStall)
--      log(b[3]['humanName']..' will stall')
--      log(bWillStall)
      if aWillStall and bWillStall then
--        log('aWillStall and bWillStall')
--        log(table.tostring({
--          a[3]['humanName'],
--          a[3]['energyMake'],
--          a[3]['power'],
--          b[3]['humanName'],
--          b[3]['energyMake'],
--          b[3]['power'],
--        }))
        return a[3]['energyMake'] / a[3]['buildTime'] / a[3]['power'] > b[3]['energyMake'] / b[3]['buildTime'] / b[3]['power']
      elseif aWillStall and not bWillStall and a[3]['energyMake'] > 0 then
--        log('aWillStall and not bWillStall ')
--        log(table.tostring({
--          a[3]['humanName'],
--          a[3]['energyMake'],
--          a[3]['power'],
--          b[3]['humanName'],
--          b[3]['energyMake'],
--          b[3]['power'],
--        }))
        return false
      elseif not aWillStall and bWillStall and b[3]['energyMake'] > 0 then
--        log('not aWillStall and bWillStall')
--        log(table.tostring({
--          a[3]['humanName'],
--          a[3]['energyMake'],
--          a[3]['power'],
--          b[3]['humanName'],
--          b[3]['energyMake'],
--          b[3]['power'],
--        }))
        return true
      else
--        log('no stall or no e')
        return a[3]['energyMake'] / a[3]['power'] > b[3]['energyMake'] / b[3]['power']
      end
    end)
  elseif assistType == 'mm' then
    table.sort(candidates, function(a,b) return getMetalMakingEfficiency(a[2]) < getMetalMakingEfficiency(b[2]) end)
  elseif assistType == 'metal' then
    return false
  end
--  log('sending candidate ' .. candidates[1][3]['humanName'])
  return candidates[1][1]
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


function updateFastResourceStatus()
  mm_level = GetTeamRulesParam(myTeamId, 'mmLevel')
  m_curr, m_max, m_pull, m_inc, m_exp = GetTeamResources(myTeamId, 'metal')
  e_curr, e_max, e_pull, e_inc, e_exp = GetTeamResources(myTeamId, 'energy')

  isPositiveMetalDerivative = m_inc > (m_pull+m_exp)/2
  metalLevel = m_curr/m_max

  -- log('e_pull '.. e_pull .. ' e_inc ' .. e_inc .. ' e_exp ' ..  e_exp)
  isPositiveEnergyDerivative = e_inc > (e_pull+e_exp)/2
  energyLevel = e_curr/e_max

  tooLittleMMs = energyLevel > mm_level*1.1
  tooMuchMMs = energyLevel < mm_level*0.9
  if energyLevel >= mm_level then
    positiveMMLevel = true
    else
    positiveMMLevel = false
  end

--  isMetalStalling = metalLevel < 0.01 and not regularizedPositiveMetal
  isEnergyStalling = energyLevel < 0.01 and not regularizedPositiveEnergy
  isMetalLeaking = metalLevel > 0.99 and regularizedPositiveMetal
  isEnergyLeaking = energyLevel > 0.99 and isPositiveEnergyDerivative

end

function getUnitResourceProperties(unitDefID, unitDef)
  local metalMakingEfficiency = getMetalMakingEfficiency(unitDefID)
  if metalMakingEfficiency == nil then
    metalMakingEfficiency = 0
  end
  local energyMaking = getEout(unitDef)
  return metalMakingEfficiency, energyMaking
end

function getMetalMakingEfficiency(unitDefID)
  makerDef = WG.energyConversion.convertCapacities[unitDefID]
  if makerDef ~= nil then
    return makerDef.e
  else
    return 0
  end
end

function getEout(unitDef)
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

function getTraveltime(unitDef, A, B)
  selectedUnits = GetSelectedUnits()
  local totalBuildSpeed = getBuildersBuildSpeed(getUnitsBuildingUnit(targetId))
  local secondsLeft = getBuildTimeLeft(targetId)
  local unitDef = UnitDefs[GetUnitDefID(targetId)]
  if isTimeToMoveOn(secondsLeft, builderId, unitDef, totalBuildSpeed) and not buildingWillStall(secondsLeft, unitDef, totalBuildSpeed) then
    moveOnFromBuilding(builderId, targetId, cmdQueueTag, cmdQueueTagg)
  end
end

function doFastForwardDecision(builderId, targetId, cmdQueueTag, cmdQueueTagg)
  selectedUnits = GetSelectedUnits()
  local totalBuildSpeed = getBuildersBuildSpeed(getUnitsBuildingUnit(targetId))
  local secondsLeft = getBuildTimeLeft(targetId)
  local unitDef = UnitDefs[GetUnitDefID(targetId)]
--  log('isTimeToMoveOn(secondsLeft, builderId, unitDef, totalBuildSpeed)')
--  log(isTimeToMoveOn(secondsLeft, builderId, unitDef, totalBuildSpeed))
--  log('buildingWillStall(targetId)')
--  log(buildingWillStall(targetId))
  if isTimeToMoveOn(secondsLeft, builderId, unitDef, totalBuildSpeed) and not buildingWillStall(targetId) then
    moveOnFromBuilding(builderId, targetId, cmdQueueTag, cmdQueueTagg)
  end
end

function moveOnFromBuilding(builderId, targetId, cmdQueueTag, cmdQueueTagg)
  if not cmdQueueTagg then
    GiveOrderToUnit(builderId, CMD.REMOVE, {cmdQueueTag}, {"ctrl"})
  else
    GiveOrderToUnit(builderId, CMD.REMOVE, {cmdQueueTag,cmdQueueTagg}, {"ctrl"})
  end
  -- log(builderId .. ' moved on from ' .. targetId)
  builders[builderId].previousBuilding = targetId
  abandonedTargetIDs[targetId] = true
  -- log('prev ' .. targetId, builders[builderId].previousBuilding)
  t1 = Spring.GetTimer()
end

function isTimeToMoveOn(secondsLeft, builderId, unitDef, totalBuildSpeed)
  local plannerBuildSpeed = builders[builderId].originalBuildSpeed
  local plannerBuildShare = plannerBuildSpeed / totalBuildSpeed
  local slowness = 45/unitDef.speed
  -- log("plannerBuild calc " .. plannerBuildShare .. " = " .. plannerBuildSpeed .. " / " .. totalBuildSpeed)
  if ((plannerBuildShare < 0.75 and secondsLeft < 1.2*slowness) or (plannerBuildShare < 0.5 and secondsLeft < 3.4*slowness) or (plannerBuildShare < 0.15 and secondsLeft < 8*slowness) or (plannerBuildShare < 0.05 and secondsLeft < 12*slowness)) then
    totalSavedTime = totalSavedTime + secondsLeft
    -- log(string.format('Con moved on, %.0f%% buildshare and %.1f sec left, saved %.0f moving sec, lost %.0f con total sec', plannerBuildShare*100, secondsLeft, totalSavedTime, Spring.DiffTimers(Spring.GetTimer(), t0)))
    return true
  else
    return false
  end
end

function buildingWillStall(unitId)
  local secondsLeft = getBuildTimeLeft(unitId)
  local unitDef = unitDef(unitId)
  local speed = unitDef.buildTime / getBuildersBuildSpeed(getUnitsBuildingUnit(unitId))
  local metal = unitDef.metalCost / speed
  local energy = unitDef.energyCost / speed

  local mDrain, eDrain = getUnitsUpkeep()

--  log('willstall? '.. unitDef.humanName .. ' ' ..mDrain .. ' '.. eDrain )

  if buildingWillStallType("metal", metal, secondsLeft, mDrain) or buildingWillStallType("energy", energy, secondsLeft, eDrain) then
    return true
  else
    return false
  end

end

function buildingWillStallType(type, consumption, secondsLeft, releasedExpenditures)
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
--    willStall = false
    return false

--    if changeWhenBuilding < 0 then
--      if after < 0 then
--        after = 0
--      end
--    else
--      if after > storage then
--        after = storage
--      end
--    end

  else
--    willStall = true
    return true
  end


end



function getMyResources(type)
  local lvl, storage, pull, inc, exp, share, sent , recieved = GetTeamResources(myTeamId, type)

  if not inc then
    log("ERROR", myTeamId, type)
    myTeamId = Spring.GetMyTeamID()
    return
  end

  local total = recieved
  local exp = 0
  local units = GetTeamUnits(myTeamId)

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

function getUnitsUpkeep()
  local alreadyCounted = {}

  local metal = 0
  local energy = 0

  for _, unitId in ipairs(GetTeamUnits(myTeamId)) do

    local unitDef = unitDef(unitId)
    if unitDef.canAssist then
      local metalUse, energyUse = traceUpkeep(unitId, alreadyCounted)
      metal = metal + metalUse
      energy = energy + energyUse
    end
  end
  return metal, energy
end

function unitDef(unitId)
  return UnitDefs[GetUnitDefID(unitId)]
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

--  log('table.tostring(builders)')
--  log(unitDef(unitID).humanName)
--  log(unitDef(unitID).isBuilder)
--  log(unitDef(unitID).canAssist)
--  log(unitDef(unitID).isFactory)
--  log(builders[unitID])
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

-- for debug

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
