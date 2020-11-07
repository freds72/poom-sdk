-- background menu
local snd="36530600324c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000060100003e0c3fa445d819c0158b1589158515841583158215811381138003800380538000000000000000000000000000000000000000000000000000000000000000000301000"

-- supports player 1 or player 2 input
local _btnp=btnp
function btnp(b)
  return _btnp(b,0) or _btnp(b,1)
end

-- copy image text to spritesheet/memory
function unpack_gfx(src,rows)
  local addr=rows and 0x4e00 or 0x0
  rows=rows or 0
  for i=0,#src-1 do
    poke(addr,ord(src,i+1))
    addr+=1
    if (addr>0x4e00+64*rows-1) addr=0x0
  end
end

-- game states
function next_state(fn,...)
  local u,d,i=fn(...)
  -- ensure update/draw pair is consistent
  _update=function()
    -- init function (if any)
    if(i) i()
    -- 
    _update,_draw=u,d
    -- actually run the update
    u()
  end
end

-- intro screen
function start_state()
  -- reset saved state
  dset(0,-1)

  return
    -- update
    function()    
      if btnp()!=0 then
        next_state(menu_state)
      end  
    end,
    -- draw
    function()
      cls()
      pal()
      memcpy(0x6000,0x4e00,64*13)
      spr(0,0,13,16,15)    
      printb("@fsouchu",2,121,vcol(4))
      printb("@gamecactus",83,121,vcol(4))

      pal(title_gfx.pal,1)
    end,
    -- init
    function()
      -- reset any bitplane masking
      poke(0x5f5e,0xff)

      unpack_gfx(title_gfx.bytes,13)
    end  
end

function menu_state()
  local menus,menu_i,anm_ttl={
    {"wHICH ePISODE?",_maps_label,sel=1,max=1},
    {"sELECT sKILL lEVEL",
    {
      "i AM TOO YOUNG TO DIE",
      "hEY, NOT TOO ROUGH",
      "hURT ME PLENTY",
      "uLTRA-vIOLENCE"
    },sel=2,max=2}},1,0

  -- read max level reached
  local max_map_id=dget(32)
  if(max_map_id>#_maps_label) max_map_id=#_maps_label dset(32,max_map_id)
  if(max_map_id<=0) max_map_id=1 dset(32,max_map_id)

  menus[1].max=max_map_id
  menus[2].max=#menus[2][2]

  return
    function()
      anm_ttl=(anm_ttl+1)%48
      if menu_i>0 then
        local active_sel=menus[menu_i].sel
        if btnp(2) then
          active_sel-=1
          sfx(0)
        end
        if btnp(3) then
          active_sel+=1
          sfx(0)
        end
        -- unlocked?
        menus[menu_i].sel=mid(active_sel,1,menus[menu_i].max)
      end
      if btnp(5) then
        if(menu_i>1)sfx(0)
        menu_i=max(1,menu_i-1)
      elseif btnp(4) then
        if(menu_i>0)sfx(1)
        menu_i+=1
        if menu_i>#menus then
          next_state(launch_state,menus[2].sel,menus[1].sel)
        end
      end
    end,
    function()
      -- exit early because state will have been change to launch_state
      if (menu_i>#menus)return

      cls()
      memcpy(0x6000,0x4e00,64*13)
      spr(0,0,13,16,15)    
      printb("@fsouchu",2,121,vcol(4))
      printb("@gamecactus",83,121,vcol(4))

      -- dark menu background
      -- todo: fix
      for i=0,15 do
        pal(vcol(i),sget(112+i,129-13))
        --pset(i,0,i)
      end
      sspr(12,51,104,19+#menus[menu_i][2]*9,12,64)
      pal()
      
      -- title
      printb(menus[menu_i][1],63-#menus[menu_i][1]*2,69,vcol(14))

      -- selection marker
      rectfill(18,70+menus[menu_i].sel*9,113,79+menus[menu_i].sel*9,vcol(2))
      palt(vcol(0),false)
      palt(vcol(4),true)
      sspr(anm_ttl\12*10,115,11,12,14,68+menus[menu_i].sel*9)
      palt()

      -- menu items
      for i=1,#menus[menu_i][2] do
        local s=menus[menu_i][2][i]
        if(i>menus[menu_i].max) s=masked(s)
        printb(s,28,72+i*9,i<=menus[menu_i].max and vcol(4) or vcol(3))
      end
      
      pal(title_gfx.pal,1)
    end,
    -- init
    function()
      unpack_gfx(title_gfx.bytes,13)
    end  
end

function fadetoblack_state(...)
  local args,fade_ttl={...},32
  return 
    function()
      fade_ttl-=1
      if fade_ttl<0 then
        next_state(unpack(args))
      end
    end,
    function()
      if fade_ttl>=0 then
        memcpy(0x5f00,0x4300|(fade_ttl\4)<<4,16)
        spr(0,0,0,16,16)
      end
    end,
    function()
      memcpy(0x0,0x6000,127*64)
      memcpy(0x5f10,0x4400,16)
    end
end

function stats_state(skill,id,level_time,kills,monsters,secrets,all_secrets)
  local ttl,msg_ttl,max_msg=0,0,2
  local msgs={
    "completed:",
    _maps_label[id],
    "time: "..time_tostr(level_time),
    "kills: "..kills.."/"..monsters,
    all_secrets>0 and "secrets: "..secrets.."/"..all_secrets or nil
  }

  return
    function()
      if ttl>600 or btnp(4) or btnp(5) then
        next_state(launch_state,skill,id+1)
      end
      ttl+=1
      msg_ttl+=1
      if msg_ttl>15 and max_msg<#msgs then
        sfx(0)
        max_msg+=1
        msg_ttl=0
      end
    end,
    function()   
      local y=40   
      for i=1,max_msg do
        local s=msgs[i]
        printb(s,63-#s*2,y,15)
        y+=10
      end
    end
end

function launch_state(skill,id,level_time,kills,secrets)
  -- record max level reached so far
  if(id>dget(32)) dset(32,id)
  return
    function()
      -- delay to allow _draw to be called to prevent palette reset on load
      if launch_ttl==0 then
        -- stop all sfx to prevent audio glitch
        for i=0,3 do
          sfx(-1,i)
        end
        load(_maps_group[id]..".p8",nil,skill..","..id)
      end
      launch_ttl-=1
    end,
    function()
      cls()
      spr(0,0,0,16,16)
      pal(loading_gfx.pal,1)
      local s="eNTERING ".._maps_label[id]
      printb(s,63-#s*2,80,15)
    end,
    function()
      unpack_gfx(loading_gfx.bytes)
    end
end


function credits_state()  
  local ttl=0
  return
    -- update
    function()
      if ttl>3000 or btnp(4) or btnp(5) then
        -- back to startup screen
        next_state(start_state)
      end

      ttl+=0.25
    end,
    -- draw
    function()
      -- doom fire!
      -- credits: https://fabiensanglard.net/doom_fire_psx/index.html

      -- draw 'fire plane'
      poke(0x5f5e,0x77)
      for x=0,127 do
        for y=127,100,-1 do
          local c=pget(x,y)
          -- decay
          pset((x+rnd(2)-1)&127,y-1,rnd()>0.5 and min(c+1,7) or c)
        end
      end
      -- draw 'text plane'
      poke(0x5f5e,0x88)      
      rectfill(0,0,127,127,0)
      local y=128-ttl
      pal(1,8)
      sspr(0,0,128,13,0,y,128,13)
      pal()
      y+=128
      for i,t in ipairs(_credits) do
        print(t,64-#t*2,y,8)
        y+=7
      end
      pal({[0]=0,7,10,9,8,2,1,0,8,10,9,8,2,8,2,8},1)
    end,
    function()
      cls()
      pal()
      -- seed
      memset(0x7fc0,0x11,64)
      -- 
      unpack_gfx(endgame_gfx.bytes)
    end
end

function slicefade_state(...)
  local args,ttl,r,h,rr=pack(...),30,{},{},0
  for i=0,127 do
    rr=lerp(rr,rnd(0.2),0.1)
    r[i],h[i]=0.05+rr,0
  end
  return 
    -- update
    function()
      ttl-=1
      if ttl<0 or btnp(4) or btnp(5) then
        next_state(unpack(args))
      end
    end,
    -- draw
    function()
      local src,mem=loading_gfx.bytes,0x6000
      for i=0,#src-1 do
        poke(mem+i,ord(src,i+1))
      end

      for i,r in pairs(r) do
        h[i]=lerp(h[i],129,r)
        sspr(i,0,1,128,i,h[i],1,128)
      end
      pal(loading_gfx.pal,1)
    end,
    -- init
    function()
      -- copy screen to spritesheet
      memcpy(0x0,0x6000,8192)
    end
end

function _init()
  cartdata(mod_name)

  -- sound effects
  local addr=0x3200
  for i=0,#snd\4-1 do
    poke2(addr+i*2,tonum("0x"..sub(snd,i*4+1,i*4+4)))
  end

  local p=split(stat(6))  
  local skill,mapid,state,level_time,kills,monsters,secrets=tonum(p[1]) or 2,tonum(p[2]) or 1,tonum(p[3]) or 0,tonum(p[4]) or 0,tonum(p[5]) or 0,tonum(p[6]) or 0,tonum(p[7]) or 0
  
  -- special case for end game state
  if(state==2 and mapid+1>#_maps_group) state=4

  local states={
    -- default
    [0]={start_state},
    -- 1: game over
    {fadetoblack_state,start_state},
    -- 2: next level
    {slicefade_state,stats_state,skill,mapid,level_time,kills,monsters,secrets,_secrets[mapid]},
    -- 3: retry
    {slicefade_state,launch_state,skill,mapid},    
    -- 4: end game
    {fadetoblack_state,credits_state}
  }

  -- wait time before launching (15 frames when loading from menu to prevent audio from getting cut too short)
  launch_ttl=(state==2 or state==3) and 1 or 15

  next_state(unpack(states[state]))  
end

-->8
-- helper functions
function printb(s,x,y,c,bgc)
  bgc=bgc or 0
  print(s,x,y+1,bgc)
  print(s,x,y,c)
end

function padding(n)
	n=tostr(min(n,99)\1)
	return sub("00",1,2-#n)..n
end

function masked(s)
  local q=""
  for i=1,#s do
    local c=sub(s,i,i)
    q=q..(c==" " and c or "?")
  end
  return q
end

-- frames per sec to human time (mm'ss''zzz')
function time_tostr(t)
	return padding((t\60)%60).."'"..padding(t%60).."''"..padding((t&0x0.ffff)*100)
end

function lerp(a,b,t)
  return a+t*(b-a)
end

-- color lookup from title image
function vcol(c)
  return sget(112+c,128-13)
end