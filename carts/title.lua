-- background menu
local snd="36530600324c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000060100003e0c3fa445d819c0158b1589158515841583158215811381138003800380538000000000000000000000000000000000000000000000000000000000000000000301000"

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
  _update60=function()
    -- init function (if any)
    if(i) i()
    -- 
    _update60,_draw=u,d
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
      printb("@fsouchu",2,121,12)
      printb("@gamecactus",83,121,12)

      pal(title_gfx.pal,1)
    end,
    -- init
    function()
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

  -- read max skill reached
  local max_skill=dget(33)
  if(max_skill>#menus[2][2]) max_skill=#menus[2][2] dset(33,max_skill)
  if(max_skill<2) max_skill=2 dset(33,max_skill)

  menus[1].max=max_map_id
  menus[2].max=max_skill

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
      if btnp(4) or btnp(5) then
        if(menu_i>0)sfx(1)
        menu_i+=1
        if menu_i>#menus then
          next_state(launch_state,menus[2].sel,menus[1].sel)
        end
      end      
    end,
    function()
      cls()
      memcpy(0x6000,0x4e00,64*13)
      spr(0,0,13,16,15)    
      printb("@fsouchu",2,121,12)
      printb("@gamecactus",83,121,12)

      -- dark menu background
      -- todo: fix
      for i=0,15 do
        pal(sget(112+i,128-13),sget(112+i,129-13))
        --pset(i,0,i)
      end
      sspr(12,51,104,19+#menus[menu_i][2]*9,12,64)
      pal()
      
      -- title
      printb(menus[menu_i][1],63-#menus[menu_i][1]*2,69,9)

      -- selection marker
      rectfill(18,70+menus[menu_i].sel*9,113,79+menus[menu_i].sel*9,4)
      palt(0,false)
      palt(12,true)
      sspr(anm_ttl\12*10,115,11,12,14,68+menus[menu_i].sel*9)
      palt()

      -- menu items
      for i=1,#menus[menu_i][2] do
        printb(menus[menu_i][2][i],28,72+i*9,i<=menus[menu_i].max and 8 or 11)
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

function launch_state(skill,id)
  -- record max level reached so far
  if(id>dget(32)) dset(32,id)

  unpack_gfx(loading_gfx.bytes)
  cls()
  spr(0,0,0,16,16)
  pal(loading_gfx.pal,1)
  local s="eNTERING ".._maps_label[id]
  printb(s,63-#s*2,80,15)
  flip()
  load(_maps_group[id]..".p8",nil,skill..","..id)
end

function endgame_state(skill)
  -- todo: music??
  
  -- record max skill reached
  if(skill>dget(33)) dset(33,skill)

  return
    function()
      if btnp()!=0 then
        -- back to startup screen
        next_state(fadetoblack_state,start_state)
      end
    end,
    function()
      cls()
      spr(0,0,0,16,16)
      pal(endgame_gfx.pal,1)
    end,
    function()
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
      cls()
      for i,r in pairs(r) do
        h[i]=lerp(h[i],129,r)
        sspr(i,0,1,128,i,h[i],1,128)
      end
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
  local skill,mapid,state=tonum(p[1]) or 2,tonum(p[2]) or 1,tonum(p[3]) or 0
  
  -- special case for end game state
  if(state==2 and mapid+1>#_maps_group) state=4

  local states={
    -- default
    [0]={start_state},
    -- 1: game over
    {fadetoblack_state,start_state},
    -- 2: next level
    {slicefade_state,launch_state,skill,mapid+1},
    -- 3: retry
    {slicefade_state,launch_state,skill,mapid},    
    -- 4: end game
    {fadetoblack_state,endgame_state,skill+1}
  }

  next_state(unpack(states[state]))
end

-->8
-- helper functions
function printb(s,x,y,c,bgc)
  bgc=bgc or 0
  print(s,x,y+1,bgc)
  print(s,x,y,c)
end

function lerp(a,b,t)
  return a+t*(b-a)
end
