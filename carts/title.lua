-- background menu
local snd="36530600324c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000060100003e0c3fa445d819c0158b1589158515841583158215811381138003800380538000000000000000000000000000000000000000000000000000000000000000000301000"
local schemes={
  {caption="esdf+⬅️⬆️⬇️➡️\nfire\023❎ use\023🅾️",btnfire=❎,btnuse=🅾️,btndown=⬇️,btnup=⬆️,space=0x10},
  {caption="esdf+⬅️➡️\nfire\023⬆️ use\023⬇️",btnfire=⬆️,btnuse=⬇️,btndown=7,btnup=7,space=0x8}
}

local leaderboard_pins,medals,leaderboard={
  victory1=3,
  victory2=4,
  victory3=5,
  victory4=6,  
  map=7,
  kills=8,
  time=9,
  -- todo: generate from mapinfo?
  secrets1=13,
  secrets2=14,
  secrets3=15,
  secrets4=16,
  secrets5=17,
  -- no secrets
  secrets6=18,
  -- total time
  total_time=19,
  -- todo: generate from mapinfo?
  punch1=23,
  punch2=24,
  punch2=25,
  punch2=26,
  punch2=27,
  punch2=28
},{}

function leaderboard_pack_fixed(key,value)
  -- export a 16:16 number
  poke4(0x5f80+leaderboard_pins[key],value)
end

function update_map_leaderboard(skill,id,level_time,total_time,kills,perfect_kills,perfect_secret)
  -- update leaderboard (if any)
  poke(0x5f80+leaderboard_pins.map,id)
  -- assumes number of kills < 255!
  poke(0x5f80+leaderboard_pins.kills,kills)
  leaderboard_pack_fixed("time",level_time)
  --
  poke(0x5f80+leaderboard_pins["secrets"..id],perfect_secret and 1 or 0)
  leaderboard_pack_fixed("total_time",total_time or 0)
end

-- supports player 1 or player 2 input
local _btnp=btnp
function btnp(b)
  return _btnp(b,0) or _btnp(b,1) 
end

-- copy image text to spritesheet/memory
function unpack_gfx(src)
  local offset=src.h-128
  local addr=offset>0 and 0x4e00 or 0x0
  offset=max(offset)
  src=src.bytes
  for i=0,#src-1 do
    poke(addr,ord(src,i+1))
    addr+=1
    if (addr>0x4e00+64*offset-1) addr=0x0
  end
end

-- assumes unpack_gfx called before
function draw_gfx(src)
  local offset=src.h-128
  memcpy(0x6000,0x4e00,64*offset)
  spr(0,0,offset,16,16)
end

-- game states
function next_state(fn,...)
  local u,d,i=fn(...)
  -- ensure update/draw pair is consistent
  _update=function()
    -- init function (if any)
    if(i) i()
    -- 
    _draw=d
    -- actually run the update
    u()
        
    -- gif capture handling
    if(peek(0x5f81)==1) poke(0x5f81,0) extcmd("video") 

    -- leaderboard?
    if not leaderboard and peek(0x5f82)>0 then
      -- medals status
      for i=0,peek(0x5f82)-1 do
        medals[i]=peek(0x5f83+i)
      end
      -- enable
      leaderboard=true
    end
  end
end

-- intro screen
function start_state()
  local ttl=300 
  -- reset saved state
  dset(0,-1)
  -- reserved:
  -- 0: save
  -- 1-13: health+armor+weapons+ammo
  -- 17-23: level time
  for i=1,#_maps_group do
    dset(16+i,0)
  end
  -- clear gpio (for leaderboard)
  memset(0x5f83,0,32)

  return
    -- update
    function()    
      if ttl<0 or btnp(4) or btnp(5) then
        next_state(menu_state)
      end  
      ttl-=1
    end,
    -- draw
    function()
      cls()
      pal()
      draw_gfx(title_gfx)
      printb("@fsouchu",2,121,vcol(4))
      printb("@gamecactus",83,121,vcol(4))

      -- display leaderboard activation
      if(leaderboard) printb("★",1,1,vcol(14),vcol(13))

      pal(title_gfx.pal,1)
    end,
    -- init
    function()
      unpack_gfx(title_gfx)
    end  
end

function menu_state()
  local mouse_ttl,mouse_x,mouse_y=0,0,0
  local anm_ttl,menus=0
  -- restore mouse acceleration
  local mouse_acc,keyboard_mode=mid(dget(39)\4,1,8),mid(dget(34),0,1)

  menus={
    select={
      title="sELECT",
      entries={"nEW gAME","cONTROLS"},
      sel=1,
      max=2,
      next=function(menus,sel)
        if(sel==1) return menus.levels
        return menus.controls
      end
    },
    controls={
      title="cONTROL oPTIONS",
      entries={
        function()
          if mouse_ttl>0 then
            return "esdf+mouse:\nfire\023lmb use\023rmb"
          end
          return schemes[keyboard_mode+1].caption
        end,
        function() 
          local bar="\nfast"
          for i=1,8 do
            if i==mouse_acc then
              bar=bar.."▮"
            else
              bar=bar.."-"
            end
          end
          return "mouse sensitivity"..bar.."slow" end
      },
      height=14,
      sel=1,
      max=2,
      back="select",
      next=function(menus,sel)
        if sel==1 and mouse_ttl==0 then
          keyboard_mode+=1
          if(keyboard_mode>1) keyboard_mode=0
          switch_scheme(keyboard_mode)        
        elseif sel==2 then
          mouse_acc+=1
          if(mouse_acc>8) mouse_acc=1
          dset(39,mouse_acc*4)
        end
        -- stay on menu
        return menus.controls
      end
    },    
    levels={
      title="wHICH ePISODE?",
      entries=_maps_label,
      sel=1,
      max=1,
      back="select",
      next="skills"
    },
    skills={
      title="sELECT sKILL lEVEL",
      entries={
        "i AM TOO YOUNG TO DIE",
        "hEY, NOT TOO ROUGH",
        "hURT ME PLENTY",
        "uLTRA-vIOLENCE"
      },
      sel=2,
      max=4,
      back="levels",
      next=function(menus)
        next_state(launch_state,menus.skills.sel,menus.levels.sel)     
      end}
    },0

  -- read max level reached
  local max_map_id=dget(32)
  if(max_map_id>#_maps_label) max_map_id=#_maps_label dset(32,max_map_id)
  if(max_map_id<=0) max_map_id=1 dset(32,max_map_id)

  menus.levels.max=max_map_id

  local active_menu=menus.select

  return
    function()
      -- mouse?
      if stat(38)!=0 then
        mouse_ttl=30
        mouse_x=mid(mouse_x+stat(38)/mouse_acc,0,126)
        mouse_y=mid(mouse_y+stat(39)/mouse_acc,0,126)
      end
      if mouse_ttl>0 then
        mouse_ttl-=1        
        switch_scheme(0)
      end

      anm_ttl=(anm_ttl+1)%48
      local active_sel,height=active_menu.sel,active_menu.height or 8
      if mouse_ttl>0 then
        local prev_sel=active_sel
        for i=1,#active_menu.entries do
          if i<=active_menu.max and mouse_y>69+i*height and mouse_y<=75+i*height then
            active_sel=i
          end
        end   
        if(prev_sel!=active_sel) sfx(0)
      else
        if btnp(2) then
          active_sel-=1
          sfx(0)
        end
        if btnp(3) then
          active_sel+=1
          sfx(0)
        end
        -- unlocked?
        active_sel=mid(active_sel,1,active_menu.max)
      end
      active_menu.sel=active_sel

      if btnp(🅾️) then
        -- cancel
        if active_menu.back then
          sfx(0)
          active_menu=menus[active_menu.back]        
        end
      elseif btnp(❎) then
        if active_menu.next then
          sfx(1)
          local next_menu=menus[active_menu.next]
          if next_menu then
            active_menu=next_menu
          else
            -- function?
            active_menu=active_menu.next(menus,active_sel)
          end
        end        
      end
    end,
    function()
      -- exit early because state will have been change to launch_state
      if (not active_menu)return

      cls()
      draw_gfx(title_gfx)
      printb("@FSOUCHU",2,0,vcol(2))
      printb("@GAMECACTUS",83,0,vcol(2))

      -- dark menu background
      for i=0,15 do
        pal(vcol(i),sget(112+i,128-11))
        --pset(i,0,i)
      end
      local height=active_menu.height or 8
      palt(0,false)
      sspr(12,52,104,15+#active_menu.entries*height,12,64)
      pal()
      
      -- title
      printb(active_menu.title,63-#active_menu.title*2,67,vcol(14))

      -- selection marker
      rectfill(18,76+(active_menu.sel-1)*height,113,75+active_menu.sel*height,vcol(2))
      palt(vcol(0),false)
      palt(vcol(4),true)
      sspr(anm_ttl\12*10,116,11,12,14,74+(active_menu.sel-1)*height)
      palt()

      -- menu items      
      for i=1,#active_menu.entries do
        local s=active_menu.entries[i]
        if(type(s)=="function") s=s()
        if(i>active_menu.max) s=masked(s)
        printb(s,28,77+(i-1)*height,i<=active_menu.max and vcol(4) or vcol(3))
        local hs=""
        for j=1,#s do
          local c=sub(s,j,j)
          if c=="🅾️" or c=="❎" or c=="⬅️" or c=="⬆️" or c=="⬇️" or c=="➡️" or c=="▮" or c=="\n" then
            hs=hs..c
          else
            hs=hs.." "
          end
        end
        printb(hs,28,77+(i-1)*height,i<=active_menu.max and vcol(13) or vcol(3))
      end
      
      if(mouse_ttl>0) palt(vcol(4),true) sspr(41,116,10,10,mouse_x,mouse_y) palt()

      pal(title_gfx.pal,1)
    end,
    -- init
    function()
      unpack_gfx(title_gfx)
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

  -- record per level play time
  dset(16+id,level_time)
  update_map_leaderboard(skill,id,level_time,nil,kills,kills==monsters,secrets==all_secrets)

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

function launch_state(skill,id)
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
      draw_gfx(loading_gfx)
      pal(loading_gfx.pal,1)
      local s="eNTERING ".._maps_label[id]
      local texty=40
      printb(s,63-#s*2,texty,15)

      -- location marker
      local x,y=_maps_loc[id*2-1],_maps_loc[id*2]
      -- valid
      if x!=-1 then
        -- outline location name
        rect(63-#s*2-2,texty-2,63+#s*2,texty+6,15)

        -- left or right anchor
        local xanchor=mid(x<64 and 63-#s*2-2 or 63+#s*2,32,96)
        line(xanchor,texty+6,xanchor,y,15)
        line(x,y)
        circfill(x,y,2)
      end
    end,
    function()
      unpack_gfx(loading_gfx)
    end
end

function credits_state(skill,id,level_time,kills,monsters,secrets,all_secrets)  
  local ttl,t,creditsi=0,{},0

  -- update leaderboard (if any)
  poke(0x5f83+leaderboard_pins["victory"..skill],1)

  local total_time=level_time
  for i=1,id-1 do
    local t=dget(16+i)
    -- not a full run?
    if(t==0) total_time=nil break
    total_time+=t
  end
  update_map_leaderboard(skill,id,level_time,total_time,kills,kills==monsters,secrets==all_secrets)

  return
    -- update
    function()
      if ttl>3000 or btnp(4) or btnp(5) then
        -- back to startup screen
        next_state(start_state)
      end

      ttl+=1
    end,
    -- draw
    function()
      cls()
      pal()
      draw_gfx(endgame_gfx)  
      
      local i=flr(#_credits*ttl/600)
      if i!=creditsi then
        t={}
        creditsi=i
      end
      -- input strings + patterns
      local s,fadein,fadeout=_credits[(creditsi%#_credits)+1]," ⁘⁙□■▮","▮■□⁙⁘"
      -- result string to display
      local sp=""
      for i=1,#s do
        t[i]=t[i] or -rnd(1)
        t[i]+=0.1
        -- character strip animation (fade in/char/fade out)
        local st=fadein
        for k=1,30 do
          st=st..sub(s,i,i)
        end
        st=st..fadeout
        -- pick a caracter from the strip
        local j=max(flr(#st*t[i]/3))+1        
        sp=sp..sub(st,j,j)
      end
      print(sp,64-#sp*2,80,15)

      if(ttl>0 and time()%4<2) print("FIRE/USE\23MENU",38,122,15)

      pal(endgame_gfx.pal,1)      
    end,
    function()
      endgame_gfx.pal[15]=1
      unpack_gfx(endgame_gfx)
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

function switch_scheme(scheme)
  local s=schemes[scheme+1]
  -- save scheme
  dset(34,scheme)
  dset(35,s.btnfire)
  dset(36,s.btnuse)
  dset(37,s.btndown)
  dset(38,s.btnup)

  -- export via gpio
  poke(0x5f80,s.space)
end

function _init()
  -- mouse suport
  poke(0x5f2d, 7)

  cartdata(mod_name)
  -- default sensititivity
  if(dget(39)==0) dset(39,8)
  
  -- control scheme
  switch_scheme(dget(34))

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
    {fadetoblack_state,credits_state,skill,mapid,level_time,kills,monsters,secrets,_secrets[mapid]}
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
  return sget(112+c,128-12)
end