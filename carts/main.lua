pico-8 cartridge // http://www.pico-8.com
version 29
__lua__

-- globals
local _bsp,_cam,_plyr,_things,_sprite_cache,_actors
local _onoff_textures={}
local _ambientlight,_znear,_ammo_factor=0,16,1
local _msg

--local k_far,k_near=0,2
--local k_right,k_left=4,8

-- copy color gradients (16*16 colors x 2) to memory
memcpy(0x4300,0x1000,512)

function make_camera()
  local shkx,shky=0,0  
  return {
    m={
      1,0,0,
      0,1,0},
    u=1,
    v=0,
    track=function(self,pos,angle,height)
      local ca,sa=-sin(angle),cos(angle)
      self.u=ca
      self.v=sa
      -- world to cam matrix
      self.m={
        ca,-sa,-ca*pos[1]+sa*pos[2],
        -height,
        sa,ca,-sa*pos[1]-ca*pos[2]
      }
    end,
    shake=function()
      shkx,shky=min(1,shkx+rnd()),min(1,shky+rnd())
    end,
    update=function()
      shkx*=-0.7-rnd(0.2)
      shky*=-0.7-rnd(0.2)
      if abs(shkx)<0.5 and abs(shky)<0.5 then
        shkx,shky=0,0
      end
      camera(shkx,shky)  
    end,
    is_visible=function(self,bbox)    
      local outcode,m1,m3,m4,_,m9,m11,m12=0xffff,unpack(self.m)
      for i=1,8,2 do
        local x,z=bbox[i],bbox[i+1]
        -- x2: fov
        local ax,az=(m1*x+m3*z+m4)<<1,m9*x+m11*z+m12
        -- todo: optimize?
        local code=2
        if(az>16) code=0
        if(az>854) code|=1
        if(ax>az) code|=4
        if(-ax>az) code|=8
        outcode&=code
      end
      return outcode==0
    end
  }
end

function lerp(a,b,t)
  return a*(1-t)+b*t
end
function smoothstep(t)
	t=mid(t,0,1)
	return t*t*(3-2*t)
end
-- return shortest angle to target
function shortest_angle(target_angle,angle)
	local dtheta=target_angle-angle
	if dtheta>0.5 then
		angle+=1
	elseif dtheta<-0.5 then
		angle-=1
	end
	return angle
end

-- 3d vector functions
function v_lerp(a,b,t)
  local t_1=1-t
  return {
    a[1]*t_1+t*b[1],
    a[2]*t_1+t*b[2],
    a[3]*t_1+t*b[3]
  }
end

-- coroutine helpers
local _futures={}
-- registers a new coroutine
function do_async(fn)
  add(_futures,cocreate(fn))
end
-- wait until timer
function wait_async(t,fn)
	for i=1,t do
		if(fn) fn(i)
		yield()
	end
end

-- 2d vector functions
function v2_dot(a,b)
  return a[1]*b[1]+a[2]*b[2]
end

function v2_normal(v)
  local d=v2_len(v)
  return {v[1]/d,v[2]/d},d
end

function v2_add(a,b,scale)
  scale=scale or 1
  a[1]+=scale*b[1]
  a[2]+=scale*b[2]
end

-- safe vector len
function v2_len(a)
  local dx,dy=abs(a[1]),abs(a[2])
  local d=max(dx,dy)
  local n=min(dx,dy)/d
  return d*sqrt(n*n + 1)
end

function v2_make(a,b)
  return {b[1]-a[1],b[2]-a[2]}
end

-- bold print helper
function printb(txt,x,y,c1,c2)
  print(txt,x,y+1,c2)
  print(txt,x,y,c1)
end

-->8
-- virtual sprites
function vspr(frame,sx,sy,scale,flipx)
  -- faster equivalent to: palt(0,false)
  poke(0x5f00,0)
  local w,xoffset,yoffset,tc,tiles=unpack(frame)
  palt(tc,true)
  local sw,xscale=xoffset*scale,flipx and -scale or scale
  sx-=sw
	if(flipx) sx+=sw  
	sy-=yoffset*scale
	for i,tile in pairs(tiles) do
    local dx,dy,ssx,ssy=sx+(i%w)*xscale,sy+(i\w)*scale,_sprite_cache:use(tile)
    -- scale sub-pixel fix 
    sspr(ssx,ssy,16,16,dx,dy,scale+dx%1,scale+dy%1,flipx)
    -- print(tile,(i%w)*16,(i\w)*16,7)
  end
  palt()
end

-- https://github.com/luapower/linkedlist/blob/master/linkedlist.lua
function make_sprite_cache(tiles,maxlen)
	local len,index,first,last=0,{}

  -- note: keep multiline assignments, they are *faster*
	local function remove(t)
		if t._next then
			if t._prev then
				t._next._prev = t._prev
				t._prev._next = t._next
			else
				t._next._prev = nil
				first = t._next
			end
		elseif t._prev then
			t._prev._next = nil
			last = t._prev
		else
			first = nil
			last = nil
		end
		-- gc
		t._next = nil
		t._prev = nil
		len-=1
		return t
	end
	
	return {
    clear=function()  len,index,first,last=0,{} end,
		use=function(self,id)
			local entry=index[id]
			if entry then
				-- existing item?
				-- force refresh
				remove(entry)
			else
				-- allocate a new 16x16 entry
				-- todo: optimize
				local sx,sy=(len<<4)&127,64+(((len<<4)\128)<<4)
				-- list too large?
				if len+1>maxlen then
					local old=remove(first)
					-- reuse cache entry
					sx,sy,index[old.id]=old[1],old[2]
				end
				-- new (or relocate)
				-- copy data to sprite sheet
				local mem=sx\2|sy<<6
				for j=0,31 do
					poke4(mem|(j&1)<<2|(j\2)<<6,tiles[id+j])
				end		
				--
				entry={sx,sy,id=id}
				-- reverse lookup
				index[id]=entry
			end
			-- insert 'fresh'
			local anchor=last
			if anchor then
				if anchor._next then
					anchor._next._prev=entry
					entry._next=anchor._next
				else
					last=entry
				end
				entry._prev=anchor
				anchor._next=entry
			else
			 -- empty list use case
				first,last=entry,entry
			end
			len+=1
			-- return sprite sheet coords
			return entry[1],entry[2]
		end
	}
end

-->8
-- bsp rendering
function polyfill(v,offset,tex,light)
  poke4(0x5f38,tex)

  -- todo: get proper player height
  local ca,sa,cx,cy,cz=_cam.u,_cam.v,_plyr[1]>>4,(_plyr[3]+45-offset)<<3,_plyr[2]>>4

  local v0,spans,pal0=v[#v],{}
  local x0,w0=v0.x,v0.w
  local y0=v0.y-offset*w0
  for i,v1 in ipairs(v) do
    local x1,w1=v1.x,v1.w
    local y1=v1.y-offset*w1
    local _x1,_y1,_w1=x1,y1,w1
    if(y0>y1) x1=x0 y1=y0 w1=w0 x0=_x1 y0=_y1 w0=_w1
    local dy=y1-y0
    local dx,dw=(x1-x0)/dy,(w1-w0)/dy
    local cy0=y0\1+1
    local sy=cy0-y0
    if(y0<0) x0-=y0*dx w0-=y0*dw cy0=0 sy=0
    x0+=sy*dx
    w0+=sy*dw

    if(y1>127) y1=127
    for y=cy0,y1\1 do
      local span=spans[y]
      if span then
      -- limit visibility
        if w0>0.15 then
          -- collect boundaries
          local a,b=x0,span
          if(a>b) a=span b=x0
          -- color shifing
          local pal1=(light*min(15,w0<<5))\1
          if(pal0!=pal1) memcpy(0x5f00,0x4300|pal1<<4,16) pal0=pal1

          -- mode7 texturing
          local rz=cy/(y-63.5)
          local rx=rz*(a-63.5)>>7
          local x,z=ca*rx+sa*rz+cx,-sa*rx+ca*rz+cz
        
          tline(a,y,b,y,x,z,ca*rz>>7,-sa*rz>>7)   
        end       
      else
        spans[y]=x0
      end

      x0+=dx
      w0+=dw
    end			
    x0=_x1
    y0=_y1
    w0=_w1
  end
end

function draw_sub_sector(segs,v_cache,light)
  -- get heights
  local sector=segs.sector
  local v0,top,bottom,pal0=v_cache[#v_cache],sector.ceil,sector.floor
  local x0,y0,w0=v0.x,v0.y,v0.w

  -- todo: test ipairs
  for i,v1 in ipairs(v_cache) do
    local seg,x1,y1,w1=v0.seg,v1.x,v1.y,v1.w
    local _x1,ldef=x1,seg.line
    -- logical split or wall?
    -- front facing?
    if x0<x1 and ldef then
      -- span rasterization
      -- pick correct texture "major"
      local dx,u0=x1-x0,v0[seg[7]]*w0
      local cx0,dy,du,dw=x0\1+1,(y1-y0)/dx,(v1[seg[7]]*w1-u0)/dx,(w1-w0)/dx
      local sx=cx0-x0      
      if(x0<0) y0-=x0*dy u0-=x0*du w0-=x0*dw cx0=0 sx=0
      y0+=sx*dy
      u0+=sx*du
      w0+=sx*dw

      -- dual?
      local facingside,otherside,otop,obottom=ldef[seg.side],ldef[not seg.side]
      -- peg bottom?
      local offsety,toptex,midtex,bottomtex=(bottom-top)>>4,facingside.toptex,facingside.midtex,facingside.bottomtex
      -- fix animated side walls (elevators)
      if ldef.flags&0x4!=0 then
        offsety=0
      end
      if otherside then
        -- visible other side walls?
        otop=otherside.sector.ceil
        obottom=otherside.sector.floor
        -- offset animated walls (doors)
        if ldef.flags&0x4!=0 then
          offsety=(otop-top)>>4
        end
        -- make sure bottom is not crossing this side top
        obottom=min(top,obottom)
        otop=max(bottom,otop)
        if(top<=otop) otop=nil
        if(bottom>=obottom) obottom=nil
      end

      if(x1>127) x1=127
      for x=cx0,x1\1 do
        if w0>0.15 then
          -- color shifing
          local pal1=(light*min(15,w0<<5))\1
          if(pal0!=pal1) memcpy(0x5f00,0x4300|pal1<<4,16) pal0=pal1
          local t,b,w=y0-top*w0,y0-bottom*w0,w0<<4
          -- wall
          -- top wall side between current sector and back sector
          local ct=t\1+1
          if otop and toptex then
            poke4(0x5f38,toptex)             
            local ot=y0-otop*w0
            tline(x,ct,x,ot,u0/w,(ct-t)/w+offsety,0,1/w)
            -- new window top
            t=ot
            ct=ot\1+1
          end
          -- bottom wall side between current sector and back sector     
          if obottom and bottomtex then
            poke4(0x5f38,bottomtex)             
            local ob=y0-obottom*w0
            local cob=ob\1+1
            tline(x,cob,x,b,u0/w,(cob-ob)/w,0,1/w)
            -- new window bottom
            b=ob\1
          end
          -- middle wall?
          if not otherside and midtex then
            -- texture selection
            poke4(0x5f38,midtex)

            tline(x,ct,x,b,u0/w,(ct-t)/w+offsety,0,1/w)
          end
        end
        y0+=dy
        u0+=du
        w0+=dw
      end
    end
    v0=v1
    x0=_x1
    y0=y1
    w0=w1
  end
end

-- ceil/floor/wal rendering
function draw_flats(v_cache,segs,things)
  local verts,outcode,nearclip={},0xffff,0
  local m1,m3,m4,m8,m9,m11,m12=unpack(_cam.m)
  
  -- to cam space + clipping flags
  for i,seg in ipairs(segs) do
    local v0=seg[1]
    local v=v_cache[v0]
    if not v then
      local x,z=v0[1],v0[2]
      local ax,az=
        m1*x+m3*z+m4,
        m9*x+m11*z+m12
      local code=2
      if(az>16) code=0
      if(az>854) code|=1
      -- fov adjustment
      if(-2*ax>az) code|=4
      if(2*ax>az) code|=8
      
      local w=128/az
      v={ax,m8,az,outcode=code,u=x,v=z,x=63.5+ax*w,y=63.5-m8*w,w=w}
      v_cache[v0]=v
    end
    v.seg=seg
    verts[i]=v
    local code=v.outcode
    outcode&=code
    nearclip+=(code&2)
  end
  -- out of screen?
  if outcode==0 then
    if(nearclip!=0) verts=z_poly_clip(_znear,verts)
    if #verts>2 then
      local sector=segs.sector
      local light=max(sector.lightlevel,_ambientlight)
      -- no texture = sky/background
      if(sector.floortex and sector.floor+m8<0) polyfill(verts,sector.floor,sector.floortex,light)
      if(sector.ceiltex and sector.ceil+m8>0) polyfill(verts,sector.ceil,sector.ceiltex,light)

      -- walls
      draw_sub_sector(segs,verts,light)

      -- draw things (if any) in this convex space
      local head,pal0=things[1].next
      while head do
        local thing,x0,y0,w0=head.thing,head.x,head.y,head.w
        -- get image from current state
        local frame=thing.state
        local side,_,flipx,bright,sides=0,unpack(frame)
        -- use frame brightness level
        local pal1=bright and 8 or (light*min(15,w0<<5))\1
        if(pal0!=pal1) memcpy(0x5f00,0x4300|min(pal1,15)<<4,16) pal0=pal1            
        -- pick side (if any)
        if sides>1 then
          local angle=((atan2(-thing[1]+_plyr[1],thing[2]-_plyr[2])-thing.angle+0.0625)%1+1)%1
          side=(sides*angle)\1
          flipx=flipx&(1<<side)!=0
        else
          flipx=nil
        end
        vspr(frame[side+5],x0,y0,w0<<5,flipx)
        -- thing:draw_vm(x0,y0)
        -- print(thing.angle,x0,y0,8)
        head=head.next
      end
    end
  end
end
-- traverse and renders bsp in back to front order
-- calls 'visit' function
function visit_bsp(node,pos,visitor)
  local side=node[1]*pos[1]+node[2]*pos[2]<=node[3]
  visitor(node,side,pos,visitor)
  visitor(node,not side,pos,visitor)
end

function find_sub_sector(node,pos)
  local side=v2_dot(node,pos)<=node[3]
  if node.leaf[side] then
    -- leaf?
    return node[side]
  else    
    return find_sub_sector(node[side],pos)
  end
end

function find_ssector_thick(node,pos,radius,res)
  local dist,d=v2_dot(node,pos),node[3]
  local side,otherside=dist<=d-radius,dist<=d+radius
  -- leaf?
  if node.leaf[side] then
    res[node[side]]=true
  else
    find_ssector_thick(node[side],pos,radius,res)
  end
  -- straddling?
  if side!=otherside then
    if node.leaf[otherside] then
      res[node[otherside]]=true
    else
      find_ssector_thick(node[otherside],pos,radius,res)
    end
  end
end

-- http://geomalgorithms.com/a13-_intersect-4.html
function intersect_sub_sector(segs,p,d,tmin,tmax,res,skipthings)
  local _tmax=tmax
  local px,pz,dx,dz,tmax_seg=p[1],p[2],d[1],d[2]

  if not skipthings then
    -- hitting things?
    local things_hits={t=-32000}
    for _,thing in pairs(_things) do
      local actor=thing.actor
      -- not a missile
      -- not dead
      if actor.flags&0x4==0 and not thing.dead and thing.subs[segs] then
        -- overflow 'safe' coordinates
        local m,r={(px-thing[1])>>8,(pz-thing[2])>>8},actor.radius>>8
        local b,c=v2_dot(m,d),v2_dot(m,m)-r*r

        -- check distance and ray direction vs. circle
        if c<=0 or b<=0 then
          local discr=b*b-c
          if discr>=0 then
            -- convert back to world units
            local t=(-b-sqrt(discr))<<8
            -- if t is negative, ray started inside sphere so clamp t to zero 
            -- if(t<tmin) t=tmin
            -- record hit
            if t>=tmin and t<tmax then
              -- empty list case
              local head,prev=things_hits,things_hits
              while head and head.t<t do
                -- swap/advance
                prev,head=head,head.next
              end
              -- insert new thing
              prev.next={t=t,thing=thing,next=prev.next}
            end
          end
        end
      end
    end
    -- add sorted things intersections
    local head=things_hits.next
    while head do
      add(res,head)
      head=head.next
    end
  end

  for _,s0 in ipairs(segs) do
    local n=s0[5]
    local denom,dist=v2_dot(n,d),s0[6]-v2_dot(n,p)
    if denom==0 then
      -- parallel and outside
      if(dist<0) return
    else
      local t=dist/denom
      -- within seg?
      local pt={
        px+t*dx,
        pz+t*dz
      }
      local d=v2_dot(s0[2],pt)-s0[3]
      if d>=0 and d<s0[4] then
        if denom<0 then
          if(t>tmin) tmin=t
          if(tmin>tmax) return
        else
          if(t<tmax) tmax=t tmax_seg=s0
          if(tmax<tmin) return 
        end
      end 
    end
  end

  if tmin<=tmax and tmax_seg then
    -- don't record node compiler lines
    if(tmax_seg.line) add(res,{t=tmax,seg=tmax_seg,n=tmax_seg[5]})
    -- any remaining segment to check?
    if(tmax<_tmax and tmax_seg.partner) intersect_sub_sector(tmax_seg.partner,p,d,tmax,_tmax,res,skipthings)
  end
end

-- returns distance and normal to target (if visible)
-- note: always return normalized target vector
function line_of_sight(thing,otherthing,maxdist)
  local n,d=v2_normal(v2_make(thing,otherthing))
  -- in radius?
  d=max(d-thing.actor.radius)
  if d<maxdist then
    -- line of sight?
    local h,hits,blocking=thing[3]+24,{}
    intersect_sub_sector(thing.ssector,thing,n,0,d,hits,true)
    for _,hit in pairs(hits) do
      -- bsp hit?
      local ldef=hit.seg.line
      local otherside=ldef[not hit.seg.side]
      if otherside==nil or 
        h>otherside.sector.ceil or 
        h<otherside.sector.floor then
        -- blocking wall
        return n
      end 
    end
    -- normal and distance to hit
    return n,d
  end
  return n
end

local depth_cls={
  __index=function(t,k)
    -- head of stack
    local head={w=0}
    t[k]=head
    return head
  end
}
local depthsorted_cls={
  __index=function(t,k)
    local s=setmetatable({},depth_cls)
    t[k]=s
    return s
  end
}

function make_thing(actor,x,y,z,angle)
   -- all sub-sectors that thing touches
  -- used for rendering and collision detection
  local subs,pos={},{x,y}
  find_ssector_thick(_bsp,pos,actor.radius,subs)
  -- default height & sector specs
  local ss=find_sub_sector(_bsp,pos)
  -- attach instance properties to new thing
  local thing=actor:attach({
    -- z: altitude
    x,y,ss.sector.floor,
    angle=angle,
    subs=subs,
    sector=ss.sector,
    ssector=ss
  })
  if actor.flags&0x2>0 then
    -- shootable
    thing=with_physic(with_health(thing))
  end
  return thing,actor
end

-- sector damage
local _sector_dmg={
  [71]=5,
  [69]=10,
  [80]=20,
  -- instadeath
  [115]=-1
}

function with_physic(thing)
  local actor=thing.actor
  -- actor properties
  local height,radius,mass,is_missile,is_player=actor.height,actor.radius,2*actor.mass,actor.flags&0x4==4,actor.id==1
  local ss,friction=thing.ssector,is_missile and 0.9967 or 0.9062
  -- init inventory
  local forces,velocity={0,0},{0,0,0}
  return setmetatable({
    apply_forces=function(self,x,y)
      -- todo: revisit force vs. impulse
      forces[1]+=96*x/mass
      forces[2]+=96*y/mass
    end,
    update=function(self)
      -- integrate forces
      v2_add(velocity,forces)
      velocity[3]-=1

      -- friction     
      velocity[1]*=friction
      velocity[2]*=friction
      
      -- check collision with world
      local move_dir,move_len,hits=v2_normal(velocity)
      
      if move_len>0.001 then
        local h=self[3]
        hits={}
        -- player: check intersection w/ additional contact radius
        intersect_sub_sector(ss,self,move_dir,0,move_len+radius+(is_player and 24 or 0),hits)    
        -- fix position
        local stair_h=is_missile and 0 or 24
        for _,hit in ipairs(hits) do
          -- skip "front colliders"
          if hit.t<move_len+radius then
            local fix_move
            if hit.seg then
              -- bsp hit?
              local ldef=hit.seg.line
              local otherside=ldef[not hit.seg.side]

              if otherside==nil or 
                -- impassable
                (not is_missile and ldef.flags&0x40>0) or
                h+height>otherside.sector.ceil or 
                h+stair_h<otherside.sector.floor then
                fix_move=hit
              end
              
              -- cross special?
              -- todo: supports monster activated triggers
              if is_player and ldef.trigger and ldef.flags&0x10>0 then
                ldef.trigger(self)
              end
            elseif hit.thing!=self then
              -- thing hit?
              local otherthing=hit.thing
              local otheractor=otherthing.actor
              if is_player and otherthing.pickup then
                -- avoid reentrancy
                otherthing.pickup=nil
                -- jump to pickup state
                otherthing:jump_to(10)
                otheractor.pickup(otherthing,self)
              elseif otheractor.flags&0x1>0 then
                -- solid actor?
                fix_move=hit
                fix_move.n=v2_normal(v2_make(self,otherthing))
              end
            end

            if fix_move then
              if is_missile then
                -- fix position & velocity
                v2_add(self,move_dir,fix_move.t-radius)
                velocity={0,0,0}
                -- explosion sound (if any)
                if(actor.deathsound) sfx(actor.deathsound)
                -- death state
                self:jump_to(5)
                -- hit thing
                local otherthing=fix_move.thing
                if(otherthing and otherthing.hit) otherthing:hit(actor.damage,move_dir,self.owner)
                -- stop at first wall/thing
                break
              else
                local n=fix_move.n
                local fix=(fix_move.t-radius)*v2_dot(n,move_dir)
                -- avoid being pulled toward prop/wall
                if fix<0 then
                  -- apply impulse (e.g. fix velocity)
                  v2_add(velocity,n,fix)
                end
              end
            end
          end
        end

        -- apply move
        v2_add(self,velocity)
              
        -- refresh sector after fixed collision
        ss=find_sub_sector(_bsp,self)
        self.sector=ss.sector
        self.ssector=ss

        -- refresh overlapping sectors
        local subs={}
        find_ssector_thick(_bsp,self,radius,subs)
        self.subs=subs
      end
      -- triggers?
      -- check triggers/bumps/...
      if is_player then
        if not hits then
          local angle=self.angle
          hits={}
          intersect_sub_sector(ss,self,{cos(angle),-sin(angle)},0,radius+24,hits)    
        end
        for _,hit in ipairs(hits) do
          if hit.seg then
            local ldef=hit.seg.line
            -- buttons
            if ldef.trigger and ldef.flags&0x8>0 then
              -- use special?
              if btn(🅾️) then
                ldef.trigger()
              else
                _msg="press 🅾️ to activate"
              end
              -- trigger/message only closest hit
              break
            end
          end
        end
      end

      -- gravity
      if not is_missile then
        local dz=velocity[3]
        local h,sector=self[3]+dz,self.sector
        if h<sector.floor then
          -- fall damage
          -- see: https://zdoom.org/wiki/Falling_damage
          local dmg=(((dz*dz)>>7)*11-30)\2
          if(dmg>0) self:hit(dmg) 
          
          -- sector damage (if any)
          self:hit_sector(_sector_dmg[sector.special])

          velocity[3]=0
          h=sector.floor
        end
        self[3]=h
      end

      -- reset forces
      forces={0,0}
    end
  },{__index=thing})
end

function with_health(thing)
  local dmg_ttl,dead=0
  local function die(self,dmg)
    self.dead=true
    -- lock state
    dead=true
    -- death state
    self:jump_to(5)
  end
  -- base health (for gibs effect)
  return setmetatable({
    hit=function(self,dmg,dir,instigator)
      -- avoid reentrancy
      if(dead) return
      
      -- avoid automatic infight
      if(self==_plyr or instigator==_plyr or rnd()>0.8) self.target=instigator

      -- damage reduction?
      local hp,armor=dmg,self.armor or 0
      if armor>0 then
        hp=0.7*dmg
        self.armor=max(armor-0.3*dmg)\1
      end
      self.health=max(self.health-hp)\1
      if self.health==0 then
        die(self,dmg)
      end
      -- kickback
      if dir then
        self:apply_forces(hp*dir[1],hp*dir[2])
      end
      return hp
    end,
    hit_sector=function(self,dmg)
      if(dead) return
      -- instadeath
      if(dmg==-1) then
        self:hit(10000)
        return
      end
      -- clear damage
      if(not dmg) dmg_ttl=0 return
      dmg_ttl-=1
      if dmg_ttl<0 then
        dmg_ttl=15
        self:hit(dmg)
      end
    end
  },{__index=thing})
end

function attach_plyr(thing,actor,skill)
  local dmg_factor=({0.5,1,1,2})[skill]
  local bobx,boby,speed,da,wp,wp_slot,wp_yoffset,wp_y,hit_ttl,wp_switching=0,0,actor.speed,0,thing.weapons,thing.active_slot,0,0,0

  local function wp_switch(slot)
    if(wp_switching) return
    wp_switching=true
    do_async(function()
      wp_yoffset=-32
      wait_async(15)
      wp_slot,wp_yoffset=slot,0
      wait_async(15)
      wp_switching=nil
    end)
  end
  local function try_switch(inc)
    local i=wp_slot+inc
    while not wp[i] do
      i+=inc
      -- wrap
      if(i>10) i=1
      if(i<1) i=10
      -- no weapon to switch to
      if(i==wp_slot) return
    end
    wp_switch(i)
  end

  return setmetatable({
    update=function(self,...)
      thing.update(self,...)
      hit_ttl=max(hit_ttl-1)
    end,
    control=function(self)
      wp_y=lerp(wp_y,wp_yoffset,0.3)

      local dx,dz=0,0
      -- cursor: fwd+rotate
      -- cursor+x: weapon switch+rotate
      -- wasd: fwd+strafe
      -- o: fire
      if btn(🅾️) then
        if(btn(0)) dx=1
        if(btn(1)) dx=-1

        -- todo: check weapon ready state
        if(btn(2)) try_switch(-1)
        if(btn(3)) try_switch(1)
      else
        if(btn(0)) da-=1
        if(btn(1)) da+=1
        if(btn(2)) dz=1
        if(btn(3)) dz=-1
      end
      -- wasd
      if(btn(0,1)) dx=1
      if(btn(1,1)) dx=-1
      if(btn(2,1)) dz=1
      if(btn(3,1)) dz=-1

      self.angle-=da/256
      local ca,sa=cos(self.angle),-sin(self.angle)
      self:apply_forces(speed*(dz*ca-dx*sa),speed*(dz*sa+dx*ca))

      -- damping
      -- todo: move to physic code?
      da*=0.8

      -- update weapon vm
      wp[wp_slot].owner=self
      wp[wp_slot]:tick()
      
      -- weapon bobing
      bobx,boby=lerp(bobx,2*da,0.3),lerp(boby,cos(time()*3)*abs(dz)*speed*2,0.2)
    end,
    attach_weapon=function(self,weapon)
      local slot=weapon.actor.slot
      -- got weapon already?
      if(wp[slot]) return

      -- attach weapon
      wp[slot]=weapon
      weapon.owner=self

      -- jump to ready state
      weapon:jump_to(7)
      weapon:tick()

      -- auto switch
      wp_switch(slot)
    end,
    hud=function(self)
      
      local active_wp=wp[wp_slot]
      local frame,light=active_wp.state,self.sector.lightlevel
      
      -- sector light affects weapon sprite
      light=frame[3] and 8 or min((light*15)\1,15)
      memcpy(0x5f00,0x4300|light<<4,16)          

      -- draw current weapon (single) frame      
      vspr(frame[5],64-bobx,132-wp_y+boby,16)

      pal()
      local ammotype=active_wp.actor.ammotype
      printb(ammotype.icon..self.inventory[ammotype],106,120,9)
      printb("♥"..self.health,2,110,12)
      printb("웃"..self.armor,2,120,3)

      -- keys
      for item,amount in pairs(self.inventory) do
        if amount>0 and item.slot then
          printb(item.icon,106,120-item.slot*10,item.hudcolor)
        end
      end

      -- set "pain" palette (defaults to screen palette if normal)
      memcpy(0x5f10,0x4400|hit_ttl<<4,16)
    end,
    hit=function(self,dmg,...)
      -- call parent function
      -- + skill adjustment
      local hp=thing.hit(self,dmg_factor*dmg,...)
      if hp>0 then
        hit_ttl=min(ceil(hp),15)
        _cam:shake()
      end
    end
  },{__index=thing})
end

function draw_bsp()
  cls()
  --
  -- draw bsp & visible things
  -- 
  local pvs,v_cache=_plyr.ssector.pvs,{}

  local sorted_things=setmetatable({},depthsorted_cls)
  local m1,m2,m3,m4,m5,m6,m7=unpack(_cam.m)
  for _,thing in pairs(_things) do
    -- visible?
    local viz
    for sub,_ in pairs(thing.subs) do
      local id=sub.id
      if(band(pvs[id\32],0x0.0001<<(id&31))!=0) viz=true break
    end
    if viz then
      local x,y=thing[1],thing[2]
      local ax,az=m1*x+m2*y+m3,m5*x+m6*y+m7
      if az>_znear and 2*ax<az and -2*ax<az then
        -- h: thing offset+cam offset
        local w,h=128/az,thing[3]+m4
        local x,y=63.5+ax*w,63.5-h*w
        -- insertion sort into each sub
        for sub,_ in pairs(thing.subs) do
          -- get start of linked list
          local head=sorted_things[sub][1]
          -- empty list case
          local prev=head
          while head and head.w<w do
            -- swap/advance
            prev,head=head,head.next
          end
          -- insert new thing
          prev.next={thing=thing,x=x,y=y,w=w,next=prev.next}
        end
      end
    end
  end

  -- visit bsp
  visit_bsp(_bsp,_plyr,function(node,side,pos,visitor)
    side=not side
    if node.leaf[side] then
      local subs=node[side]
      -- potentially visible?
      local id=subs.id
      -- use band to support gaps (nil) in pvs hash
      if band(pvs[id\32],0x0.0001<<(id&31))!=0 then
        draw_flats(v_cache,subs,sorted_things[subs])
      end
    elseif _cam:is_visible(node.bbox[side]) then
      visit_bsp(node[side],pos,visitor)
    end
  end)
end

-->8
-- game states
function next_state(fn,...)
  local u,d,i=fn(...)
  do_async(function()
    -- init function?
    -- usefull for "breaking" state transitions
    if(i) i()
    _update_state,_draw_state=u,d
  end)
end

-- level selection
function levelmenu_state()
  return make_menu(
    "wHICH ePISODE?:",
    _maps_label,
    function(map_id)
      next_state(skillmenu_state,map_id)
    end)
end

-- skill selection
function skillmenu_state(map_id)
  return make_menu(
  "sELECT sKILL lEVEL:",
  {
    "i AM TOO YOUNG TO DIE",
    "hEY, NOT TOO ROUGH",
    "hURT ME PLENTY",
    "uLTRA-vIOLENCE"
  },function(skill)
    do_async(function()
      -- clear seed
      memset(0x7fc0,0,64)
      wait_async(20)
      next_state(play_state,skill,map_id)
    end)
  end)
end

function make_menu(title,options,fn)
  local colors,sel,loading={
    [7]=10,
    [10]=9,
    [9]=8,
    [8]=2,
    [2]=1,
    [1]=0},1
  
  return 
    -- update
    function()
      if(btnp(2)) sel-=1
      if(btnp(3)) sel+=1
      if not loading and (btnp(5) or btnp(4)) then
        -- avoid reentrancy
        loading=true
        -- callback
        fn(sel)
      end
      sel=mid(sel,1,#options)
    end,
    -- draw
    function()
      rectfill(0,0,127,99,0)
      spr(160,0,14,16,4)
      
      printb(title,30,50,4,2)

      for i,txt in pairs(options) do 
        local y=50+i*10
        printb(txt,30,y,9,4)
        if(sel==i) sspr(flr(time()%2)*10,112,10,10,18,y-1,10,10)
      end

      -- doom fire!
      -- credits: https://fabiensanglard.net/doom_fire_psx/index.html
      for x=0,127 do
        for y=127,100,-1 do
          local c=pget(x,y)
          -- decay
          pset((x+rnd(2)-1)&127,y-1,rnd()>0.5 and colors[c] or c)
        end
      end
    end,
    -- init
    function()
      cls()
      pal()
      reload()
      -- seed line
      memset(0x7fc0,0x77,64)
    end
end

function play_state(skill,map_id)
  cls()  
  printb("loading...",44,120,6,5)
  flip()

  -- not already loaded?
  if not _actors then
    _actors,_sprite_cache=unpack_actors()
  end
  -- fix garbage sprites when loading 2nd map
  _sprite_cache:clear()

  -- ammo scaling factor
  _ammo_factor=split"2,1,1,1"[skill]
  _bsp,thingdefs=unpack_map(skill,_actors,_maps_cart[map_id],_maps_offset[map_id])
  -- reset active things
  _things,_plyr={}

  -- attach behaviors to things
  for _,thingdef in pairs(thingdefs) do 
    local thing,actor=make_thing(unpack(thingdef))
    -- get direct access to player
    if actor.id==1 then
      _plyr=attach_plyr(thing,actor,skill)
      thing=_plyr
    end
    add(_things,thing)
  end
  assert(_plyr,"missing player in level")

  _cam=make_camera()

  -- start level music (if any)
  music(_maps_music[map_id])

  return 
    -- update
    function()
      if _plyr.dead then
        next_state(gameover_state,_plyr,_plyr.angle,_plyr.target,45)
      end

      _cam:track(_plyr,_plyr.angle,_plyr[3]+45)
      _cam:update()
    end,
    -- draw
    function()
      draw_bsp()
      _plyr:hud()

      if(_msg) print(_msg,64-#_msg*2,120,15)

      -- debug messages
      local cpu=stat(1).."|"..stat(0)
    
      print(cpu,2,3,3)
      print(cpu,2,2,15)    
    end
end

function gameover_state(pos,angle,target,h)
  local target_angle=angle
  return
    -- update
    function()
      -- fall to ground
      h=lerp(h,10,0.2)
      -- track 'death' instigator
      if target then
        target_angle=atan2(-target[1]+pos[1],target[2]-pos[2])+0.5
        angle=lerp(shortest_angle(target_angle,angle),target_angle,0.08)
      end
      _cam:track(pos,angle,pos[3]+h)
      _cam:update()

      if btnp(4) or btnp(5) then
        next_state(slicefade_state,levelmenu_state)
      end
    end,
    -- draw
    function()
      draw_bsp()

      -- set screen palette
      -- pal({140,1,139,3,4,132,133,7,6,134,5,8,2,9,10},1)
      memcpy(0x5f10,0x4400,16)
    end
end

function slicefade_state(...)
  local args=pack(...)
  local ttl,r,h,rr=30,{},{},0
  for i=0,127 do
    rr=lerp(rr,rnd(0.1),0.3)
    r[i],h[i]=0.1+rr,0
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

-->8
-- game loop
function _init()
  next_state(levelmenu_state)
end

function _update()
  -- any futures?
  local tmp={}
	for k,f in pairs(_futures) do
		local cs=costatus(f)
		if cs=="suspended" then
      assert(coresume(f))
      add(tmp,f)
		end
  end
  _futures=tmp

  -- keep world running
  for _,thing in pairs(_things) do
    if(thing.control) thing:control()
    thing:tick()
    if(thing.update) thing:update()
  end

  _update_state()

end

function _draw()
  _draw_state()
  _msg=nil
end

-->8
-- 3d functions
local function v_clip(v0,v1,t)
  local invt=1-t
  local x,y,z=
    v0[1]*invt+v1[1]*t,
    v0[2]*invt+v1[2]*t,
    v0[3]*invt+v1[3]*t
    local w=128/z
    return {
      x,y,z,
      x=63.5+x*w,
      y=63.5-y*w,
      u=v0.u*invt+v1.u*t,
      v=v0.v*invt+v1.v*t,
      w=w,
      seg=v0.seg
    }
end

function z_poly_clip(znear,v)
  local res,v0={},v[#v]
	local d0=v0[3]-znear
	for i=1,#v do
		local v1=v[i]
		local d1=v1[3]-znear
		if d1>0 then
      if d0<=0 then
        res[#res+1]=v_clip(v0,v1,d0/(d0-d1))
      end
			res[#res+1]=v1
		elseif d0>0 then
      res[#res+1]=v_clip(v0,v1,d0/(d0-d1))
    end
		v0,d0=v1,d1
	end
	return res
end

-->8
-- unpack map
local cart_id,mem=0
function mpeek()
	if mem==0x4300 then
    cart_id+=1
		reload(0,0,0x4300,mod_name.."_"..cart_id..".p8")
		mem=0
	end
	local v=@mem
	mem+=1
	return v
end

-- w: number of bytes (1 or 2)
function unpack_int(w)
  w=w or 1
	local i=w==1 and mpeek() or mpeek()<<8|mpeek()
  return i
end
-- unpack 1 or 2 bytes
function unpack_variant()
	local h=mpeek()
	-- above 127?
  if h&0x80>0 then
    h=(h&0x7f)<<8|mpeek()
  end
	return h
end
-- unpack a fixed 16:16 value
function unpack_fixed()
	return mpeek()<<8|mpeek()|mpeek()>>8|mpeek()>>16
end

-- unpack an array of bytes
function unpack_array(fn)
	for i=1,unpack_variant() do
		fn(i)
	end
end

-- returns an array of 2d vectors 
function unpack_bbox()
  local t,b,l,r=unpack_fixed(),unpack_fixed(),unpack_fixed(),unpack_fixed()
  return {l,b,l,t,r,t,r,b}
end

function unpack_special(special,line,sectors,actors)
  local function switch_texture()
    line[true].midtex=_onoff_textures[line[true].midtex]
  end
  -- helper function - handles lock & repeat
  local function trigger_async(fn,actorlock)
    return function(thing)
      -- need lock?
      if actorlock then
        -- keep key in inventory (for reusable locked doors)
        if not thing.inventory[actorlock] then 
          _msg="need key"
          -- play "err" sound
          sfx(62)
          return
        end
      end

      -- backup trigger
      local trigger=line.trigger
      -- avoid reentrancy
      line.trigger=nil
      --
      switch_texture()
      do_async(function()
        fn()
        -- unlock (if repeatable)
        if(line.flags&32>0) line.trigger=trigger switch_texture()
      end)
    end
  end

  if special==202 then
    -- sectors
    local doors={}
    -- backup heights
    unpack_array(function()
      local sector=sectors[unpack_variant()]
      -- safe for replay init values
      sector.close=sector.close or sector.floor
      sector.open=sector.open or sector.ceil
      add(doors,sector)
    end)
    local speed,kind,delay,lock=mpeek(),mpeek(),mpeek(),unpack_variant()
    local function move_async(to,speed)
      -- play open/close sound
      sfx(63)

      -- take current values
      local ceils={}
      for _,sector in pairs(doors) do
        ceils[sector]=sector.ceil
      end
      -- lerp from current values
      for i=0,speed do
        for _,sector in pairs(doors) do
          sector.ceil=lerp(ceils[sector],sector[to],i/speed)
        end
        yield()
      end
    end
    -- init
    if kind&2==0 then
      move_async("close",1)
    else
      move_async("open",1)
    end
    return trigger_async(function()  
      if kind&2==0 then
        move_async("open",speed)
      else
        move_async("close",speed)
      end
      if kind==0 then
        wait_async(delay)
        move_async("close",speed)
      elseif kind==2 then
        wait_async(delay)
        move_async("open",speed)
      end
    end,
    -- lock id 0 is no lock
    actors[lock])  
  elseif special==64 then
    -- todo: unify with doors??
    -- elevator raise
    local sector,target_floor,speed=sectors[unpack_variant()],unpack_fixed(),128-mpeek()
    -- backup initial height
    local orig_floor=sector.floor
    local function move_async(from,to)
      -- todo: configurable wait time
      wait_async(30)
      -- play open/close sound
      sfx(63)
      for i=0,speed do
        sector.floor=lerp(from,to,smoothstep(i/speed))
        yield()
      end
      -- avoid drift
      sector.floor=to
    end
    return function()
      -- avoid reentrancy
      if(sector.moving) return
      -- lock (from any other trigger)
      sector.moving=true
      do_async(function()
        move_async(orig_floor,target_floor)
        move_async(target_floor,orig_floor)
        sector.moving=nil
      end)
    end
  elseif special==80 then
    local arg0,arg1=mpeek(),mpeek()
    return trigger_async(function()
      -- sfx(0)
      -- todo: trigger action

      do_async(function()
        
      end)
    end)
  elseif special==112 then
    -- sectors
    local target_sectors={}
    unpack_array(function()
      add(target_sectors,sectors[unpack_variant()])
    end)
    local lightlevel=mpeek()/255
    return trigger_async(function()
      for _,sector in pairs(target_sectors) do
        sector.lightlevel=lightlevel
      end
    end)
  elseif special==243 then
    -- exit level
    return trigger_async(function()
      -- return to main menu
      -- todo: go to next level or end game
      next_state(slicefade_state,levelmenu_state)
    end)
  end
end

function unpack_texture()
  local tex=unpack_fixed()
  return tex!=0 and tex
end

function unpack_actors()
  -- jump to data cart
  cart_id,mem=0,0
  reload(0,0,0x4300,mod_name.."_"..cart_id..".p8")
  
  -- sprite index
	local frames,tiles={},{}
  unpack_array(function()
    -- packed:
    -- width/transparent color
    -- xoffset/yoffset in tiles unit (16x16)
    local wtc=mpeek()
		local frame=add(frames,{wtc&0xf,(mpeek()-128)/16,(mpeek()-128)/16,flr(wtc>>4),{}})
		unpack_array(function()
			-- tiles index
			frame[5][mpeek()]=unpack_variant()
    end)
  end)
  -- sprite tiles
	unpack_array(function()
		-- 16 rows of 2*8 pixels
		for k=0,31 do
			add(tiles,unpack_fixed())
		end
  end)
  
  -- inventory & things
  local actors={}
  local unpack_actor_ref=function()
    return actors[unpack_variant()]
  end

  unpack_array(function()
    local kind,id,state_labels,states,weapons,active_slot,inventory=unpack_variant(),unpack_variant(),{},{},{}
    local item={
      id=id,
      kind=kind,
      radius=unpack_fixed(),
      height=unpack_fixed(),
      mass=100,
      -- flags layout:
      -- 0x1: solid
      -- 0x2: shootable
      -- 0x4: missile
      flags=mpeek(),
      -- attach actor to this thing
      attach=function(self,thing)
        -- vm state (starts at spawn)
        local i,ticks=state_labels[0],-2

        -- extend properties
        thing=setmetatable({
          actor=self,
          health=self.health,
          armor=self.armor,
          active_slot=active_slot,
          -- pickable things
          pickup=self.pickup,  
          -- ****************** 
          -- decorate vm engine       
          -- goto vm label
          jump_to=function(self,label)
            i,ticks=state_labels[label],-2
          end,
          -- vm update
          tick=function(self)
            while ticks!=-1 do
              -- wait
              if(ticks>0) ticks-=1 return
              -- done, next step
              if(ticks==0) i+=1
::loop::
              local state=states[i]
              -- stop (or end of vm instructions)
              if(not state or state.jmp==-1) do_async(function() del(_things,self) end) return
              -- loop or goto
              if(state.jmp) self:jump_to(state.jmp) goto loop

              -- effective state
              self.state=state
              -- get ticks
              ticks=state[1]
              -- trigger function (if any)
              if(state.fn) state.fn(self)
            end
          end
        },{__index=thing})

        local function clone(coll,name)
          if coll then
            thing[name]={}
            for k,v in pairs(coll) do
              thing[name][k]=v
            end  
          end
        end

        -- clone startup inventory
        clone(inventory,"inventory")
        -- clone weapons (to avoid changing actor definition)
        clone(weapons,"weapons")

        return thing
      end
    }

    -- actor properties + skill ammo factor
    local properties,properties_factory=unpack_fixed(),{
      {0x0.0001,"health"},
      {0x0.0002,"armor"},
      {0x0.0004,"amount"},
      {0x0.0008,"maxamount"},
      -- convert icon code into character
      {0x0.0010,"icon",function() return chr(mpeek()) end},
      {0x0.0020,"slot",mpeek},
      {0x0.0040,"ammouse"},
      {0x0.0080,"speed"},
      {0x0.0100,"damage"},
      {0x0.0200,"ammotype",unpack_actor_ref},
      {0x0.0800,"mass"},
      -- some actor have multiple sounds (weapon for ex.)
      {0x0.1000,"pickupsound"},
      {0x0.2000,"attacksound"},
      {0x0.4000,"hudcolor"},
      {0x0.8000,"deathsound"},
      {0x0.0400,"",function()
        -- 
        unpack_array(function()
          local startitem,amount=unpack_actor_ref(),unpack_variant()
          if startitem.kind==2 then
            -- weapon
            weapons=weapons or {}
            -- create a new "dummy" thing
            local weapon_thing=startitem:attach({})
            weapons[startitem.slot]=weapon_thing
            -- force 'ready' state
            weapon_thing:jump_to(7)
            -- set initial weapon selection
            if(not active_slot) active_slot=startitem.slot
          else
            inventory=inventory or {}
            inventory[startitem]=amount
          end
        end)
      end}
    }
    -- decode 
    for _,props in ipairs(properties_factory) do
      local mask,k,fn=unpack(props)
      if mask&properties!=0 then
        -- unpack
        item[k]=(fn or unpack_variant)()
      end
    end
    local function pickup(owner,ref,qty,maxqty)
      ref=ref or item
      owner[ref]=min((owner[ref] or 0)+(qty or item.amount),maxqty or item.maxamount)
      if(item.pickupsound) sfx(item.pickupsound)
    end
    
    if kind==0 then
      -- default inventory item (ex: lock)
      item.pickup=function(thing,target)
        pickup(target.inventory)
      end
    elseif kind==1 then
      -- ammo family
      local ammotype=unpack_actor_ref()
      item.pickup=function(thing,target)
        pickup(target.inventory,ammotype,_ammo_factor*item.amount)
      end
    elseif kind==2 then
      -- weapon
      local ammogive,ammotype=unpack_variant(),item.ammotype
      item.pickup=function(thing,target)
        pickup(target.inventory,ammotype,_ammo_factor*ammogive,ammotype.maxamount)

        target:attach_weapon(thing)
        -- remove from things
        do_async(function() del(_things,thing) end)
      end
    elseif kind==3 then
      -- health pickup
      item.pickup=function(thing,target)
        pickup(target,"health")
      end
    elseif kind==4 then
      -- armor pickup
      item.pickup=function(thing,target)
        pickup(target,"armor")
      end
    end

    -- actor states
    unpack_array(function()
      -- map label id to state command line number
      state_labels[mpeek()]=mpeek()
    end)

    -- actors functions
    local function_factory={
      -- A_FireBullets
      function()
        local xspread,yspread,bullets,dmg,puff=unpack_fixed(),unpack_fixed(),mpeek(),mpeek(),unpack_actor_ref()
        return function(owner)
          -- find 'real' owner
          owner=owner.owner or owner
          for i=1,bullets do
            local angle=owner.angle+(rnd(2*xspread)-xspread)/360
            local hits,move_dir={},{cos(angle),-sin(angle)}
            intersect_sub_sector(owner.ssector,owner,move_dir,owner.actor.radius/2,1024,hits)    
            -- todo: get from actor properties
            local h=owner[3]+32
            for _,hit in ipairs(hits) do
              local fix_move
              if hit.seg then
                -- bsp hit?
                local ldef=hit.seg.line
                local otherside=ldef[not hit.seg.side]
    
                if otherside==nil or 
                  h>otherside.sector.ceil or 
                  h<otherside.sector.floor then
                  fix_move=hit
                end              
              elseif hit.thing!=owner then
                -- thing hit?
                local otherthing=hit.thing
                local otheractor=otherthing.actor
                -- within height?
                if otheractor.flags&0x1>0 and 
                  h>otherthing[3] and 
                  h<otherthing[3]+otheractor.height then
                  -- solid actor?
                  fix_move=hit
                end
              end
    
              if fix_move then
                -- actual hit position
                local pos={owner[1],owner[2],h}
                v2_add(pos,move_dir,fix_move.t)
                local puffthing=make_thing(puff,pos[1],pos[2],0,angle)
                -- todo: get height from properties
                -- todo: improve z setting
                puffthing[3]=h
                add(_things,puffthing)
      
                -- hit thing
                local otherthing=fix_move.thing
                if(otherthing and otherthing.hit) otherthing:hit(dmg,move_dir,owner)
                break
              end
            end
          end
        end
      end,
      -- A_PlaySound
      function()
        local s=mpeek()
        return function()
          sfx(s)
        end
      end,
      -- A_FireProjectile
      function()
        local projectile=unpack_actor_ref()
        return function(owner)
          -- find 'real' owner
          owner=owner.owner or owner
          local angle,speed,radius=owner.angle,projectile.speed,owner.actor.radius/2
          local ca,sa=cos(angle),-sin(angle)
          -- fire at edge of owner radius
          local thing=with_physic(make_thing(projectile,owner[1]+radius*ca,owner[2]+radius*sa,0,angle))
          thing.owner=owner
          -- todo: get height from properties
          -- todo: improve z setting
          thing[3]=owner[3]+32
          thing:apply_forces(speed*ca,speed*sa)         
          add(_things,thing)
        end
      end,
      -- A_WeaponReady
      function()
        return function(weapon)
          if btn(❎) then
            local inventory=weapon.owner.inventory
            local newqty=inventory[item.ammotype]-item.ammouse
            if newqty>=0 then
              inventory[item.ammotype]=newqty
              -- play attack sound
              if(item.attacksound) sfx(item.attacksound)
              -- fire state
              weapon:jump_to(9)
            end
          end
        end
      end,
      -- A_Explode
      function()
        local dmg,maxdist=unpack_variant(),unpack_variant()
        return function(thing)
          -- todo: optimize lookup!!!
          for _,otherthing in pairs(_things) do
            if otherthing!=thing and otherthing.hit then
              local n,d=line_of_sight(thing,otherthing,maxdist)
              if(d) otherthing:hit(dmg*(1-d/maxdist),n)
            end
          end
        end
      end,
      -- A_FaceTarget
      function()
        local speed=mpeek()/255
        return function(thing)
          -- nothing to face to
          local otherthing=thing.target
          if(not otherthing) return
          local target_angle=atan2(-thing[1]+otherthing[1],thing[2]-otherthing[2])
          thing.angle=lerp(shortest_angle(target_angle,thing.angle),target_angle,speed)
        end
      end,
      -- A_Look
      function()
        return function(self)
          for ptgt in all({self.target,_plyr}) do
            if(ptgt and not ptgt.dead) otherthing=ptgt break
          end
          -- nothing to do?
          if(not otherthing) self.target=nil return
          -- in range/visible?
          local n,d=line_of_sight(self,otherthing,1024)
          if d then
            self.target=otherthing
            -- see
            self:jump_to(2)
          end 
        end
      end,
      -- A_Chase
      function()
        local speed=item.speed
        return function(self)
          -- still active target?
          local otherthing=self.target
          if otherthing and not otherthing.dead then
            -- in range/visible?
            local n,d=line_of_sight(self,otherthing,1024)
            if d and d<512 and rnd()<0.4 then
              -- missile attack
              self:jump_to(4)
            else
              -- zigzag toward target
              local nx,ny,dir=n[1]*0.5,n[2]*0.5,rnd{1,-1}
              local mx,my=ny*dir+nx,nx*-dir+ny
              local target_angle=atan2(mx,-my)
              self.angle=lerp(shortest_angle(target_angle,self.angle),target_angle,0.5)
              self:apply_forces(speed*mx,speed*my)
            end
            return
          end
          -- lost/dead?
          self.target=nil
          -- spawn
          self:jump_to(0)
        end
      end,
      -- A_Light
      function()
        local light=mpeek()/255
        return function()
          _ambientlight=light
        end
      end
    }
    -- states & sprites
    unpack_array(function()
      local flags=mpeek()
      -- default cmd: stop
      local ctrl,cmd=flags&0x3,{jmp=-1}
      if ctrl==2 then
        -- loop or goto label id   
        cmd={jmp=flr(flags>>4)}
      elseif ctrl==0 then
        -- normal command
        -- todo: use a reference to sprite sides (too many duplicates for complex states)
        -- or merge sides into array
        -- layout:
        -- 1 ticks
        -- 2 flipx
        -- 3 light level (bright/normal)
        -- 4 number of sides
        -- 5+ sides
        cmd={mpeek()-128,mpeek(),flags&0x4>0,0}
        -- get all pose sides
        unpack_array(function(i)
          add(cmd,frames[unpack_variant()])
          -- number of sides
          cmd[4]=i
        end)
        -- function?
        if flags&0x8>0 then
          cmd.fn=function_factory[mpeek()]()
        end
      end
      add(states,cmd)
    end)

    -- register
    actors[id]=item
  end)
  return actors,make_sprite_cache(tiles,32)
end

-- unpack level data (geometry + things)
function unpack_map(skill,actors,map_cart_id,map_cart_offset)
  -- jump to map cart
  cart_id,mem=map_cart_id,map_cart_offset
  reload(0,0,0x4300,mod_name.."_"..cart_id..".p8")
  
  -- sectors
  local sectors,sides,verts,lines,sub_sectors,all_segs,nodes={},{},{},{},{},{},{}
  unpack_array(function(i)
    local special=mpeek()
    local sector=add(sectors,{
      id=i,
      -- sector attributes
      special=special,
      -- ceiling/floor height
      ceil=unpack_int(2),
      floor=unpack_int(2),
      ceiltex=unpack_texture(),
      floortex=unpack_texture(),
      -- rebase to 0-1
      lightlevel=mpeek()/255
    })
    -- sector behaviors (if any)
    if special==65 then
      local lights={sector.lightlevel,0}
      do_async(function()
        while true do
          sector.lightlevel=rnd(lights)
          wait_async(rnd(15))
        end
      end)
    end
  end)

  -- sidedefs
  unpack_array(function()
    add(sides,{
      sector=sectors[unpack_variant()],
      toptex=unpack_texture(),
      midtex=unpack_texture(),
      bottomtex=unpack_texture()
    })
  end)

  -- vertices
  unpack_array(function()
    add(verts,{unpack_fixed(),unpack_fixed()})
  end)
  
  -- linedefs
  unpack_array(function()
    local line={
      -- sides
      [true]=sides[unpack_variant()],
      [false]=sides[unpack_variant()],
      flags=mpeek()}
    -- special actions
    if line.flags&0x2>0 then
      line.trigger=unpack_special(mpeek(),line,sectors,actors)
    end
    add(lines,line)
  end)

  -- convex sub-sectors
  unpack_array(function(i)
    -- register current sub-sector in pvs
    local segs={id=i,pvs={}}
    unpack_array(function()
      local s=add(segs,{
        -- 1: vertex
        verts[unpack_variant()],
      })
      local flags=mpeek()
      s.side=flags&0x1==0
      -- optional links
      if(flags&0x2>0) s.line=lines[unpack_variant()]
      if(flags&0x4>0) s.partner=unpack_variant()
      
      -- direct link to sector (if not already set)
      if s.line and not segs.sector then
        segs.sector=s.line[s.side].sector
      end
      --assert(s.v0,"invalid seg")
      --assert(segs.sector,"missing sector")
      add(all_segs,s)
    end)
    -- pvs (packed as a bit array)
    unpack_array(function()
      local id=unpack_variant()
      local mask=segs.pvs[id\32] or 0
      segs.pvs[id\32]=mask|0x0.0001<<(id&31)
    end)
    -- normals
    local s0=segs[#segs]
    local v0=s0[1]
    for i,s1 in ipairs(segs) do
      local v1=s1[1]
      local n,len=v2_normal(v2_make(v0,v1))
      -- 2: segment dir
      add(s0,n)
      -- 3: dist to origin
      add(s0,v2_dot(n,v0))
      -- 4: len
      add(s0,len)
      -- normal
      n={-n[2],n[1]}
      -- 5: normal
      add(s0,n)
      -- 6: distance to origin
      add(s0,v2_dot(n,v0))
      -- 7: use normal direction to select uv direction
      add(s0,abs(n[1])>abs(n[2]) and "v" or "u")

      v0,s0=v1,s1
    end
    add(sub_sectors,segs)
  end)
  -- fix seg -> sub-sector link (e.g. portals)
  for _,seg in pairs(all_segs) do
    seg.partner=sub_sectors[seg.partner]
  end
  -- bsp nodes
  unpack_array(function()
    local node=add(nodes,{
      -- normal packed in struct to save memory
      unpack_fixed(),unpack_fixed(),
      -- distance to plane
      unpack_fixed(),
      bbox={},
      leaf={}})
    local flags=mpeek()
    local function unpack_node(side,leaf)
      if leaf then
        node.leaf[side]=true
        node[side]=sub_sectors[unpack_variant()]
      else
        -- bounding box only on non-leaves
        node.bbox[side]=unpack_bbox()
        node[side]=nodes[unpack_variant()]
      end
    end
    unpack_node(true,flags&0x1>0)
    unpack_node(false,flags&0x2>0)
  end)
  
  -- texture pairs
  unpack_array(function()
    _onoff_textures[unpack_fixed()]=unpack_fixed()
  end)

  -- things
  local things={}
  unpack_array(function()
    local flags,id,x,y=mpeek(),unpack_variant(),unpack_fixed(),unpack_fixed()
    if flags&(0x10<<(skill-1))!=0 then
      add(things,{
        -- link to underlying actor
        actors[id],
        -- coordinates
        x,y,
        -- height
        0,
        -- angle
        (flags&0xf)/8,
      })
    end
  end)

  -- restore main cart
  reload()
  return nodes[#nodes],things
end
