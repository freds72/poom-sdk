-- globals
local _slow,_ambientlight,_drag,_ammo_factor,_intersectid,_onoff_textures,_transparent_textures,_futures,_things,_btns,_bsp,_cam,_plyr,_sprite_cache,_actors,_wp_hud,_msg=0,0,0,1,0,{[0]=0},{},{},{},{}

--local k_far,k_near=0,2
--local k_right,k_left=4,8

-- copy color gradients (16*16 colors x 2) to memory + skybox image (if any)
memcpy(0x4300,0x0,4096)

-- create a new instance with parent properties
function inherit(t,parent)
  return setmetatable(t,{__index=parent})
end

function lerp(a,b,t)
  -- faster by 1 cycle
  return a+t*(b-a)
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

-- registers a new coroutine
-- returns a handle to the coroutine
-- used to cancel a coroutine
function do_async(fn)
  return add(_futures,{co=cocreate(fn)})
end
-- wait until timer
function wait_async(t)
	for i=1,t do
		yield()
	end
end

-- 2d vector functions
function v2_dot(a,b)
  return a[1]*b[1]+a[2]*b[2]
end

function v2_normal(v)
  -- safe vector len
  local dx,dy=abs(v[1]),abs(v[2])
  local d=max(dx,dy)
  local n=min(dx,dy)/d
  local len=d*sqrt(n*n + 1)
  return {v[1]/len,v[2]/len},len
end

function v2_add(a,b,scale)
  scale=scale or 1
  a[1]+=scale*b[1]
  a[2]+=scale*b[2]
end

function v2_make(a,b)
  return {b[1]-a[1],b[2]-a[2]}
end

-- bold print helper
function printb(txt,x,y,c1,c2)
  ?txt,x,y+1,c2
  ?txt,x,y,c1
end

-->8
-- virtual sprites
function vspr(frame,sx,sy,scale,flipx)
  -- faster equivalent to: palt(0,false)
  poke(0x5f00,0)
  local xscale,w,xoffset,yoffset,tc,tiles=scale,unpack(frame)
  palt(tc,true)
  if(flipx) xoffset,xscale=1-xoffset,-scale
  sx-=xoffset*scale
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
function make_sprite_cache(tiles)
  -- pre-generated poke4 offsets
  -- for j=0,31 do
  --   poke4(mem|(j&1)<<2|(j\2)<<6,tiles[id+j])
  -- end	
	local len,index,offsets,first,last=0,{},split("4,64,68,128,132,192,196,256,260,320,324,384,388,448,452,512,516,576,580,640,644,704,708,768,772,832,836,896,900,960,964",",",1)
  offsets[0]=0

  local function remove(t)
    -- note: keep multiline assignments, they are *faster*
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
    use=function(self,id)
			local entry=index[id]
			if entry then
				-- existing item?
				-- force refresh
				remove(entry)
			else
				-- allocate a new 16x16 entry
				local sx,sy=(len<<4)&127,(len\8)<<4
        -- list too large?
        -- 32: cache max entry size
				if len>31 then
					local old=remove(first)
					-- reuse cache entry
					sx,sy,index[old.id]=old.sx,old.sy
				end
				-- new (or relocate)
				-- copy data to sprite sheet
				local mem=sx\2|sy<<6
				for j,offset in pairs(offsets) do
					poke4(mem|offset,tiles[id+j])
				end		
				--
				entry={sx=sx,sy=sy,id=id}
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
			return entry.sx,entry.sy
		end
	}
end

-->8
-- bsp rendering

-- traverse and renders bsp in back to front order
-- calls 'visit' function
function visit_bsp(node,pos,visitor)
  local side=v2_dot(node,pos)<=node[3]
  visitor(node,not side,pos,visitor)
  visitor(node,side,pos,visitor)
end

function find_sub_sector(node,pos)
  while true do
    local side=v2_dot(node,pos)<=node[3]
    if node.leaf[side] then
      -- leaf?
      return node[side]
    end
    node=node[side]
  end
end

-- floor/ceiling n-gon filling routine
-- xoffset: used as texture offset
-- yoffset: height
-- if tex is 0: renders skybox gradient
function polyfill(v,xoffset,yoffset,tex,light)
  poke4(0x5f38,tex)
  local _tline,_memcpy=tline,memcpy
  if tex==0 then
    pal()
    _memcpy,_tline=peek,function(x0,y0,x1) 
      if(y0>_sky_height) y0=_sky_height
      rectfill(x0,y0,x1,y0,@(_sky_offset+y0))
    end
  end

  local v0,spans,ca,sa,cx,cy,cz,pal0=v[#v],{},_cam.u,_cam.v,(_plyr[1]>>4)+xoffset,(-_cam.m[4]-yoffset)<<3,_plyr[2]>>4
  local x0,w0=v0.x,v0.w
  local y0=v0.y-yoffset*w0
  for i,v1 in ipairs(v) do
    local x1,w1=v1.x,v1.w
    local y1=v1.y-yoffset*w1
    local _x1,_y1,_w1=x1,y1,w1
    if(y0>y1) x1=x0 y1=y0 w1=w0 x0=_x1 y0=_y1 w0=_w1
    local dy=y1-y0
    local cy0,dx,dw=y0\1+1,(x1-x0)/dy,(w1-w0)/dy
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
          -- collect boundaries + color shitfint + mode 7
          local wlight=w0<<5
          if(wlight>15) wlight=15
          local a,b,rz,pal1=x0,span,cy/(y-63.5),(light*wlight)\1
          if(a>b) a=span b=x0
          -- color shifing
          if(pal0!=pal1) _memcpy(0x5f00,0x4300|pal1<<4,16) pal0=pal1

          -- mode7 texturing
          local rx=rz*(a\1-63.5)>>7
        
          -- camera space
          _tline(a,y,b,y,ca*rx+sa*rz+cx,ca*rz-sa*rx+cz,ca*rz>>7,-sa*rz>>7)   
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

local function v_clip(v0,v1,t)
  local invt=1-t
  local x,y=
    v0.xx*invt+v1.xx*t,
    v0.yy
    --local w=128/z
    return {
      -- z is clipped to near plane
      xx=x,yy=y,zz=8,
      x=63.5+(x<<4),
      y=63.5-(y<<4),
      u=v0.u*invt+v1.u*t,
      v=v0.v*invt+v1.v*t,
      -- 128/8
      w=16,
      seg=v0.seg
    }
end

-- ceil/floor/wall rendering
function draw_flats(v_cache,segs)
  local verts,outcode,nearclip,px,py,m1,m3,m4,m8,m9,m11,m12={},0xffff,0,_plyr[1],_plyr[2],unpack(_cam.m)
  
  -- to cam space + clipping flags
  for i,seg in ipairs(segs) do
    local v0=seg[1]
    local v=v_cache[v0]
    if not v then
      local code,x,z=2,v0[1],v0[2]
      local ax,az=
        m1*x+m3*z+m4,
        m9*x+m11*z+m12
      if(az>8) code=0
      if(az>854) code|=1
      -- fov adjustment
      if(-ax<<1>az) code|=4
      if(ax<<1>az) code|=8
      
      local w=128/az
      -- >>4 to fix u|v*w overflow on large maps 
      v={xx=ax,yy=m8,zz=az,outcode=code,u=x>>4,v=z>>4,x=63.5+ax*w,y=63.5-m8*w,w=w}
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
    if nearclip!=0 then
      -- near clipping required?
      local res,v0={},verts[#verts]
      local d0=v0.zz-8
      for i,v1 in ipairs(verts) do
        local d1=v1.zz-8
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
      verts=res
    end

    if #verts>2 then
      local sector,pal0=segs.sector
      local floor,ceil,light,things=sector.floor,sector.ceil,max(sector.lightlevel,_ambientlight),{}
      -- not visible or no texture?
      if(sector.floortex!=0 and floor+m8<0) polyfill(verts,sector.tx or 0,floor,sector.floortex,light)
      if(ceil+m8>0) polyfill(verts,0,ceil,sector.ceiltex,light)

      -- draw walls
      -- get heights
      local v0,top,bottom=verts[#verts],ceil>>4,floor>>4
      local x0,y0,w0=v0.x,v0.y,v0.w
    
      for i,v1 in ipairs(verts) do
        local seg,x1,y1,w1=v0.seg,v1.x,v1.y,v1.w
        local _x1,ldef=x1,seg.line
        -- logical split or wall?
        -- front facing?
        if x0<x1 and ldef then
          -- peg bottom?
          local _,toptex,midtex,bottomtex=unpack(ldef[seg.side])
          -- no need to draw untextured walls
          if toptex|midtex|bottomtex!=0 then
            -- get other side (if any)
            local yoffset,yoffsettop,otherside,otop,obottom=bottom-top,bottom-top,ldef[not seg.side]
            -- fix animated side walls (elevators)
            if ldef.dontpegtop then
              yoffset=0
            end
            if otherside then
              -- visible other side walls?
              otop,obottom=otherside[1].ceil>>4,otherside[1].floor>>4
              -- offset animated walls (doors)
              yoffset=0
              if ldef.dontpegtop then
                yoffsettop=otop-top
              end
              -- make sure bottom is not crossing this side top
              if(obottom>top) obottom=top
              if(otop<bottom) otop=bottom
              -- not visible?
              if(top<=otop) otop=nil
              if(bottom>=obottom) obottom=nil
            end
            -- span rasterization
            -- pick correct texture "major"
            local dx,u0,midtex_tc=x1-x0,v0[seg[9]]*w0,_transparent_textures[midtex]
            local cx0,dy,du,dw=x0\1+1,(y1-y0)/dx,(v1[seg[9]]*w1-u0)/dx,((w1-w0)<<4)/dx
            w0<<=4
            local sx=cx0-x0    
            if(x0<0) y0-=x0*dy u0-=x0*du w0-=x0*dw cx0=0 sx=0
            y0+=sx*dy
            u0+=sx*du
            w0+=sx*dw
            if(x1>127) x1=127
            for x=cx0,x1\1 do
              if w0>2.4 then
                -- top/bottom+perspective correct texture u+color shifing
                local wlight=w0<<1
                if(wlight>15) wlight=15
                local t,b,u,pal1=y0-top*w0,y0-bottom*w0,u0/(w0>>4),(light*wlight)\1
                if(pal0!=pal1) memcpy(0x5f00,0x4300|pal1<<4,16) pal0=pal1
    
                -- top wall side between current sector and back sector
                local ct=t\1+1
    
                if otop then
                  poke4(0x5f38,toptex)             
                  local ot=y0-otop*w0
                  tline(x,ct,x,ot,u,(ct-t)/w0+yoffsettop,0,1/w0)
                  -- new window top
                  t=ot
                  ct=ot\1+1
                end
                -- bottom wall side between current sector and back sector     
                if obottom then
                  poke4(0x5f38,bottomtex)             
                  local ob=y0-obottom*w0
                  local cob=ob\1+1
                  tline(x,cob,x,b,u,(cob-ob)/w0,0,1/w0)
                  -- new window bottom
                  b=ob
                end
        
                -- middle wall?
                if midtex!=0 then
                  -- texture selection
                  poke4(0x5f38,midtex)
                  -- enable transparent black if needed
                  poke(0x5f00,midtex_tc)
                  tline(x,ct,x,b,u,(ct-t)/w0+yoffset,0,1/w0)
                  poke(0x5f00,0)
                end   
              end
              y0+=dy
              u0+=du
              w0+=dw
            end
          end
        end
        v0=v1
        x0=_x1
        y0=y1
        w0=w1
      end

      -- draw things (if any) in this convex space
      for thing,_ in pairs(segs.things) do
        -- todo: cache thing projection
        -- done: not sure if faster (90% of things are single sub-sector...)
        --[[        
        local v=v_cache[thing]
        if not v then
          local x,y=thing[1],thing[2]
          local ax,az=m1*x+m3*y+m4,m9*x+m11*y+m12
          -- todo: take radius into account 
          if az>8 and az<854 and ax<az and -ax<az then
            -- h: thing offset+cam offset
            local w,h=128/az,thing[3]+m8
            local x,y=63.5+ax*w,63.5-h*w
            -- visible
            v={x=x,y=y,w=w}
          else
            v={}
          end
          v_cache[thing]=v
        end
        -- visible?
        local w=v.w
        if w then
          -- get start of linked list
          local head=things[1]
          -- empty list case
          local prev=head
          while head and head.w<w do
            -- swap/advance
            prev,head=head,head.next
          end
          -- insert new thing
          prev.next={thing=thing,x=v.x,y=v.y,w=w,next=prev.next}
        end
      end          
      ]]
        local x,y=thing[1],thing[2]
        local ax,az=m1*x+m3*y+m4,m9*x+m11*y+m12
        -- todo: take radius into account 
        if az>8 and az<854 and ax<az and -ax<az then
          -- default: insert at end of sorted array
          local w,thingi=128/az,#things+1
          -- basic insertion sort
          for i,otherthing in ipairs(things) do          
            if(otherthing[1]>w) thingi=i break
          end
          -- thing offset+cam offset
          add(things,{w,thing,63.5+ax*w,63.5-(thing[3]+m8)*w},thingi)
        end
      end
      -- things are sorted, draw them
      for _,head in ipairs(things) do
        local w0,thing,x0,y0=unpack(head)
        -- get image from current state
        local frame=thing.state
        local side,_,flipx,bright,sides=0,unpack(frame)
        -- use frame brightness level
        local pal1=bright and 8 or (light*min(15,w0<<5))\1
        if(pal0!=pal1) memcpy(0x5f00,0x4300|pal1<<4,16) pal0=pal1            
        -- pick side (if any)
        if sides>1 then
          local angle=((atan2(px-thing[1],thing[2]-py)-thing.angle+0.0625)%1+1)%1
          side=(sides*angle)\1
          -- get flip bit from mask
          flipx=flipx&(1<<side)!=0
        else
          flipx=nil
        end
        vspr(frame[side+5],x0,y0,w0<<5,flipx)
        
        -- thing:draw_vm(x0,y0)
        -- print(thing.angle,x0,y0,8)
      end
    end
  end
end

function add_thing(thing)
  register_thing_subs(_bsp,thing,thing.actor.radius/2)
  _things[#_things+1]=thing
end

function del_thing(thing)
  -- detach thing from sub-sector
  unregister_thing_subs(thing)
  del(_things,thing)
end

function unregister_thing_subs(thing)
  for node,_ in pairs(thing.subs) do
    if(node.things) node.things[thing]=nil
    -- remove self from sectors (multiple)
    if(thing.actor.is_solid) node.sector.things-=1
  end
end

function register_thing_subs(node,thing,radius)
  -- leaf?
  if node.in_pvs then
    -- thing -> sector
    thing.subs[node]=true
    -- reverse
    if(not node.things) node.things={}
    node.things[thing]=true
    -- register only solid actors
    if(thing.actor.is_solid) node.sector.things+=1
    return
  end

  local dist,d=v2_dot(node,thing),node[3]
  local side,otherside=dist<=d-radius,dist<=d+radius
  
  register_thing_subs(node[side],thing,radius)
  
  -- straddling?
  if side!=otherside then
    register_thing_subs(node[otherside],thing,radius)
  end
end

-- http://geomalgorithms.com/a13-_intersect-4.html
-- calls intersect_cb for each hit
-- must return true to stop intersection search
-- t: impact depth (to fix velocity)
-- ti: impact on velocity vector
function intersect_sub_sector(segs,p,d,tmin,tmax,radius,intersect_cb,skipthings)
  local intersectid,_tmax,px,py,dx,dy,othersector=_intersectid,tmax,p[1],p[2],d[1],d[2]

  if not skipthings then
    local hits={}
    for thing,_ in pairs(segs.things) do
      -- not already "hit"
      -- not a missile
      -- not dead
      if thing.intersectid!=intersectid and not thing.dead and not thing.actor.is_missile then
        -- overflow 'safe' coordinates
        local m,r={(px-thing[1])>>8,(py-thing[2])>>8},(thing.actor.radius+radius)>>8
        local b,c=v2_dot(m,d),v2_dot(m,m)-r*r

        -- check distance and ray direction vs. circle
        if c<=0 or b<=0 then
          local discr=b*b-c
          if discr>=0 then
            -- convert back to world units
            local t=(-b-sqrt(discr))<<8
            -- if t is negative, ray started inside sphere so clamp t to zero 
            if(t<tmin) t=tmin
            -- record hit
            if t>=tmin and t<tmax then
              local thingi=#hits+1
              for i=1,#hits do
                -- find first largest hit
                if(t<hits[i].t) thingi=i break
              end
              -- insert new thing
              add(hits,{ti=t,t=(radius-t)/radius,thing=thing},thingi)
            end
          end
        end
        -- avoid duplicate hits
        thing.intersectid=intersectid
      end
    end
    for _,hit in ipairs(hits) do
      if(intersect_cb(hit)) return
    end
  end

  for _,s0 in ipairs(segs) do
    local n={s0[6],s0[7]}
    local denom,dist_a=v2_dot(n,d),s0[8]-v2_dot(n,p)
    if denom>0 then
      local t=dist_a/denom
      -- within seg?
      local d=s0[2]*(px+t*dx)+s0[3]*(py+t*dy)-s0[4]
      -- extended segment
      if d>=-radius and d<s0[5]+radius then
        local dist_b,inseg=s0[8]-v2_dot(n,{px+_tmax*dx,py+_tmax*dy}),d>=0 and d<s0[5]
        -- exact segment?
        if inseg then
          if(t<tmax) tmax=t othersector=s0.partner
          -- if(tmax<tmin) return 
        end
        if s0.line and (dist_a<radius or dist_b<radius) then
          if(intersect_cb({ti=t,t=mid(dist_a/(dist_a-dist_b),0,1),dist=dist_a<radius and inseg and (radius-dist_a),seg=s0,n=n})) return
        end
      end
    end
  end

  if tmin<=tmax and tmax<_tmax and othersector then
    -- any remaining segment to check?
    intersect_sub_sector(othersector,p,d,tmax,_tmax,radius,intersect_cb,skipthings)
  end
end

-- scan attack (e.g. will hit anything in range)
function hitscan_attack(owner,angle,range,dmg,puff)
  -- todo: get height from properties
  local h,move_dir=owner[3]+32,{cos(angle),-sin(angle)}
  _intersectid+=1
  intersect_sub_sector(owner.ssector,owner,move_dir,owner.actor.radius/2,range,0,function(hit)    
    local otherthing,fix_move=hit.thing
    if hit.seg then
      fix_move=intersect_line(hit.seg,h,0,0,true,true) and hit
    elseif otherthing!=owner and intersect_thing(otherthing,h,0) then
      fix_move=hit
    end

    if fix_move then
      -- actual hit position
      local pos={owner[1],owner[2]}
      v2_add(pos,move_dir,fix_move.ti)
      local puffthing=make_thing(puff,pos[1],pos[2],h+rnd(4)-2,angle)
      add_thing(puffthing)

      -- hit thing
      if(otherthing and otherthing.hit) puffthing:jump_to(otherthing.actor.noblood and 0 or 11,0) otherthing:hit(dmg,move_dir,owner)
      return true
    end
  end)
end

-- returns distance and normal to target (if visible)
function line_of_sight(thing,otherthing,maxdist)
  -- pvs check
  local n,d=v2_normal(v2_make(thing,otherthing))
  if(not thing.ssector:in_pvs(otherthing.ssector.id)) return n

  -- in radius?
  d=max(d-thing.actor.radius)
  if d<maxdist then
    -- line of sight?
    local h=thing[3]
    -- artificially level floating actors to their target
    if(thing.actor.floating) h=otherthing[3]
    _intersectid+=1
    intersect_sub_sector(thing.ssector,thing,n,0,d,0,function(hit)
      -- cannot see
      if(intersect_line(hit.seg,h+24,0,0,true,true)) d=nil return true
    end,true)
    -- normal and distance to hit
    return n,d
  end
  return n
end

function make_thing(actor,x,y,z,angle,special)
  -- default height & sector specs
  local ss=find_sub_sector(_bsp,{x,y})
  -- attach instance properties to new thing
  local thing=actor:attach{
    -- z: altitude
    x,y,z==0x8000 and ss.sector.floor or z,
    angle=angle,
    sector=ss.sector,
    ssector=ss,
    subs={},
    trigger=special
  }
  
  -- additional wrapper if shootable
  return actor.is_shootable and with_physic(with_health(thing)) or thing,actor
end

-- sector damage
local _sector_dmg={
  [71]=5,
  [69]=10,
  [80]=20,
  [84]=5,
  -- instadeath
  [115]=-1
}

function intersect_line(seg,h,height,clearance,is_missile,is_dropoff)
  local ldef=seg.line
  local otherside=ldef[not seg.side]

  return otherside==nil or 
    -- impassable
    (not is_missile and ldef.blocking) or
    h+height>otherside[1].ceil or 
    h+clearance<otherside[1].floor or 
    -- avoid monster jumping off cliffs
    (not is_dropoff and h-otherside[1].floor>clearance)
end

function intersect_thing(otherthing,h,radius)
  local otheractor=otherthing.actor
  return otheractor.is_solid and
    h>=otherthing[3]-radius and 
    h<otherthing[3]+otheractor.height+radius
end

-- attach physic behavior
function with_physic(thing)
  local actor=thing.actor
  -- actor properties
  local height,radius,mass,is_missile,is_player=actor.height,actor.radius,2*actor.mass,actor.is_missile,actor.id==1
  local ss,friction,forces,velocity,dz=thing.ssector,is_missile and 0.9967 or 0.9062,{0,0},{0,0},0  
  return inherit({
    apply_forces=function(self,x,y,mag)
      -- todo: review arbitrary factor...
      mag<<=6
      forces[1]+=mag*x/mass
      forces[2]+=mag*y/mass
    end,    
    update=function(self)
      -- integrate forces
      v2_add(velocity,forces)
      -- alive floating actor? : track target height
      if not self.dead and actor.floating then
        dz+=rnd(1.6)-0.9
        if(self.target) dz+=mid(self.target[3]-self[3],-512,512)>>8
        -- avoid woobling
        dz*=friction
      end
      -- gravity?
      if(not actor.is_nogravity or (self.dead and not actor.is_dontfall)) dz-=1

      -- x/y friction
      local friction=is_player and friction*(1-_drag) or friction
      velocity[1]*=friction
      velocity[2]*=friction
      
      -- check collision with world
      local move_dir,move_len=v2_normal(velocity)
      
      -- cancel small moves
      if move_len>1/16 then
        local h,stair_h=self[3],is_missile and 0 or 24

        unregister_thing_subs(self)
        
        -- check intersection with actor radius
        _intersectid+=1
        intersect_sub_sector(ss,self,move_dir,0,move_len,radius,function(hit)
          local otherthing,fix_move=hit.thing
          if hit.seg then
            fix_move=intersect_line(hit.seg,h,height,stair_h,is_missile,actor.is_dropoff) and hit
            -- cross special?
            local ldef=hit.seg.line
            if is_player and ldef.trigger and ldef.playercross then
              ldef.trigger(self)
            end
          else
            if is_player and otherthing.pickup then
              otherthing.actor.pickup(otherthing,self)
            elseif self.owner!=otherthing then -- avoid projectile intersect with owner
              fix_move=intersect_thing(otherthing,h,radius) and hit
            end
          end

          if fix_move then
            if is_missile then
              -- fix position & velocity
              v2_add(self,move_dir,fix_move.ti)
              velocity={0,0}
              -- explosion sound (if any)
              if(actor.deathsound and ss:in_pvs(_plyr.ssector.id)) sfx(actor.deathsound)
              -- death state
              self:jump_to(5)
              -- hit thing
              if(otherthing and otherthing.hit) otherthing:hit((1+rnd(7))*actor.damage,move_dir,self.owner)
              -- stop at first wall/thing
              return true
            else
              local n=fix_move.n or v2_normal(v2_make(self,otherthing))
              local fix=-fix_move.t*v2_dot(n,velocity)
              -- avoid being pulled toward prop/wall
              if fix<0 then
                -- apply impulse (e.g. fix velocity)
                v2_add(velocity,n,fix)
              end
              -- too close to wall?
              if fix_move.dist then
                v2_add(self,n,-fix_move.dist)
              end
              -- colliding actor inflicts damage?
              if(otherthing and actor.damage and otherthing.hit) otherthing:hit((1+rnd(7))*actor.damage,n,self)
            end
          end
        end)

        -- trail? (at previous position)
        if actor.trailtype  then
          add_thing(make_thing(actor.trailtype,self[1],self[2],h,self.angle))
        end

        -- apply move
        v2_add(self,velocity)

        -- refresh sector after fixed collision
        ss=find_sub_sector(_bsp,self)

        self.sector,self.ssector,self.subs=ss.sector,ss,{}
        register_thing_subs(_bsp,self,radius/2)
      else
        velocity={0,0}
      end

      -- triggers?
      -- check triggers/bumps/...
      if is_player then
        intersect_sub_sector(ss,self,{cos(self.angle),-sin(self.angle)},0,radius+48,0,function(hit)
          local ldef=hit.seg.line
          -- buttons
          if ldef.trigger and ldef.playeruse then
            -- use special?
            if btnp(_btnuse) then
              ldef.trigger(self)
            end
            -- trigger/message only closest hit
            return true
          end
        end,true)    
      end

      if not is_missile then
        local h,sector=self[3]+dz,self.sector
        if h<sector.floor then
          -- found secret?
          if is_player and sector.special==195 and not _found[sector] then
            _found[sector],_msg=true,"a secret has been revealed!"
            _secrets+=1
            do_async(function()
              wait_async(30)
              _msg=nil
            end)
          end
          -- not fall damage or sector damage for floating actors
          if not (actor.floating or actor.nosectordmg) and actor.is_shootable then
            -- fall damage
            -- see: https://zdoom.org/wiki/Falling_damage
            local dmg=(((dz*dz)>>7)*11-30)\2
            if(dmg>0) self:hit(dmg) 
          
            -- sector damage (if any)
            self:hit_sector(_sector_dmg[sector.special])
          end

          dz,h=0,sector.floor
        end
        -- for floating actors, avoid going through roof!
        if h+height>sector.ceil then
          dz,h=0,sector.ceil-height
        end
        self[3]=h
      end

      -- reset forces
      forces={0,0}
    end
  },thing)
end

function with_health(thing)
  local dmg_ttl,dead=0
  local function die(self,dmg)
    self.dead,self.target=true    
    -- lock state
    dead=true
    -- any special?
    if(self.trigger) self:trigger()
    -- death state
    self:jump_to(5)
  end
  return inherit({
    hit=function(self,dmg,dir,instigator)
      -- avoid reentrancy
      if(dead) return
      
      -- avoid same species fight
      if(instigator and instigator.actor.id==self.actor.id) return

      -- avoid automatic infight
      -- + override when player
      if(self==_plyr or instigator==_plyr or rnd()>0.8) self.target=instigator

      -- damage reduction?
      local hp,armor=dmg,self.armor or 0
      if armor>0 then
        hp=0.3*dmg
        self.armor=max(armor-dmg)\1
      end
      self.health=max(self.health-hp)\1
      if self.health==0 then
        -- register kill
        if(self.actor.countkill) _kills+=1

        die(self,dmg)
      end
      -- kickback
      if dir then
        self:apply_forces(dir[1],dir[2],hp)
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
  },thing)
end

function attach_plyr(thing,actor)
  local bobx,boby,speed,da,wp,wp_slot,wp_yoffset,wp_y,hit_ttl,dmg_factor,wp_switching=0,0,actor.speed,0,thing.weapons,thing.active_slot,0,0,0,pack(0.5,1,1,2)[_skill]

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

  return inherit({
    -- tick for player
    tick=function(self)
      hit_ttl=max(hit_ttl-1)
      if not self.dead then
        wp_y=lerp(wp_y,wp_yoffset,0.3)

        local dx,dz,daf=0,0,0.8

        if _wp_hud then
          -- slow mode: pico calls _update 2 times
          -- but button state is updated only on first frame
          -- skip button state check in this case (e.g. keep hud open)
          if(_slow==0) _wp_hud=not btn(6)
          for i,k in pairs{0,3,1,2,🅾️} do
            if btnp(k) or btnp(k,1) then
              -- only switch if we have the weapon and it's not the current weapon
              _wp_hud,_btns=(wp_slot!=i and wp[i]) and wp_switch(i),{}
            end
          end
        else
          -- direct mouse input?
          if stat(38)!=0 then
            da+=stat(38)/8
            daf=0.2
          else
            -- cursor: fwd+rotate
            -- cursor+x: weapon switch+rotate
            -- wasd: fwd+strafe
            -- o: fire
            if btn(🅾️) then
              if(_btns[0]) dx=1
              if(_btns[1]) dx=-1
            else
              if(_btns[0]) da-=0.75
              if(_btns[1]) da+=0.75
            end
          end

          if(_btns[_btnup]) dz=1
          if(_btns[_btndown]) dz=-1

          _wp_hud=btn(6)
          poke(0x5f30,1)

          -- wasd
          if(_btns[0x10]) dx=1
          if(_btns[0x11]) dx=-1
          if(_btns[0x12]) dz=1
          if(_btns[0x13]) dz=-1
        end

        self.angle-=da/256
        local ca,sa=cos(self.angle),-sin(self.angle)
        self:apply_forces(dz*ca-dx*sa,dz*sa+dx*ca,speed)

        -- damping
        -- todo: move to physic code?
        da*=daf

        -- update weapon vm
        wp[wp_slot].owner=self
        wp[wp_slot]:tick()
        
        -- weapon bobing
        bobx,boby=lerp(bobx,2*da,0.3),lerp(boby,cos(time()*3)*abs(dz)*speed*2,0.2)
      end
      return true
    end,
    attach_weapon=function(self,weapon,switch)
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
      if(switch) wp_switch(slot)
    end,
    hud=function(self)
      local active_wp=wp[wp_slot]
      local frame,light=active_wp.state,self.sector.lightlevel
      
      -- sector light affects weapon sprite (unless bright)
      light=frame[3] and 8 or min((light*15)\1,15)
      memcpy(0x5f00,0x4300|light<<4,16)          

      -- draw current weapon (single) frame      
      vspr(frame[5],64-bobx,132-wp_y+boby,16)

      pal()
      local ammotype=active_wp.actor.ammotype
      if(ammotype) printb(ammotype.icon..self.inventory[ammotype],106,120,9)
      printb("♥"..self.health,2,110,12)
      printb("웃"..self.armor,2,120,3)

      -- keys
      for item,amount in pairs(self.inventory) do
        if amount>0 and item.slot then
          printb(item.icon,98+item.slot*8,111,item.hudcolor)
        end
      end

      -- display weapon selection hud
      if _wp_hud then
        -- structure:
        -- slot number (or -1)
        -- function
        -- args (packed as a comma-separated string)
        for i=1,#_wp_wheel,3 do
          local slot=_wp_wheel[i]
          if(slot==-1 or wp[slot]) _ENV[_wp_wheel[i+1]](unpack(split(_wp_wheel[i+2])))
        end
      end

      -- set "pain" palette (defaults to screen palette if normal)
      memcpy(0x5f10,0x4400|hit_ttl<<4,16)
    end,
    hit=function(self,dmg,...)
      -- call parent function
      -- + skill adjustment
      local hp=thing.hit(self,dmg_factor*dmg,...)
      -- hp can be null if actor is dead
      if hp and hp>0 then
        hit_ttl=min(ceil(hp),15)
      end
    end,
    -- restore state
    load=function(self)
      if dget(0)>0 then
        self.health,self.armor=dget(1),dget(2)
        for i=1,5 do
          local actor=_actors[dget(i+2)]
          if actor then
            -- create thing
            self:attach_weapon(actor:attach{})
            -- don't restore counters for ammoless weapons (ex: fist)
            if(actor.ammotype) self.inventory[actor.ammotype]=dget(i+7)
          end
        end
      end
    end,
    -- save state
    save=function(self)
      dset(0,1)
      dset(1,self.health)
      dset(2,self.armor)
      -- iterate over weapon slots
      for i=1,5 do
        local w=wp[i]
        dset(i+2,w and w.actor.id or -1)
        if w then
          local ammotype=w.actor.ammotype        
          if(ammotype) dset(i+7,ammotype and self.inventory[ammotype] or -1)
        end
      end
    end
  },thing)
end
  
function draw_bsp()
  cls()
  --
  -- draw bsp & visible things
  local id,v_cache=_plyr.ssector.id,{}

  -- visit bsp
  visit_bsp(_bsp,_plyr,function(node,side,pos,visitor)
    if node.leaf[side] then
      local subs=node[side]
      if subs:in_pvs(id) then
        draw_flats(v_cache,subs)
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
  -- ensure update/draw pair is consistent
  _update_state=function()
    -- init function (if any)
    if(i) i()
    -- 
    _update_state,_draw=u,d
    -- actually run the update
    u()
  end
end

--_max_cpu,_max_cpu_ttl=0,0

function play_state()
  _loading=true

  -- stop music (eg. restart game)
  music(-1)
  _actors,_sprite_cache=decompress(mod_name,0,0,unpack_actors)

  -- ammo scaling factor
  _ammo_factor=split"2,1,1,1"[_skill]
  local bsp,thingdefs=decompress(mod_name,_maps_cart[_map_id],_maps_offset[_map_id],unpack_map)
  -- geometry + misc game counters  
  _bsp,_kills,_monsters,_found,_secrets=bsp,0,0,{},0

  -- restore main data cart
  reload()

  -- attach behaviors to things
  for _,thingdef in pairs(thingdefs) do 
    local thing,actor=make_thing(unpack(thingdef))
    -- get direct access to player
    if actor.id==1 then
      _plyr=attach_plyr(thing,actor)
      _plyr:load()
      thing=_plyr
    end    
    if(actor.countkill) _monsters+=1
    add_thing(thing)
  end

  _cam={
    -- must be called before any rendering
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
    is_visible=function(self,bbox)    
      local outcode,m1,m3,m4,_,m9,m11,m12=0xffff,unpack(self.m)
      for i=1,8,2 do
        local code,x,z=2,bbox[i],bbox[i+1]
        -- x2: fov
        local ax,az=(m1*x+m3*z+m4)<<1,m9*x+m11*z+m12
        -- todo: optimize?
        if(az>8) code=0
        if(az>854) code|=1
        if(ax>az) code|=4
        if(-ax>az) code|=8
        outcode&=code
      end
      return outcode==0
    end
  }

  -- start level music (if any)
  music(_maps_music[_map_id],0,14)
  _loading=nil
  
  return 
    -- update
    function()
      -- must be done after load to not register unpacking time!
      if(not _start_time) _start_time=time()

      if _plyr.dead then
        next_state(gameover_state,_plyr,_plyr.angle,_plyr.target,45)
      end
      _cam:track(_plyr,_plyr.angle,_plyr[3]+45)
    end,
    -- draw
    function()
      _slow=0
      draw_bsp()
      _plyr:hud()

      if(_msg) printb(_msg,64-#_msg*2,50,15)

      -- spr(0,0,64,16,8)
      -- debug messages
      --[[
      local cpu=stat(1)
      _max_cpu_ttl-=1
      if(_max_cpu_ttl<0) _max_cpu=0
      if(cpu>_max_cpu) _max_cpu=cpu _max_cpu_ttl=30

      cpu=cpu.."|".._max_cpu
    
      print(cpu,2,3,3)
      print(cpu,2,2,15)    
      cpu=(time()-_start_time).."|".._kills.."/".._monsters
    
      print(cpu,2,3,3)
      print(cpu,2,2,15)    
      ]]     
      -- printb(stat(0),2,2,15)
    end
end

function gameover_state(pos,angle,target,h)
  local idle_ttl,target_angle=30,angle
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
      
      idle_ttl-=1
      -- avoid immediate button hit
      if idle_ttl<0 then
        if btnp(_btnuse) then
          -- 1: gameover    
          load(title_cart,nil,_skill..",".._map_id..",1")
        elseif btnp(_btnfire) then
          -- 3: retry
          load(title_cart,nil,_skill..",".._map_id..",3")
        end
      end
    end,
    -- draw
    function()
      draw_bsp()

      if(time()%4<2) printb("      you died\n\nFIRE\23RESTART USE\23MENU",22,108,12)

      -- set screen palette
      -- pal({140,1,139,3,4,132,133,7,6,134,5,8,2,9,10},1)
      memcpy(0x5f10,0x4400,16)
    end
end


-->8
-- game loop
function _init()
  -- force gc
  stat(0)
  -- mouse 
  -- enable lock+button alias
  poke(0x5f2d,7)

  cartdata(mod_name)

  -- exit menu entry
  menuitem(1,"main menu",function()
    load(title_cart)
  end)
  
  -- launch params
  local p=split(stat(6))
  _skill,_map_id=tonum(p[1]) or 2,tonum(p[2]) or 1
  -- sky texture + load keyboard control mapping
  _sky_height,_sky_offset,_btnfire,_btnuse,_btndown,_btnup=_maps_sky[_map_id*2-1],_maps_sky[_map_id*2],dget(35),dget(36),dget(37),dget(38)
  -- skybox fill pattern
  fillp(0xaaaa)

  next_state(play_state)
end

function _update()
  -- get btn states and suppress pressed buttons until btnp occurs  
  for p,mask in pairs{[0]=0,0x10} do
    for i=0,5 do
      _btns[mask|i]=btnp(i,p) or _btns[mask|i] and btn(i,p)
    end
  end

  -- any futures?
  for i=#_futures,1,-1 do
    -- get actual coroutine
    local f=_futures[i].co
    -- still active?
    if f and costatus(f)=="suspended" then
      coresume(f)
    else
      deli(_futures,i)
    end
  end
  
  -- decay flash light  
  _ambientlight*=0.8
  -- decay drag
  _drag*=0.83
  -- keep world running
  for thing in all(_things) do
    if(thing:tick() and thing.update) thing:update()
  end

  _update_state()
  -- capture video!
  if(peek(0x5f83)==1) poke(0x5f83,0) extcmd("video") 
  
  _slow+=1
end

-->8
-- data unpacking functions
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

function unpack_chr()
  return chr(mpeek())
end

-- convert numeric flags to "not nil" tests
function with_flags(item,flags,all_flags)
  for i=1,#all_flags,2 do
    if(flags&all_flags[i]!=0) item[all_flags[i+1]]=true
  end
  return item
end

-- inventory & things
function unpack_ref(a)
  return a[unpack_variant()]
end

function unpack_special(sectors)
  local special=mpeek()
  local function unpack_moving_sectors(what,trigger_delay)
    -- door speed: https://zdoom.org/wiki/Map_translator#Constants
    -- speed is signed (]-32;32[)
    local moving_sectors,moving_speed,delay,lock={},(mpeek()-128)/8,unpack_variant(),unpack_variant()
    -- sectors
    -- backup heights
    unpack_array(function()
      local sector=unpack_ref(sectors)
      -- "stable" state = always floor
      sector.init=sector.floor
      sector.target=unpack_fixed()
      add(moving_sectors,sector)
    end)
    local function move_sector_async(sector,to,speed,no_crush)
      -- play open/close sound
      sfx(63)

      local hmax=sector[to]
      while true do
        -- avoid crushing things
        if no_crush then
          while sector.things>0 do
            -- wait 1 sec if door is blocked
            wait_async(30)
          end
        end          
        local h=sector[what]+speed
        if (speed>0 and h>hmax) or (speed<0 and h<hmax) then          
          sector[what]=hmax
          break
        end
        sector[what]=h
        yield()
      end
    end
    -- init
    if special==13 then
      -- close doors
      for _,sector in pairs(moving_sectors) do
        sector.ceil=sector.floor
      end
    end

    return function()
      -- move to target
      for _,sector in pairs(moving_sectors) do
        -- kill any previous moving handler
        if(sector.action) sector.action.co=nil
        -- register an async routine
        sector.action=do_async(function()
          wait_async(trigger_delay)
          move_sector_async(sector,"target",moving_speed,special==13 and moving_speed<0)
          if delay>0 then
            wait_async(delay)
            -- 
            move_sector_async(sector,"init",-moving_speed,special==13 and moving_speed>0)
          end
        end)
      end
    end,
    -- lock id 0 is no lock
    _actors[lock]
  end

  if special==13 then
    return unpack_moving_sectors("ceil",unpack_variant())
  elseif special==64 then
    return unpack_moving_sectors("floor",0)
  elseif special==112 then
    -- sectors
    local target_sectors,lightlevel={},mpeek()/255
    unpack_array(function()
      add(target_sectors,unpack_ref(sectors))
    end)
    return function()
      for _,sector in pairs(target_sectors) do
        sector.lightlevel=lightlevel
      end
    end
  elseif special==243 then
    -- exit level
    local delay=unpack_variant()    
    return function()
      -- save player's state
      _plyr:save()
      local t=time()-_start_time
      do_async(function()
        wait_async(delay)
        -- record level completion time + send stats
        load(title_cart,nil,_skill..",".._map_id..",2,"..t..",".._kills..",".._monsters..",".._secrets)
      end)
    end
  end
end

function unpack_actors()
  -- sprite index
	local actors,frames,tiles={},{},{}
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
  -- actor properties + skill ammo factor
  local properties_factory=split("0x0.0001,health,unpack_variant,0x0.0002,armor,unpack_variant,0x0.0004,amount,unpack_variant,0x0.0008,maxamount,unpack_variant,0x0.0010,icon,unpack_chr,0x0.0020,slot,mpeek,0x0.0040,ammouse,unpack_variant,0x0.0080,speed,unpack_variant,0x0.0100,damage,unpack_variant,0x0.0200,ammotype,unpack_ref,0x0.0800,mass,unpack_variant,0x0.1000,pickupsound,unpack_variant,0x0.2000,attacksound,unpack_variant,0x0.4000,hudcolor,unpack_variant,0x0.8000,deathsound,unpack_variant,0x1,meleerange,unpack_variant,0x2,maxtargetrange,unpack_variant,0x4,ammogive,unpack_variant,0x8,trailtype,unpack_ref,0x10,drag,unpack_fixed",",",1)

  -- actors functions
  local function_factory={
    -- A_FireBullets
    function()
      local xspread,yspread,bullets,dmg,puff=unpack_fixed(),unpack_fixed(),mpeek(),mpeek(),unpack_ref(actors)
      return function(owner)
        for i=1,bullets do
          hitscan_attack(owner,owner.angle+(rnd(2*xspread)-xspread)/360,1024,dmg,puff)
        end
      end
    end,
    -- A_PlaySound
    function()
      local s=mpeek()
      return function(owner)
        -- play sound only if visible from player
        if(owner.ssector:in_pvs(_plyr.ssector.id)) sfx(s)
      end
    end,
    -- A_FireProjectile
    function()
      local projectile=unpack_ref(actors)
      return function(owner)
        -- fire at 1/2 edge of owner radius (ensure collision when close to walls)
        local angle,speed,radius=owner.angle,projectile.speed,owner.actor.radius/2
        local ca,sa=cos(angle),-sin(angle)
        -- todo: get height from properties
        local thing=with_physic(make_thing(projectile,owner[1]+radius*ca,owner[2]+radius*sa,owner[3]+32,angle))
        thing.owner=owner
        thing:apply_forces(ca,sa,speed)         
        add_thing(thing)
      end
    end,
    -- A_WeaponReady
    function(item)
      return function(owner,weapon)
        if not _wp_hud and not _loading and btn(_btnfire) then
          local inventory,ammotype,newqty=owner.inventory,item.ammotype,0
          -- handle "fist" (eg weapon without ammotype)
          if(ammotype) newqty=inventory[ammotype]-item.ammouse
          if newqty>=0 then
            if(ammotype) inventory[ammotype]=newqty
            -- play attack sound
            if(item.attacksound) sfx(item.attacksound)
            -- drag?
            _drag=item.drag or 0
            -- fire state
            weapon:jump_to(9)
          end
        end
      end
    end,    
    -- A_Explode
    function()
      local dmg,maxrange=unpack_variant(),unpack_variant()
      return function(thing)
        -- todo: optimize lookup!!!
        for _,otherthing in pairs(_things) do
          if otherthing.hit and otherthing!=thing then
            local n,d=line_of_sight(thing,otherthing,maxrange)
            if(d) otherthing:hit(dmg*(1-d/maxrange),n)
          end
        end
      end
    end,
    -- A_FaceTarget
    function()
      local speed=mpeek()/255
      return function(self)
        -- nothing to face to?
        if self.target then
          local target_angle=atan2(self.target[1]-self[1],self[2]-self.target[2])
          self.angle=lerp(shortest_angle(target_angle,self.angle),target_angle,speed)   
        end
      end
    end,
    -- A_Look
    function()
      return function(self)
        -- either we got something already or player
        local otherthing=self.target or _plyr
        -- if this is dead, try player
        if(otherthing.dead) otherthing=_plyr
        -- nothing to do?
        if(otherthing.dead) self.target=nil return
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
    function(item)
      local speed,range,maxrange,ttl=item.speed,item.meleerange or 64,item.maxtargetrange or 1024,30
      return function(self)
        -- still active target?
        local otherthing=self.target
        if otherthing and not otherthing.dead then
          -- in range/visible?
          local n,d=line_of_sight(self,otherthing,maxrange)
          if d and rnd()<0.4 then
            -- i see you!
            -- note: ttl is in 'decorate' unit (not frames)
            ttl=30
            if d<range then
              -- close range attack (if any)
              self:jump_to(3,4)
            else
              -- ranged attack
              self:jump_to(4)
            end
            return
          elseif ttl>0 then
            ttl-=1
            -- zigzag toward target
            local nx,ny,dir=n[1]*0.5,n[2]*0.5,rnd{1,-1}
            local mx,my=ny*dir+nx,nx*-dir+ny
            local target_angle=atan2(mx,-my)
            self.angle=lerp(shortest_angle(target_angle,self.angle),target_angle,0.5)
            self:apply_forces(mx,my,speed)
            return
          end
        end
        -- lost/dead?
        -- !! token saving hack !! assumes jump returns nothing
        self.target=self:jump_to(0)        
      end
    end,
    -- A_Light
    function()
      local light=mpeek()/255
      return function()
        _ambientlight=light
      end
    end,
    -- A_MeleeAttack
    function()
      local dmg,puff=mpeek(),unpack_ref(actors)
      return function(owner)
        hitscan_attack(owner,owner.angle,owner.meleerange or 64,dmg,puff)
      end
    end,
    -- A_SkullAttack
    function()
      local speed=unpack_variant()
      return function(self)
        -- nothing to face to?
        if self.target then   
          self:apply_forces(cos(self.angle),-sin(self.angle),speed)
        end
      end
    end
  }

  -- copy "coll" and attach to a property "name" on thing
  local function attach_array(coll,thing,name)
    if coll then
      thing[name]={}
      for k,v in pairs(coll) do
        thing[name][k]=v
      end  
    end
  end

  
  -- actor flags layout:
  -- solid
  -- shootable
  -- missile
  -- monster
  -- nogravity
  -- float
  -- dropoff
  -- dontfall
  local all_flags=split("0x1,is_solid,0x2,is_shootable,0x4,is_missile,0x8,is_monster,0x10,is_nogravity,0x20,floating,0x40,is_dropoff,0x80,is_dontfall,0x100,randomize,0x200,countkill,0x400,nosectordmg,0x800,noblood",",",1)
  unpack_array(function()
    local kind,id,state_labels,states,weapons,active_slot,inventory=unpack_variant(),unpack_variant(),{},{},{}

    local item=with_flags({
      id=id,
      kind=kind,
      radius=unpack_fixed(),
      height=unpack_fixed(),
      mass=100,
      -- attach actor to this thing
      attach=function(self,thing)
        -- vm state (starts at spawn)
        local i,ticks,delay=state_labels[0],-2,self.is_monster and rnd(30)\1 or 0
        if(self.randomize) delay=rnd(4)\1

        -- extend properties
        thing=inherit({
          actor=self,
          health=self.health,
          armor=self.armor,
          active_slot=active_slot,
          -- pickable things
          pickup=self.pickup,  
          -- ****************** 
          -- decorate vm engine       
          -- goto vm label
          jump_to=function(self,label,fallback)
            i,ticks=state_labels[label] or (fallback and state_labels[fallback]),-2
          end,
          -- vm update
          tick=function(self)
            while ticks!=-1 do
              -- wait
              if(ticks>0) ticks+=delay-1 delay=0 return true
              -- done, next step
              if(ticks==0) i+=1
::loop::
              local state=states[i]
              -- stop (or end of vm instructions)
              if(not state or state.jmp==-1) del_thing(self) return
              -- loop or goto
              if(state.jmp) self:jump_to(state.jmp) goto loop

              -- effective state
              self.state=state
              -- get ticks
              ticks=state[1]
              -- trigger function (if any)
              -- provide owner and self (eg. for weapons)
              if(state.fn) state.fn(self.owner or self,self)
            end
          end
        },thing)

        -- clone startup inventory
        attach_array(inventory,thing,"inventory")
        -- clone weapons (to avoid changing actor definition)
        attach_array(weapons,thing,"weapons")

        return thing
      end
    },mpeek()|mpeek()<<8,all_flags)

    local properties=unpack_fixed()
    -- decode 
    for i=1,#properties_factory,3 do
      if properties_factory[i]&properties!=0 then
        -- unpack
        item[properties_factory[i+1]]=_ENV[properties_factory[i+2]](actors)
      end
    end
    -- startitems (always packed at the end)
    if properties&0x0.0400!=0 then
      unpack_array(function()
        local startitem,amount=unpack_ref(actors),unpack_variant()
        if startitem.kind==2 then
          -- weapon
          weapons=weapons or {}
          -- create a new "dummy" thing
          local weapon_thing=startitem:attach{}
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
    end

    local function pickup(thing,owner,ref,qty,maxqty)
      ref,maxqty=ref or item,maxqty or item.maxamount
      -- only pick up if we're below max quantity
      if not owner[ref] or owner[ref]<maxqty then
        owner[ref]=min((owner[ref] or 0)+(qty or item.amount),maxqty)
        if(item.pickupsound) sfx(item.pickupsound)
        del_thing(thing)
      end
    end
    
    item.pickup=pack(
      -- default inventory item (ex: lock)
      function(thing,target)
        pickup(thing,target.inventory)
      end,
      -- ammo family
      function(thing,target)
        pickup(thing,target.inventory,item.ammotype,_ammo_factor*item.amount)
      end,
      -- weapon
      function(thing,target)
        local ammotype=item.ammotype
        pickup(thing,target.inventory,ammotype,_ammo_factor*item.ammogive,ammotype.maxamount)
        target:attach_weapon(thing,true)
      end,
      -- health pickup
      function(thing,target)
        pickup(thing,target,"health")
      end,
      -- armor pickup
      function(thing,target)
        pickup(thing,target,"armor")
      end
    )[kind+1]
    
    -- actor states
    unpack_array(function()
      -- map label id to state command line number
      state_labels[mpeek()]=mpeek()
    end)
        
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
          add(cmd,unpack_ref(frames))
          -- number of sides
          cmd[4]=i
        end)
        -- function?
        if flags&0x8>0 then
          cmd.fn=function_factory[mpeek()](item)
        end
      end
      add(states,cmd)
    end)

    -- register
    actors[id]=item
  end)
  return actors,make_sprite_cache(tiles)
end

-- linedefs
function switch_texture(line)
  -- flip midtex only
  line[true][3]=_onoff_textures[line[true][3]]
end

-- unpack level data (geometry + things)
function unpack_map()
  -- sectors
  local sectors,sub_sectors,nodes={},{},{}
  unpack_array(function()
    local special=mpeek()
    local sector=add(sectors,{
      -- sector attributes
      special=special,
      -- ceiling/floor height
      ceil=unpack_fixed(),
      floor=unpack_fixed(),
      ceiltex=unpack_fixed(),
      floortex=unpack_fixed(),
      -- rebase to 0-1
      lightlevel=mpeek()/255,
      -- number of things in sector
      things=0
    })
    -- sector behaviors (if any)
    if special==65 then
      local lights={sector.lightlevel,0.125}
      do_async(function()
        while true do
          sector.lightlevel=rnd(lights)
          wait_async(5)
        end
      end)
    elseif special==84 then
      -- east scrolling
      sector.tx=rnd(32)
      do_async(function()
        while true do 
          sector.tx+=1/32
          yield()
        end
      end)
    end
  end)

  do
    local sides,verts,lines,all_segs={},{},{},{}
    -- sidedefs
    unpack_array(function()
      add(sides,{
        -- 1: sector reference
        unpack_ref(sectors),
        -- bottomtex
        unpack_fixed(),
        -- midtex
        unpack_fixed(),
        -- toptex
        unpack_fixed()
      })
    end)

    -- vertices
    unpack_array(function()
      add(verts,{unpack_fixed(),unpack_fixed()})
    end)

    local all_flags=split("0x1,twosided,0x2,special,0x4,dontpegtop,0x8,playeruse,0x10,playercross,0x20,repeatspecial,0x40,blocking",",",1)
    unpack_array(function()
      local line=add(lines,with_flags({
        -- sides
        [true]=unpack_ref(sides),
        [false]=unpack_ref(sides)},mpeek(),all_flags))
      -- special actions
      if line.special then
        local special,actorlock=unpack_special(sectors)             
        line.trigger=function(thing)
          -- need lock?
          -- note: keep key in inventory (for reusable locked doors)
          if actorlock and not thing.inventory[actorlock] then 
            -- avoid too much messages/reentrancy
            if not line.locked then
              line.locked=do_async(function()
                _msg="locked"
                wait_async(15)
                _msg,line.locked=nil
              end)
              -- play "err" sound
              sfx(62)
            end
            return
          end

          -- backup trigger
          local trigger=line.trigger
          -- avoid reentrancy
          line.trigger=nil
          --
          switch_texture(line)
          -- do the action *outside* of a coroutine
          special()
          -- repeatable?
          if line.repeatspecial then
            do_async(function()
              -- avoid player hitting trigger/button right away
              wait_async(30)
              -- unlock (if repeatable)
              line.trigger=trigger 
              -- restore visual
              switch_texture(line)
            end)
          end
        end
      end
    end)

    -- convex sub-sectors
    unpack_array(function(i)
      local pvs={}
      local segs={
        id=i,
        in_pvs=function(self,id)
          -- avoid band (costly in 0.2.2)
          return pvs[id\32] and pvs[id\32]&0x0.0001<<(id&31)!=0
        end}
      unpack_array(function()
        local v,flags=unpack_ref(verts),mpeek()
        local s=add(segs,{
          -- 1: vertex
          v,
          side=flags&0x1==0,
          -- optional links
          line=flags&0x2>0 and unpack_ref(lines),
          partner=flags&0x4>0 and unpack_variant()
        })

        -- direct link to sector (if not already set)
        if s.line and not segs.sector then
          segs.sector=s.line[s.side][1]
        end
        --assert(s.v0,"invalid seg")
        --assert(segs.sector,"missing sector")
        add(all_segs,s)
      end)
      -- pvs (packed as a bit array)
      unpack_array(function()
        local id=unpack_variant()
        pvs[id\32]=bor(pvs[id\32],0x0.0001<<(id&31))
      end)
      -- normals
      local s0=segs[#segs]
      local v0=s0[1]
      for i,s1 in ipairs(segs) do
        local v1=s1[1]
        local n,len=v2_normal(v2_make(v0,v1))
        local nx,ny=unpack(n)
        -- 2: segment dir x
        add(s0,nx)
        -- 3: segment dir y
        add(s0,ny)
        -- 4: dist to origin
        add(s0,v2_dot(n,v0))
        -- 5: len
        add(s0,len)
        -- 6: normal x
        add(s0,-ny)
        -- 7: normal y
        add(s0,nx)
        -- 8: distance to origin
        add(s0,v2_dot({-ny,nx},v0))
        -- 9: use normal direction to select uv direction
        add(s0,abs(ny)>abs(nx) and "v" or "u")

        v0,s0=v1,s1
      end
      add(sub_sectors,segs)
    end)

    -- fix seg -> sub-sector link (e.g. portals)
    for _,seg in pairs(all_segs) do
      seg.partner=sub_sectors[seg.partner]
    end
  end

  -- bsp nodes
  unpack_array(function()
    local node,flags=add(nodes,{
      -- normal packed in struct to save memory
      unpack_fixed(),unpack_fixed(),
      -- distance to plane
      unpack_fixed(),
      leaf={}
    }),mpeek()
    local function unpack_node(side,leaf)
      local refs=sub_sectors
      if leaf then
        node.leaf[side]=true
      else
        -- bounding box only on non-leaves
        node.bbox=node.bbox or {}
        -- returns an array of 2d vectors 
        local t,b,l,r=unpack_fixed(),unpack_fixed(),unpack_fixed(),unpack_fixed()
        node.bbox[side]={l,b,l,t,r,t,r,b}
        refs=nodes
      end
      node[side]=unpack_ref(refs)
    end
    unpack_node(true,flags&0x1>0)
    unpack_node(false,flags&0x2>0)
  end)

  -- texture pairs
  unpack_array(function()
    _onoff_textures[unpack_fixed()]=unpack_fixed()
  end)

  -- transparent textures
  unpack_array(function()
    _transparent_textures[unpack_fixed()]=0x10
  end)

  -- things
  local things={}
  local function unpack_thing()
    local flags,id,x,y=mpeek(),unpack_variant(),unpack_fixed(),unpack_fixed()
    if flags&(0x10<<(_skill-1))!=0 then
      -- layout must match make_thing
      return add(things,{
        -- link to underlying actor
        _actors[id],
        -- coordinates
        x,y,
        -- dummy height, extracted from sector
        0x8000,
        -- angle
        (flags&0xf)/8
      })
    end
  end
  -- standard things
  unpack_array(unpack_thing)

  -- things with special behaviors
  unpack_array(function()
    -- make sure to unpack special even if things does not appear on skill level
    local thing,special=unpack_thing(),unpack_special(sectors)
    if thing then
      add(thing,function(self)
          -- avoid reentrancy
          self.trigger=nil
          --
          special()
        end)
    end
  end)    

  -- returns top level bsp node + things to be created
  return nodes[#nodes],things
end
