-- lzs unpacking function
-- credits: https://www.excamera.com/sphinx/article-compression.html
function decompress(cart,cart_id,mem,fn,...)
	-- jump to cart
	reload(0,0,0x4300,cart.."_"..cart_id..".p8")

	local function read()
		-- switch cart as needed
		if mem>0x42ff then
			cart_id+=1
			mem=0
			reload(0,0,0x4300,cart.."_"..cart_id..".p8")
		end	
		local b=@mem
		mem+=1
		return b
	end

	local dst,history,src,mask={},{},read(),1
	local function get1()		
		local r=(src&mask)!=0 and 1 or 0
    mask<<=1
    if(mask>0x80) src=read() mask=1
    return r
	end
	local function getn(n)
		local r=0
    for i=1,min(16,n) do
			r=get1()|r<<1
		end
		if n>16 then
			r|=getn(n-16)>>>16
		end
    return r
	end
	
	local o,l,m,e=getn(4),getn(4),getn(2),getn(32)
	local max_offset=1<<o
	local function push(b)
		add(dst,add(history,b))	
		-- keep buffer size under max offset limit	
		if(#history>max_offset) deli(history,1)
	end

	-- register global mpeek function
	mpeek=function()		
		if #dst==0 then
			-- fetch more data
			if get1()==0 then
				push(getn(8))
			else
				local offset=-getn(o)
				for k=1,getn(l)+m do
					push(history[#history+offset])
				end
			end	
		end
		-- pop first byte		
		return deli(dst,1)
	end
	-- deserialize in context	
	return fn(...)
end