-- plain read from cart (e.g. not compressed)
function decompress(cart,cart_id,mem,fn,...)
	-- jump to cart
	reload(0,0,0x4300,cart.."_"..cart_id..".p8")

	-- register global mpeek function
	mpeek=function()
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
	-- deserialize in context	
	return fn(...)
end