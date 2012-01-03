--[[
Author: Darren Schnare
Keywords: lua,prototype,inheritence,constructor,copy,javascript,ecmascript
License: MIT ( http://www.opensource.org/licenses/mit-license.php )
Repo: https://github.com/dschnare/Proto-Lua
]]--

-- Determines if a table instance has a constructor's protoype in its prototype chain.
-- The constructor is expected to be created from 'constructor.create()'. Safe to call
-- with any arguments.
--[[ Example:
		local cat = Cat('felix', 'black')
		print(cat:instanceof(Cat)) -- true
--]]
local instanceof = function(a, constructor)
	local _type, _rawequal = type, rawequal

	if (_type(a) ~= 'table') then return false end
	if (_type(constructor) ~= 'table' or _type(constructor.prototype) ~= 'table') then return false end

	local o = constructor.prototype

	while true do
		a = a.__proto
		if (_type(a) ~= 'table') then return false end
		if (_rawequal(o, a)) then return true end
	end
end -- instanceof()

return {
  -- Determines if a value is a string.
  isString = function(v) return type(v) == 'string' end,
  -- Determines if a value is a table.
  isTable = function(v) return type(v) == 'table' end,
  -- Determines if a value is a function.
  isFunction = function(v) return type(v) == 'function' end,
  -- Determines if a value is defined (not nil).
  isDefined = function(v) return type(v) ~= 'nil' end,
  -- Determines if a value is nil.
  isNil = function(v) return type(v) == 'nil' end,
  -- Determines if a value is a number.
  isNumber = function(v) return type(v) == 'number' end,
  -- Determines if a value is a boolean.
  isBoolean = function(v) return type(v) == 'boolean' end,
  -- Determines if a value is a thread.
  isThread = function(v) return type(v) == 'thread' end,
  -- Determines if a value is a userdata.
  isUserdata = function(v) return type(v) == 'userdata' end,
  --[[
    Determines if a table adheres to another table's interface.

    This function will iterate over all table key:value pairs in 'b' and compare the built-in type
    of a coorisponding key in 'a' if it exists. If the built-in types don't match then return false
    otherwise returns true.

    In place of 'b' a table with key:value pairs whose values are strings equivalent to the expected built-in type
    can be used. The '*' string is a special type that will expect the key to at least exist on 'a', but can be of
    any built-in type.
  ]]--
  adheresTo = function(a, b)
		local _type = type

    if (_type(a) == 'table' and _type(b) == 'table') then
      for k,v in pairs(b) do
        if (v == '*' and _type(a[k]) ~= 'nil') then return true end

        if (_type(v) == _type(a[k]) or _type(v) == a[k]) then return true end
      end

      return false
    else
      return _type(a) == _type(b)
    end
  end, -- adheresTo()
  --[[
    Copies all key:value pairs of each table passed to the function into the first table of the argument list.

    If the first argument is not a valid table then one will be created. Existing key:value pairs on the table
    will be overwritten.

    Returns the first table or the newly created table.

    mixin()
    mixin(t)
    mixin(t, t1, t2, ..., tn)
  ]]--
  mixin = function(...)
    local args, _type, a = {...}, type, nil
		local t = args[1]

    if (_type(t) ~= 'table') then t = {} end

    for i=1,#args do
      if (i ~= 1) then
				a = args[i]

        if (_type(a) == 'table') then
          for k,v in pairs(a) do t[k] = v end
        end
      end
    end

    return t
  end, -- mixin()
	-- Determines if a table instance has a constructor's protoype in its prototype chain.
	-- The constructor is expected to be created from 'constructor.create()'. Safe to call
	-- with any arguments.
	instanceof = instanceof,
	-- Promotes all members of an instance's prototype chain to be instance members. Since all members will be at the
	-- instance level their access is much faster than if they were left deep in the prototype chain.
	-- A drawback of this is that the instance is now overriding its entire prototype chain which
	-- means any changes to the chain will not be reflected in the instance. However, new members added
	-- to the prototype chain will be available immediately as usual. Setting an instance member to 'nil'
	-- will restore typical prototypal behaviour.
	promote = function(t)
		local _type, _rawget = type, rawget

		if (_type(t) == 'table') then
			local p = t.__proto

			while (_type(p) == 'table') do
				for k,v in pairs(p) do
					if (_rawget(t, k) == nil) then
						t[k] = v
					end
				end

				p = p.__proto
			end

			return t
		end
	end, -- promote()
  --[[
    The constructor namespace.
  ]]--
  constructor = {
    --[[
      Creates a new constructor with the specified members and base prototype. If 'base' is a constructor then the
      constructor's prototype will be used. All key:value pairs in the 'members' table will be added to the
      constructor's prototype, so the keys on 'members' are prototype properties.

      All constructors have the following properties:
      - prototype   The constructor's prototype table. Any key:value pairs on this table will be available on all
                    instances. Any new key:value pairs added after instances have been created will be available
                    immediately on all instances.

      When a constructor is called as a function it will return a new table instance that inherits all properties
      from the constructor's prototype and its base prototype and so on. If an 'init' method exists on the instance
      either from its members or protytpe chain then it will be called with the arguments passed to the constructor.

      If a constructor is called with a single table argument that was created by the constructor being called and
      the instance being created has a 'copy' method, then the 'copy' method will be called with the other table
      instance to be copied from. This pattern is the copy-constructor pattern similar to C++.

      All instances created from a constructor have at least the following properties:
      - constructor   A reference back to the constructor that created the instance.
      - instanceof    A method that tests the prototype chain of the instance. When called with a constructor will
                      return true if the instance has the constructor's prototype in its prototype chain,
                      false otherwise.
			- __proto				A reference to the instance's prototype. This can be modified at runtime.

      create()
      create(members)
      create(base, members)
    ]]--
    create = (function()
			-- Locals for the 'create' function.

			-- The metatable used for ALL instances created with constructors created from this API.
			-- This saves substantially on memory usage compared to creating a new metatable for each constructor.
			local metatable = {
				__add = function(self, other)
					local __add = self.__add

					if (type(__add) == 'function') then
						return __add(self, other)
					else
						error('Table does not have an "add" metamethod.')
					end
				end,

				__sub = function(self, other)
					local __sub = self.__sub

					if (type(__sub) == 'function') then
						return __sub(self, other)
					else
						error('Table does not have a "sub" metamethod.')
					end
				end,

				__mul = function(self, other)
					local __mul = self.__mul

					if (type(__mul) == 'function') then
						return __mul(self, other)
					else
						error('Table does not have a "mul" metamethod.')
					end
				end,

				__div = function(self, other)
					local __div = self.__div

					if (type(__div) == 'function') then
						return __div(self, other)
					else
						error('Table does not have a "div" metamethod.')
					end
				end,

				__mod = function(self, other)
					local __mod = self.__mod

					if (type(__mod) == 'function') then
						return __mod(self, other)
					else
						error('Table does not have a "mod" metamethod.')
					end
				end,

				__pow = function(self, other)
					local __pow = self.__pow

					if (type(__pow) == 'function') then
						return __pow(self, other)
					else
						error('Table does not have a "pow" metamethod.')
					end
				end,

				__unm = function(self)
					local __unm = self.__unm

					if (type(__unm) == 'function') then
						return __unm(self)
					else
						error('Table does not have an "unm" metamethod.')
					end
				end,

				__concat = function(self, other)
					local __concat = self.__concat

					if (type(__concat) == 'function') then
						return __concat(self, other)
					else
						error('Table does not have a "concat" metamethod.')
					end
				end,

				__eq = function(self, other)
					local __eq = self.__eq

					if (type(__eq) == 'function') then
						return __eq(self, other)
					else
						if type(self) ~= type(other) then  -- different types?
							return false   -- different objects
						end

						return rawequal(self, other)
					end
				end,

				__lt = function(self, other)
					local __lt = self.__lt

					if (type(__lt) == 'function') then
						return __lt(self, other)
					else
						error('Table does not have a "lt" metamethod.')
					end
				end,

				__le = function(self, other)
					local __le = self.__le

					if (type(__le) == 'function') then
						return __le(self, other)
					else
						error('Table does not have a "le" metamethod.')
					end
				end,

				__index = function(self, key)
					local value, __proto = nil, self.__proto

					if (type(__proto) == 'table') then
						value = __proto[key]
					end

					if (value ~= nil) then return value end

					local __index = rawget(self, '__index')
					if (__index ~= true and type(__proto) == 'table') then
						__index = __proto.__index
					end

					if (__index) then
						if (type(__index) == 'function') then
							return __index(self, key)
						elseif (type(__index) == 'table') then
							return __index[key]
						end
					end

					return value
				end,

				__newindex = function(self, key, value)
					local __proto = self.__proto

					local __newindex = self.__newindex

					if (type(__newindex) == 'function') then
						__newindex(self, key, value)
					else
						rawset(self, key, value)
					end
				end,

				__call = function(self, ...)
					local __call = self.__call

					if (type(__call) == 'function') then
						return __call(self, ...)
					else
						error('Table does not have a "call" metamethod.')
					end
				end,

				__tostring = nil
			}

			function metatable:__tostring()
				local __tostring = self.__tostring

				if (type(__tostring) == 'function') then
					return __tostring(self)
				else
					__tostring = metatable.__tostring
					metatable.__tostring = nil
					local str = tostring(self)
					metatable.__tostring = __tostring
					return str
				end
			end

			-- Creates a table with a readonly '__proto' key referencing base.
			local createPrototype = function(base)
				if (type(base.prototype) == 'table') then base = base.prototype end

				return setmetatable({instanceof = nil}, {
					__index = function(self, key)
						if (key == '__proto') then return base end
						return base[key]
					end,

					__newindex = function(self, key, value)
						if (key ~= '__proto') then
							rawset(self, key, value)
						end
					end
				})
			end -- createPrototype()

			-- The 'create' function.
			return function(base, members)
				local prototype = nil

				if (type(members) ~= 'table') then
					members = base
					base = nil
				end

				if (type(base) ~= 'table') then base = {} end

				-- Create a prototype to resolve missing properties from our base
				-- and have a readonly '__proto' key referencing the base prototype.
				-- This reference is for walking the prototype chain.
				prototype = createPrototype(base)

				-- Copy all properties from the members table into our prototype.
				if (type(members) == 'table') then
					for k,v in pairs(members) do prototype[k] = v end
				end

				-- Nil out the members and base table
				members = nil
				base = nil

				-- All prototypes have an 'instanceof' method for convenience.
				prototype.instanceof = instanceof

				-- This is our constructor we are creating. Make sure we give a property that points to our prototype.
				local constructor = {prototype = prototype}

				-- Set the metatable for our constructor, making it callable.
				setmetatable(constructor, {
					__call = function(self, ...)
						local instance = setmetatable({constructor = constructor, __proto = prototype}, metatable)

						local other = ...

						-- If a single table is passed in as an argument and its constructor equals our constructor
						-- and we have a '__copy' method (i.e. copy operation) then this call to our constructor is
						-- really a 'copy constructor' call. When this occurs we do not call the '__init' method before returning.
						if (select('#', ...) == 1
								and type(other) == 'table'
								and other.constructor == constructor
								and other.__proto = constructor.prototype
								and type(instance.__copy) == 'function') then

							instance:__copy(other)
							return instance
						end

						-- If we have an '__init' method then we call it with all of the arguments passed to our constructor.
						if (type(instance.__init) == 'function') then
							instance:__init(...)
						end

						-- Return the newly created instance.
						return instance
					end
				})

				return constructor
			end -- create()
    end)() -- create() closure
  } -- constructor {}
} -- proto namespace {}