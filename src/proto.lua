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
	if (type(a) ~= 'table') then return false end
	if (type(constructor) ~= 'table' or type(constructor.prototype) ~= 'table') then return false end

	local o = constructor.prototype

	while true do
		a = a.__proto
		if (type(a) ~= 'table') then return false end
		if (rawequal(o, a)) then return true end
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
    if (type(a) == 'table' and type(b) == 'table') then
      for k,v in pairs(b) do
        if (v == '*' and type(a[k]) ~= 'nil') then return true end

        if (type(v) == type(a[k]) or type(v) == a[k]) then return true end
      end

      return false
    else
      return type(a) == type(b)
    end
  end, -- adheresTo()
  --[[
    Copies all key:value paris of each table passed to the function into the first table of the argument list.

    If the first argument is not a valid table then one will be created. Existing key:value pairs on the table
    will be overwritten.

    Returns the first table or the newly created table.

    mixin()
    mixin(t)
    mixin(t, t1, t2, ..., tn)
  ]]--
  mixin = function(...)
    local args = {...} local t = args[1]
    if (type(t) ~= 'table') then t = {} end

    for i,a in ipairs(args) do
      if (i ~= 1) then
        if (type(a) == 'table') then
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

      create()
      create(members)
      create(base, members)
    ]]--
    create = (function()
			-- Locals for the 'create' function.

			-- Creates a metatable that will look for metamethods in the table instance (i.e. self).
			-- If no metamethod is found then throws an error.
			local createMetatable = function(prototype)
				-- Instantiate the metatable with pre-allocated keys.
				local m = {
					__add = nil,
					__sub = nil,
					__mul = nil,
					__div = nil,
					__mod = nil,
					__pow = nil,
					__unm = nil,
					__concat = nil,
					__len = nil,
					__eq = nil,
					__lt = nil,
					__le = nil,
					__index = nil,
					__newindex = nil,
					__call = nil
				}

				function m:__add(other)
					if (type(self.__add) == 'function') then
						return self:__add(other)
					else
						error('Table does not have an "add" metamethod.')
					end
				end -- __add()

				function m:__sub(other)
					if (type(self.__sub) == 'function') then
						return self:__sub(other)
					else
						error('Table does not have a "sub" metamethod.')
					end
				end -- __sub()

				function m:__mul(other)
					if (type(self.__mul) == 'function') then
						return self:__mul(other)
					else
						error('Table does not have a "mul" metamethod.')
					end
				end -- __mul()

				function m:__div(other)
					if (type(self.__div) == 'function') then
						return self:__div(other)
					else
						error('Table does not have a "div" metamethod.')
					end
				end -- __div()

				function m:__mod(other)
					if (type(self.__mod) == 'function') then
						return self:__mod(other)
					else
						error('Table does not have a "mod" metamethod.')
					end
				end -- __mod()

				function m:__pow(other)
					if (type(self.__pow) == 'function') then
						return self:__pow(other)
					else
						error('Table does not have a "pow" metamethod.')
					end
				end -- __pow()

				function m:__unm()
					if (type(self.__unm) == 'function') then
						return self:__unm()
					else
						error('Table does not have an "unm" metamethod.')
					end
				end -- __unm()

				function m:__concat(other)
					if (type(self.__concat) == 'function') then
						return self:__concat(other)
					else
						error('Table does not have a "concat" metamethod.')
					end
				end -- __concat()

				function m:__len()
					if (type(self.__len) == 'function') then
						return self:__len()
					else
						error('Table does not have a "len" metamethod.')
					end
				end -- __len()

				function m:__eq(other)
					if (type(self.__eq) == 'function') then
						return self:__eq(other)
					else
						if type(self) ~= type(other) then  -- different types?
							return false   -- different objects
						end
						return rawequal(self, other)
					end
				end -- __eq()

				function m:__lt(other)
					if (type(self.__lt) == 'function') then
						return self:__lt(other)
					else
						error('Table does not have a "lt" metamethod.')
					end
				end -- __lt()

				function m:__le(other)
					if (type(self.__le) == 'function') then
						return self:__le(other)
					else
						error('Table does not have a "le" metamethod.')
					end
				end -- __le()

				-- Set the 'index' event to a metamethod that will first check
				-- if the key exists in the prototype chain then fall back to
				-- an '__index' key in the prototype chain. The '__index' key can be
				-- a function or a table (see the Lua 5.1 reference for details how this works).
				function m:__index(key)
					if (key == '__proto') then return prototype end

					local value = prototype[key]

					if (value ~= nil) then return value end

					local __index = rawget(self, '__index')

					if (__index) then
						if (type(__index) == 'function') then
							return __index(self, key)
						elseif (type(__index) == 'table') then
							return __index[key]
						end

						return nil
					end

					return value
				end -- __index()

				function m:__newindex(key, value)
					if (key == '__proto' and type(value) == 'table' and self ~= value) then
						prototype = value
					elseif (type(self.__newindex) == 'function') then
						self:__newindex(key, value)
					else
						rawset(self, key, value)
					end
				end -- __newindex()

				function m:__call(...)
					if (type(self.__call) == 'function') then
						return self:__call(...)
					else
						error('Table does not have a "call" metamethod.')
					end
				end -- __call()

				function m:__tostring()
					if (type(self.__tostring) == 'function') then
						return self:__tostring()
					else
						local old_tostring = getmetatable(self).__tostring
						getmetatable(self).__tostring = nil
						local str = tostring(self)
						getmetatable(self).__tostring = old_tostring
						return str
					end
				end -- __tostring

				-- __gc

				return m
			end -- createMetatable()

			-- Creates a table with a readonly '__proto' key referencing base.
			local createPrototype = function(base)
				if (type(base.prototype) == 'table') then base = base.prototype end

				return setmetatable({}, {
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

				-- All prototypes have an 'instanceof' method for convenience.
				function prototype:instanceof(other)
					return instanceof(self, other)
				end

				-- Create a metatable for all instances.
				local metatable = createMetatable(prototype)

				-- This is our constructor we are creating. Make sure we give a property that points to our prototype.
				local constructor = {prototype = prototype}

				-- Set the metatable for our constructor, making it callable.
				setmetatable(constructor, {
					__call = function(self, ...)
						local instance = setmetatable({constructor = constructor}, metatable)

						local other = ...

						-- If a single table is passed in as an argument and its constructor equals our constructor
						-- and we have a 'copy' method (i.e. copy operation) then this call to our constructor is
						-- really a 'copy constructor' call. When this occurs we do not call the 'init' method before returning.
						if (select('#', ...) == 1
								and type(other) == 'table'
								and other.constructor == constructor
								and type(instance.__copy) == 'function') then

							instance:__copy(other)
							return instance
						end

						-- If we have an 'init' method then we call it with all of the arguments passed to our constructor.
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