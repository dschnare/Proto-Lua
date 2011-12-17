--[[
Author: Darren Schnare
Keywords: lua,prototype,inheritence,constructor,copy,javascript,ecmascript
License: MIT ( http://www.opensource.org/licenses/mit-license.php )
Repo: https://github.com/dschnare/Proto-Lua
]]--

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
    local args = {...}; local t = args[1]
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
  -- Determines if a table is an instance of the specified constructor. The constructor must have been 
  -- created from 'constructor.create()'. It is safe to call this function with any argument types.
  instanceof = function(a, b)
    if (type(a) == 'table' and type(b) == 'table') then
      if (type(a.instanceof) == 'function') then
        return a:instanceof(b)
      end
    end

    return false
  end -- instanceof(),
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
    create = function(base, members)
      local prototype = {instanceof = nil}

      if (type(members) ~= 'table') then members = base end

      if (type(members) == 'table') then
        if (type(base) == 'table') then
          -- Setup our prototype to resolve missing properties from our base.
          if (type(base.prototype) == 'table') then
            setmetatable(prototype, {__index = base.prototype})
          else
            setmetatable(prototype, {__index = base})
          end
        else
          base = nil
        end

        -- Copy all properties from the members table into our prototype.      
        for k,v in pairs(members) do prototype[k] = v end

        -- This is our constructor we are creating. Make sure we give a property that points to our prototype.
        local constructor = {prototype = prototype}

        -- All constructors have an 'instanceof' method.
        --[[ Example:            
            local cat = Cat('felix', 'black')
            print(cat:instanceof(Cat)) // true            
        --]]
        prototype.instanceof = function(self, other)
          if (type(other) == 'table') then
            if (other == constructor) then return true end
            local b = base
            if (b and type(b.prototype) == 'table') then b = base.prototype end
            if (b and type(b.instanceof) == 'function') then return b:instanceof(other) end
          end

          return false
        end

        setmetatable(constructor, {
          __call = function(self, ...)
            local instance = setmetatable({constructor = constructor}, {__index = prototype})

            local other = ...

            -- If a single table is passed in as an argument and its constructor equals our constructor
            -- and we have a 'copy' method (i.e. copy operation) then this call to our constructor is
            -- really a 'copy constructor' call. When this occurs we do not call the 'init' method before returning.
            if (select('#', ...) == 1 
                and type(other) == 'table' 
                and other.constructor == constructor 
                and type(instance.copy) == 'function') then              
              instance:copy(other);
              return instance;
            end

            -- If we have an 'init' method then we call it with any of the arguments passed to our constructor.
            if (type(instance.init) == 'function') then
              instance:init(...)
            end

            -- Return the newly created instance.
            return instance;
          end
        })

        return constructor
      else
        error('Expected members to be a table.')
      end
    end -- create()
  } -- constructor {}
} -- proto namespace {}