local proto = require('proto')

print(createMetatable)

-- Create a constructor for Animal objects.
-- Give all animals an initialization method that accepts a name.
local Animal = proto.constructor.create({
  __init = function(self, name)
    self.name = name
  end,
  -- Copy method that facilitates our copy constructor.
  -- Up to us to initialize the instance properly.
  __copy = function(self, other)
    self:__init(other:getName())
  end
})

-- We can also add properties to our prototype directly.
-- Here we add a method to return our name.
function Animal.prototype:getName()
  return self.name
end

-- Create a constructor that extends/inherits from our Animal constructor's prototype.
-- Here we create a closure to capture our reference to our base prototype.
local Dog = (function(base)
  -- Create and return the constructor, but override the initialization method
  -- with our own by concatenating ' the dog' to the name.
  return proto.constructor.create(base, {
    __init = function(self, name)
      base.__init(self, name .. ' the dog')
    end
  })

-- Call the closure immediately.
end)(Animal.prototype)


-- Create another constructor, but override the initialization method
-- with our own by concatenating ' the cat' to the name.
-- This time we create a constructor by using the Animal constructor directly
-- as our base (its prototype will be used), and we call the base init() method
-- by calling it on Animal's prototype and passing ourself as 'self'. This approach
-- is a bit smaller to write, but is not as optimal as the approach above for Dog
-- since we are accessing the prototype and constructor each time we want to call base version of a method.
local Cat = proto.constructor.create(Animal, {
  __init = function(self, name)
    Animal.prototype.__init(self, name .. ' the cat')
  end
})

-- Create some animals.
-- Create a temporary animal.
local temp = Animal('some animal')
-- Create a new animal that is a copy of our temporary animal.
-- This uses our copy constructor for Animals.
-- Not that in order to use the copy constructor mechanism the table passed in
-- must have been created from the constructor being called, Animal in this case.
local animal = Animal(temp)
local misha = Dog('misha')
local felix = Cat('felix')

print('temp == animal: ', temp == animal)
print('animal\'s name is: ', animal:getName())
print('misha\'s name is: ', misha:getName())
print('felix\'s name is: ', felix:getName())
print('---')
print('is animal an Animal: ', animal:instanceof(Animal))
print('is animal a Dog: ', animal:instanceof(Dog))
print('is animal a Cat: ', animal:instanceof(Cat))
print('is animal a {}: ', animal:instanceof({}))
print('---')
print('is misha an Animal: ', proto.instanceof(misha, Animal))
print('is misha a Dog: ', proto.instanceof(misha, Dog))
print('is misha a Cat: ', proto.instanceof(misha, Cat))
print('---')
print('is felix an Animal: ', proto.instanceof(felix, Animal))
print('is felix a Dog: ', proto.instanceof(felix, Dog))
print('is felix a Cat: ', proto.instanceof(felix, Cat))
print('---')
print('misha adheresTo animal: ', proto.adheresTo(misha, animal))
print('misha adheresTo interface: ', proto.adheresTo(misha, {name='string', getName='function', constructor='*'}))