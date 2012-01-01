>   Author: Darren Schnare

>   Keywords: lua,prototype,inheritence,constructor,copy,javascript,ecmascript

>   License: MIT ( http://www.opensource.org/licenses/mit-license.php )

>   Repo: https://github.com/dschnare/Proto-Lua


Proto Lua
====================

Proto Lua is an API that performs metatable management to facilitate prototypal design patterns very similar to that found in JavaScript.
This module can ease the transition from JavaScript to Lua for JavaScript developers. There are also several useful functions
that help with mixins, testing for interface adherence, type testing and creating constructors.


Installation
--------------------

The `src/proto.lua` file is the source file with inline commenting explaining its usage.

The `src/example.lua` file is the entry point for a simple test demonstrating proto.lua`.


Running the example
--------------------

To run the example change to the `src/` directory and run Lua with `example.lua`.

    pushd src
    lua5.1 example.lua
    popd