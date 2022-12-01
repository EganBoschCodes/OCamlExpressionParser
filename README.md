# OCaml Expression Parser
A string-to-expression parser to enable the parsing of a string into a rapidly evaluated datatype in order to prevent the need for re-parsing when variables change. The inspiration for this originally came from just being impressed with online calculators, but actual need for it came when I began writing Bosch's Mod, a mod for Minecraft written in Java to allow myself to graph mathematical expressions in Minecraft. In order to actually graph equations of my design, I wanted the freedom to actually input the expression into the command. However, that would require me to be able to parse the string expression in the command into a number from which I could make decisions on whether or not to place blocks, and what blocks to place.

My original design evaluated a string and a map of string variable names to their float values directly to a float. This however, left something to be desired. When I needed to evaluate the expression multiple times with different values in the string variables (E.G. if I were evaluation an expression for all (x, y, z) points in a specified region in order to decide whether or not to place a block at said coordinates), I would have to go through all the work of re-parsing the string expression every time I needed to change the value of the variables, a waste of computational power. Therefore I designed a halfway point, a way to represent an Expression as a series of Objects that were in one of four categories;

1. Constant: Just a number, like 1 or 2.5.
2. Symbol: A variable name, like x, y, or val.
3. Binary Operator: An expression containing a binary operator (like "+") and two sides of the operator, the left and the right side.
4. Function Calls: An expression containing the name of a function and a list of other expressions, representing the arguments of the function.

In doing so, I could parse the string into the Expression object once, and then MUCH more efficiently evaluate the Expression object with the map of string variable names to float values many times over, with changing variable values. Runtime efficiency skyrocketed.

However, that was Java. Everyone knows Java. The new hot trend is functional programming (or Rust, still need to hop on that train). I also just missed writing in OCaml after I'd learned it in my CS 131 class at UCLA, and after working in some Monads into Java code I couldn't help but remember OCaml. Besides, who doesn't like a challenge?

Ultimately transcribing this wasn't that hard. I genuinely like the functional programming style, it feels very clean and elegant and much easier to debug. Monads are also just something that I have been enjoying utilizing in my code more and more, and especially as more and more languages adopt the higher-order functions and lambda functions that functional languages pioneered, the tenets of functional programming are a good set of rules and ideas to be familiar with. So enjoy messing with this code if you'd like! Main.ml contains a basic example of how to use the code.
