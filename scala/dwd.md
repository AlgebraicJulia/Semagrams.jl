# Plan for ASKEM Hackathon

## Needed features on day 1

- Wiring diagrams that work, with palettes and labels
- Petri nets
- Integration with observable

## Todo during hackathon

- Serialization
- More integration
- Recursive wiring diagrams

# Implementation

Question: do we allow nested *and* non-nested acsets? Because the current way that sprites are handled, we could pass an acset instead of a propmap. The problem with this is that sub-acsets are not "standalone".

The minimum change needed to be made is that sprites should have subsprites that can change dynamically. This is actually not too hard, as long as we forget about the handle system and just attach the click/hover handlers directly in the sprite code. Which is kind of what one might want to do anyways.

The remnant of the "handle" system then comes simply via what you pass into the click/hover handlers.

Basically, we change the sprite interface so that you *pass in* a function with two arguments, one being a svg element, and the other being the part/handle that you want to attach to that svg element.

The other thing is that we need a dynamic list of handles. I think perhaps the best thing to do is to hack 

