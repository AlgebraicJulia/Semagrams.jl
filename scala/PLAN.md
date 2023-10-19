# Validity of Patches

Three building blocks for patches:

- Sets
- Typed sets
- Maps

Sets are simple; we simply record their changes via additions and deletions.

Typed sets and maps seem similar: they are both keeping track of a map from keys to values. However, in the first instance, once a key has been associated with a certain value, it should not be associated with a new value in the future. It might be killed and resurrected, but never *changed*. This would be used for:

- Set of properties in a schema
- Set of entities in an instance

# Balloon-based Semagrams

- Balloon for global state
- Balloon for acset
- Balloon for semagram, handling bindings

# How to display an acset

For now, stick with extractor design.
