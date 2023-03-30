package semagrams

/** In this package, we define nested ACSets.
  *
  * In a nested acset, each element of an acset has an associated acset, which
  * we call a subacset. The schema for a nested acset has an acset schema
  * associated with each object, which defines the schema for the acsets
  * associated to the parts assigned to that object.
  *
  * A nested part of a nested acset is a list of (ob, id) pairs, which at each
  * step tell you how to get to the next nested acset. The type of such a nested
  * part is just the list of obs.
  *
  * Morphisms in a nested acset schema go from nested part types to nested part
  * types. The acset schema that contains the morphism is the "highest point"
  * that the morphism can go, i.e. the only way to get from a nested part to
  * another nested part via a morphism is to go all the way up to where the
  * morphism is defined, and then go back down to the target of the morphism.
  *
  * Nested acsets are still not fully understood mathematically.
  *
  * @todo
  *   We have a confusion of terminology in that we use "part" to refer to
  *   both an element of the category of elements of a single acset, and also
  *   an element of the category of elements of the entire nested acset. We should
  *   fix this.
  */
package object acsets {}
