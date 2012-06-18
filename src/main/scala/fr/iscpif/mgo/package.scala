

package fr.iscpif


package object mgo {
  
  implicit def traversable2Population[G, I](seq: Traversable[PopulationElement[G, I]]) =
    new Population[G, I] {
      override val content = seq.toIndexedSeq
    }
  
  implicit def population2IndexedSeq[G, I](pop: Population[G, I]) = pop.content

}