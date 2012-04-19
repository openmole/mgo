/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

trait GAGenomeWithSigma extends GAGenome with Sigma { self =>
  def size: Int
  def wrappedValues = values ++ sigma
  
  override def updatedValues(_values: IndexedSeq [Double]) = 
    new GAGenomeWithSigma {
      def values = _values
      def sigma = self.sigma
      def size = self.size
    }
   

  override def updatedSigma(_sigma: IndexedSeq [Double]) = 
    new GAGenomeWithSigma {
      def values = self.values
      def sigma = _sigma
      def size = self.size
    }
    
  
                                  
}



