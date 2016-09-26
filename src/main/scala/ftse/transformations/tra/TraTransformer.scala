package ftse.transformations.tra
import ftse.transformations.AbstrTransformer
import ftse.formalism.tra.Tra


// TODO (Martin->Alex) is this abstraction necessary, and if yes, why to implement the identity transformation method???

/**
 * This trait is used as a base implementation of the AbstrTransformer-Trait.
 * It is needed for the stackable trait pattern.
 */
trait TraTransformer extends AbstrTransformer[Tra, Tra] {
	
  /** The transformation method implementation 
   * 
   * It implements the only method of the AbstrTransformer trait, 
   * which is the identity function in its basic version  
   * 
   * @param tra A transition system as source upon which to apply the implemented transformation 
   * @return The transformed transition system
   */
  override def transform(tra: Tra): Tra = {
	tra
  }
}