package ftse.transformations

/**
 * Common Transformation Interface
 * 
 * Should be implemented in all kinds of transformations described in that package
 * 
 * @tparam S  The type of a source to be transformed  
 * @tparam T The type of the resulting transformation target 
 */
trait AbstrTransformer[S,T] {
  
  /**
   * Abstract transformation method to be implemented by each specific transformation
   * 
   * @param l Model of source type S
   * @return The transformation result of type T 
   */
  def transform(l : S) : T 
}
