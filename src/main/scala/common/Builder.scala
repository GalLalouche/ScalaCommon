package common

/**
 * An immutable builder class.
 * This builder aggregates all parameters in a map, so implementing classes need only implement
 * a cloning method that converts a map of parameters and values to a new object.
 * This class uses the stack trace to match parameter names with updating functions.
 * @param parameters this value should at first hold all default mappings of parameters and values
 * @param [T] This should be Builder[ImplementingClass] to allow the builder to return the correct type on clone and update.
 */
abstract class Builder[T](parameters: Map[String, Any]) {
  /** 
   * Creates a new builder
   * @param parameters all parameters to the values of the builder   
   */
	protected def clone(parameters: Map[String, Any]): T
  
  /** 
   * Updates the field that matches <b>the calling function's name</b>.
   * How cool is that?
   * @param o The new value   
   */
	protected def update(o: Any): T = {
		val nameOfInvokingMethod: String = Thread.currentThread().getStackTrace.apply(2).getMethodName
		require(parameters contains nameOfInvokingMethod, s"No key found for $nameOfInvokingMethod. Every key should have a default value")
		clone(parameters.updated(nameOfInvokingMethod, o))
	}
  
	
  /** 
   * Gets the parameter matching the key and and casts it
   * @param key the key to look
   */
	protected def asT[S](key: String): S = parameters(key).asInstanceOf[S]
}
