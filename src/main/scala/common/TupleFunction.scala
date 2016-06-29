package common

/**
 * This implicitly converts functions of two parameters to functions who receive a tuple
 */
object TupleFunction {
  implicit def richFunction2[T, S, U](f: (T, S) => U): ((T,S)) => U = tuple => f(tuple._1, tuple._2)
  implicit def richFunction3[T, S, U, W](f: (T, S, W) => U): ((T,S, W)) => U = tuple => f(tuple._1, tuple._2, tuple._3)
}
