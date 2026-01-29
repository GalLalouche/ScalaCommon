package common

/**
 * Asserts with a higher guard than regular asserts, since assertions are usually enabled by default
 * in Scala, and we might we want to enable them in test runs only for performance reasons.
 *
 * To make this work, add an empty file named "witness" to the root of the test-resources folder.
 */
object TestAsserts {
  private val isTest: Boolean = getClass.getClassLoader.getResource("witness") != null
  @inline def testAssert(b: => Boolean): Unit = if (isTest) assert(b)
  @inline def testAssert(b: => Boolean, msg: => Any): Unit = if (isTest) assert(b, msg)
}
