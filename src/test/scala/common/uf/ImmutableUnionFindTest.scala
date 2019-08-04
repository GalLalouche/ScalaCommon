package common.uf

class ImmutableUnionFindTest extends UnionFindTest {
  override protected def createUnionFind[A](xs: IterableOnce[A]) = ImmutableUnionFind(xs)
}
