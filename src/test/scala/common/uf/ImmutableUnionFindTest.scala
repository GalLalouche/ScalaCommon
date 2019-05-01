package common.uf

class ImmutableUnionFindTest extends UnionFindTest {
  override protected def createUnionFind[A](xs: TraversableOnce[A]) = ImmutableUnionFind(xs)
}
