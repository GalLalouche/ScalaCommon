package common.uf

class MutableUnionFindTest extends UnionFindTest {
  override protected def createUnionFind[A](xs: IterableOnce[A]) = MutableUnionFind(xs)
}

