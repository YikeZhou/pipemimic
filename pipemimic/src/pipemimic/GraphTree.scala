package pipemimic

/**
  * A GraphTree is a data structure which is used to represent a set of graphs that are mostly similar, but with a few
  * small differences. For example, suppose we want to represent two graphs: one which adds an edge e1 to a graph G, and
  * another which adds a different edge e2 to G. Rather than representing these as {G + e1, G + e2}, a GraphTree would
  * represent them as G + {e1 or e2}.
  * The motivation for a GraphTree is to more easily represent the case in which, for example, a litmus test outcome may
  * be observable in any one of a number of possible graphs, some of which will generally look very similar.
  * @tparam A type of tree nodes
  */
sealed abstract class GraphTree[A] {
  def map[B](f: (A, A, String) => (B, B, String)): GraphTree[B]

  def map[B](f: A => B): GraphTree[B] = map((s, d, str) => (f(s), f(d), str))

  /**
    * An explicit list of the graphs represented by the tree, i.e., no longer in the compacted GraphTree format.
    * @return list of graphs represented by the tree
    */
  def flatten: List[(String, List[(A, A, String)])]

  def toString(printNode: A => String): String
}

case class GraphTreeOr[A](l: List[GraphTree[A]]) extends GraphTree[A] {
  override def map[B](f: (A, A, String) => (B, B, String)): GraphTree[B] = GraphTreeOr(l.map(_.map(f)))

  override def flatten: List[(String, List[(A, A, String)])] = l.flatMap(_.flatten)

  override def toString(printNode: A => String): String = s"Or(${l.map(_.toString(printNode)).mkString})"
}

case class GraphTreeAnd[A](l: List[GraphTree[A]]) extends GraphTree[A] {
  override def map[B](f: (A, A, String) => (B, B, String)): GraphTree[B] = GraphTreeAnd(l.map(_.map(f)))

  override def flatten: List[(String, List[(A, A, String)])] = {
    /**
      * Simply concat two strings and lists
      * @param a former graph
      * @param b latter graph
      * @return (a.name + b.name, a.list ::: b.list)
      */
    def joinGraphs(a: (String, List[(A, A, String)]), b: (String, List[(A, A, String)])): (String, List[(A, A,
      String)]) = {
      a match { case (an, al) => b match { case (bn, bl) => (an + bn, al ::: bl) } }
    }
    CartesianProduct(l.map(_.flatten)).map(_.reduce(joinGraphs))
  }

  override def toString(printNode: A => String): String = s"And(${l.map(_.toString(printNode)).mkString})"
}

case class GraphTreeLeaf[A](s: String, l: List[(A, A, String)]) extends GraphTree[A] {
  override def map[B](f: (A, A, String) => (B, B, String)): GraphTree[B] = GraphTreeLeaf(s, l map {
    case (s, d, str) => f(s, d, str)
  })

  override def flatten: List[(String, List[(A, A, String)])] = if (l.isEmpty) Nil else (s, l) :: Nil

  override def toString(printNode: A => String): String = l.map {
    case (a, b, label) => s"${printNode(a)}-$label->${printNode(b)}"
  } match {
    case Nil => "<empty leaf>"
    case leaves => leaves.mkString(s + "(", ",", ")")
  }
}

object TreeNodeType extends Enumeration {
  val Or, And, Leaf = Value
}

object GraphTree {

  def GraphTreeEmptyLeaf[A]: GraphTreeLeaf[A] = GraphTreeLeaf("", List.empty[(A, A, String)])

  implicit class GraphTreeMethods[A](g: GraphTree[A]) {
    def isEmpty: Boolean = {
      g match {
        case GraphTreeOr(l) => l.isEmpty || l.map(_.isEmpty).reduce(_ && _)
        case GraphTreeAnd(l) => l.isEmpty || l.map(_.isEmpty).reduce(_ && _)
        case GraphTreeLeaf(s, l) => l.isEmpty
      }
    }

    /**
      * Tries to represent a GraphTree in a simpler but equivalent form. It doesn't guarantee minimality.
      * @return simplified GraphTree equivalent to `g`
      */
    def simplify: GraphTree[A] = {
      if (g.isEmpty) return GraphTreeEmptyLeaf[A]

      g match {
        case GraphTreeOr(l) =>
          val children = l.filterNot(_.isEmpty).map(_.simplify)
          if (children.length == 1) children.head else GraphTreeOr(children)
        case GraphTreeAnd(l) =>
          val children = l.filterNot(_.isEmpty).map(_.simplify)
          if (children.length == 1) children.head else GraphTreeAnd(children)
        case _ => g
      }
    }
  }

  /**
    * converts a list of graphs into GraphTree representation.
    * @param l list of graphs
    * @tparam A type of graph nodes
    * @return GraphTree
    */
  def apply[A](l: List[(String, List[(A, A, String)])]): GraphTree[A] =
    GraphTreeOr(l.map { case (str, value) => GraphTreeLeaf(str, value) }).simplify

  def apply[A](t: TreeNodeType.Value, l: List[GraphTree[A]]): GraphTree[A] = {
    require(t == TreeNodeType.Or || t == TreeNodeType.And)
    (t: @unchecked) match {
      case TreeNodeType.Or => GraphTreeOr(l).simplify
      case TreeNodeType.And => GraphTreeAnd(l).simplify
    }
  }

  def apply[A](t: TreeNodeType.Value, s: String, l: List[(A, A, String)]): GraphTree[A] = {
    require(t == TreeNodeType.Leaf)
    GraphTreeLeaf(s, l).simplify
  }
}