package pipemimic

import CartesianUtils.CartesianProduct

/**
  * A GraphTree is a data structure which is used to represent a set of graphs that are mostly similar, but with a few
  * small differences. For example, suppose we want to represent two graphs: one which adds an edge e1 to a graph G, and
  * another which adds a different edge e2 to G. Rather than representing these as {G + e1, G + e2}, a GraphTree would
  * represent them as G + {e1 or e2}.
  * The motivation for a GraphTree is to more easily represent the case in which, for example, a litmus test outcome may
  * be observable in any one of a number of possible graphs, some of which will generally look very similar.
  * @tparam T type of tree nodes
  */
abstract class GraphTree[T]

case class GraphTreeOr[T](l: List[GraphTree[T]]) extends GraphTree[T] // FIXME so where do we put e1 or e2?
case class GraphTreeAnd[T](l: List[GraphTree[T]]) extends GraphTree[T]
case class GraphTreeLeaf[T](s: String, l: List[(T, T, String)]) extends GraphTree[T]

object GraphTree {
  def GraphTreeEmptyLeaf[T]: GraphTreeLeaf[T] = GraphTreeLeaf("", List.empty[(T, T, String)])

  /**
    * The DNFOfTree of a GraphTree is an explicit list of the graphs represented by the tree, i.e., no longer in the
    * compacted GraphTree format.
    * @param t GraphTree
    * @tparam T type of graph nodes
    * @return list of graphs (String: name, list (t, t): edges in one graph)
    */
  def DNFOfTree[T](t: GraphTree[T]): List[(String, List[(T, T, String)])] = {
    /**
      * Simply concat two strings and lists
      * FIXME so a graph is a GraphTree Leaf ?
      * @param a former graph
      * @param b latter graph
      * @return (a.name ++ b.name, a.list ::: b.list)
      */
    def joinGraphs(a: (String, List[(T, T, String)]), b: (String, List[(T, T, String)])): (String, List[(T, T, String)]) = {
      a match { case (an, al) => b match { case (bn, bl) => (an + bn, al ::: bl) } }
    }
    t match {
      case GraphTreeOr(l) => l.flatMap(DNFOfTree[T]) /* every element in l is a graph */
      case GraphTreeAnd(l) => /* first calculate each branch - List(List(g1, g2), List(g3, g4)) */
        val _l: List[List[(String, List[(T, T, String)])]] = l.map(DNFOfTree)
        /* cartesian product yields every combination of graph edge set, then fold list of set into list of edges */
        CartesianProduct(_l).map(_.foldLeft(("", List.empty[(T, T, String)]))((h, n) => joinGraphs(h, n)))
      case GraphTreeLeaf(s, l) => List((s, l)) /* a leaf represents one graph */
    }
  }

  /**
    * GraphTreeSimplify tries to represent a GraphTree in a simpler but equivalent form.
    * It doesn't guarantee minimality.
    * @param g a GraphTree
    * @tparam T type of graph nodes
    * @return simplified GraphTree
    */
  def GraphTreeSimplify[T](g: GraphTree[T]): GraphTree[T] = {
    g match {
      case GraphTreeOr(List(x)) => GraphTreeSimplify(x)
      case GraphTreeOr(l) => GraphTreeOr(l.map(GraphTreeSimplify))
      case GraphTreeAnd(List(x)) => GraphTreeSimplify(x)
      case GraphTreeAnd(l) => GraphTreeAnd(l.map(GraphTreeSimplify))
      case _ => g
    }
  }

  /**
    * converts a list of graphs into GraphTree representation.
    * @param l list of graphs
    * @tparam T type of graph nodes
    * @return GraphTree
    */
  def TreeOfDNF[T](l: List[(String, List[(T, T, String)])]): GraphTree[T] = {
    GraphTreeSimplify(GraphTreeOr(l.map(x => GraphTreeLeaf(x._1, x._2))))
  }

  /**
    * convert GraphTree to string
    * @param print_node node formatter
    * @param t tree
    * @tparam T type of node
    * @return formatted string represents given GraphTree
    */
  def DNFStringOfTree[T](print_node: T => String, t: GraphTree[T]): String = {
    /**
      * print one edge
      * @param print_node convert edge to string
      * @param e edge
      * @tparam A type of node
      * @return string represent one edge
      */
    def helper[A](print_node: A => String, e: (A, A, String)): String = {
      e match {
        case (s, d, label) => s": ${print_node(s)}-$label->${print_node(d)} "
      }
    }
    t match {
      case GraphTreeOr(l) => s"Or(${l.map(DNFStringOfTree(print_node, _)).mkString})"
      case GraphTreeAnd(l) => s"And(${l.map(DNFStringOfTree(print_node, _)).mkString})"
      case GraphTreeLeaf(s, l) => l.map(helper(print_node, _)).mkString(s, "", "")
    }
  }

  def GraphTreeMap[A, B](f: A => B, g: GraphTree[A]): GraphTree[B] = {
    g match {
      case GraphTreeAnd(l) => GraphTreeAnd(l.map(GraphTreeMap(f, _)))
      case GraphTreeOr(l) => GraphTreeOr(l.map(GraphTreeMap(f, _)))
      case GraphTreeLeaf(s, l) => GraphTreeLeaf(s, l.map(x => (f(x._1), f(x._2), x._3)))
    }
  }

  def GraphTreeMapPair[A, B](f: ((A, A, String)) => (B, B, String), g: GraphTree[A]): GraphTree[B] = {
    g match {
      case GraphTreeAnd(l) => GraphTreeAnd(l.map(GraphTreeMapPair(f, _)))
      case GraphTreeOr(l) => GraphTreeOr(l.map(GraphTreeMapPair(f, _)))
      case GraphTreeLeaf(s, l) => GraphTreeLeaf(s, l.map(f))
    }
  }
}