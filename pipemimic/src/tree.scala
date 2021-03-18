package pipemimic

import CartesianUtils.CartesianProduct

abstract class GraphTree[T]

case class GraphTreeOr[T](l: List[GraphTree[T]]) extends GraphTree[T]
case class GraphTreeAnd[T](l: List[GraphTree[T]]) extends GraphTree[T]
case class GraphTreeLeaf[T](s: String, l: List[Tuple3[T, T, String]]) extends GraphTree[T]

object GraphTree {
  def GraphTreeEmptyLeaf = GraphTreeLeaf("", List.empty)

  def DNFOfTree[T](t: GraphTree[T]): List[Tuple2[String, List[Tuple3[T, T, String]]]] = {
    def joinGraphs(a: Tuple2[String, List[Tuple3[T, T, String]]], b: Tuple2[String, List[Tuple3[T, T, String]]]): Tuple2[String, List[Tuple3[T, T, String]]] = {
      a match { case (an, al) => b match { case (bn, bl) => (an ++ bn, al ++ bl) } }
    }
    t match {
      case GraphTreeOr(l) => l.map(DNFOfTree).foldLeft(List[Tuple2[String, List[Tuple3[T, T, String]]]]())(_ ++ _)
      case GraphTreeAnd(l) => CartesianProduct(l.map(DNFOfTree)).map(_.foldLeft(("", List[Tuple3[T, T, String]]()))((x, y) => joinGraphs(x, y)))
      case GraphTreeLeaf(s, l) => List((s, l))
    }
  }

  def GraphTreeSimplify[T](g: GraphTree[T]): GraphTree[T] = {
    g match {
      case GraphTreeOr(List(x)) => GraphTreeSimplify(x)
      case GraphTreeOr(l) => GraphTreeOr(l.map(GraphTreeSimplify))
      case GraphTreeAnd(List(x)) => GraphTreeSimplify(x)
      case GraphTreeAnd(l) => GraphTreeAnd(l.map(GraphTreeSimplify))
      case _ => g
    }
  }

  def TreeOfDNF[T](l: List[Tuple2[String, List[Tuple3[T, T, String]]]]): GraphTree[T] = {
    GraphTreeSimplify(GraphTreeOr(l.map(x => GraphTreeLeaf(x._1, x._2))))
  }

  def DNFStringOfTreeHelper[T](print_node: T => String, e: Tuple3[T, T, String]): String = {
    e match {
      case (s, d, label) => s": ${print_node(s)}-$label->${print_node(d)} "
    }
  }

  def DNFStringOfTree[T](print_node: T => String, t: GraphTree[T]): String = {
    t match {
      case GraphTreeOr(l) => s"Or(${l.map(DNFStringOfTree(print_node, _)).foldLeft("")((a, b) => s"$b-$a")})"
      case GraphTreeAnd(l) => s"And(${l.map(DNFStringOfTree(print_node, _).foldLeft("")((a, b) => s"$b-$a"))})"
      case GraphTreeLeaf(s, l) => l.map(DNFStringOfTreeHelper(print_node, _)).foldLeft(s)(_ ++ _)
    }
  }

  def GraphTreeMap[A, B](f: A => B, g: GraphTree[A]): GraphTree[B] = {
    g match {
      case GraphTreeAnd(l) => GraphTreeAnd(l.map(GraphTreeMap(f, _)))
      case GraphTreeOr(l) => GraphTreeOr(l.map(GraphTreeMap(f, _)))
      case GraphTreeLeaf(s, l) => GraphTreeLeaf(s, l.map(x => (f(x._1), f(x._2), x._3)))
    }
  }

  def GraphTreeMapPair[A, B](f: Tuple3[A, A, String] => Tuple3[B, B, String], g: GraphTree[A]): GraphTree[B] = {
    g match {
      case GraphTreeAnd(l) => GraphTreeAnd(l.map(GraphTreeMapPair(f, _)))
      case GraphTreeOr(l) => GraphTreeOr(l.map(GraphTreeMapPair(f, _)))
      case GraphTreeLeaf(s, l) => GraphTreeLeaf(s, l.map(f))
    }
  }
}