package org.wartremover
package contrib.warts

import scala.concurrent.Future

object CreatedFutureInFutureMap extends WartTraverser {

  val message: String =
    """create new Future instance inside of Future#map may lead to race conditions.
      |To chain the result of Future to other Future, use flatMap.
      |""".stripMargin

  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._

    val mapMethodName: TermName = TermName("map")
    val futureSymbol = typeOf[Future[Any]]
    val mapMethod = futureSymbol.member(mapMethodName)
    val futureTypeSymbol = futureSymbol.typeSymbol
    require(mapMethod != NoSymbol)
    require(futureTypeSymbol != NoSymbol)

    new Traverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          // Ignore trees marked by SuppressWarnings
          case t if hasWartAnnotation(u)(t) =>
          case Apply(Apply(method, List(callback)), _) if method.symbol == mapMethod && callback.tpe
            .typeArgs(1)
            .typeSymbol == futureTypeSymbol =>
            error(u)(tree.pos, message)
            super.traverse(tree)
          case _ =>
            super.traverse(tree)
        }
      }
    }
  }
}
