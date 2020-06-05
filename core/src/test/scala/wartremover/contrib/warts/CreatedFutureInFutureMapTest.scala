package org.wartremover
package contrib.test

import org.scalatest.funsuite.AnyFunSuite
import org.wartremover.contrib.warts.CreatedFutureInFutureMap
import org.wartremover.test.WartTestTraverser

import scala.concurrent.Future

class CreatedFutureInFutureMapTest extends AnyFunSuite with ResultAssertions {
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global

  test("error if Future is a return type of anonymous function`") {
    val result = WartTestTraverser(CreatedFutureInFutureMap) {
      val f = Future.successful(1)
      f.map {
        _ => f
      }
    }
    assertError(result)(CreatedFutureInFutureMap.message)
  }

  test("error if Future is a return type of the value") {
    val result = WartTestTraverser(CreatedFutureInFutureMap) {
      val f = Future.successful(1)
      val func: Function[Int, Future[String]] = _ => Future.successful("")

      f.map(func)
    }
    assertError(result)(CreatedFutureInFutureMap.message)
  }

  test("success if non-Future is a type of return value") {
    val result = WartTestTraverser(CreatedFutureInFutureMap) {
      val f = Future.successful(1)
      f.map {
        _ => 1
      }
    }
    assertEmpty(result)
  }

  test("can suppress warnings") {
    val result = WartTestTraverser(CreatedFutureInFutureMap) {
      @SuppressWarnings(Array("org.wartremover.contrib.warts.CreatedFutureInFutureMap"))
      def m() = {
        val f = Future.successful(1)
        f.map {
          _ => f
        }
      }
    }
    assertEmpty(result)
  }
}
