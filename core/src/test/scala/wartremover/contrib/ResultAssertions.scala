package org.wartremover.contrib
package test

import org.scalatest.Assertions

import org.wartremover.test.WartTestTraverser

trait ResultAssertions extends Assertions {

  def assertEmpty(result: WartTestTraverser.Result) = {
    assertResult(List.empty, "result.errors")(result.errors)
    assertResult(List.empty, "result.warnings")(result.warnings)
  }

  def assertError(result: WartTestTraverser.Result)(message: String) = assertErrors(result)(message, 1)

  def assertErrors(result: WartTestTraverser.Result)(message: String, times: Int) = {
    assertResult(List.fill(times)(message), "result.errors")(result.errors)
    assertResult(List.empty, "result.warnings")(result.warnings)
  }

  def assertWarnings(result: WartTestTraverser.Result)(message: String, times: Int) = {
    assertResult(List.empty, "result.errors")(result.errors)
    assertResult(List.fill(times)(message), "result.warnings")(result.warnings)
  }
}