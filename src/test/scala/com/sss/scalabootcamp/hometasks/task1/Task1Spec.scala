package com.sss.scalabootcamp.hometasks.task1

import com.sss.scalabootcamp.hometasks.task1.Task1.lcm
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Task1Spec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "3 and 5 lcm" should "work correctly" in {
    lcm(3, 5) shouldEqual 15
  }
}
