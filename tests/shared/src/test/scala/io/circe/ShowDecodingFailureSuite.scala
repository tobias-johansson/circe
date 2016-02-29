package io.circe

import cats.Show
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import io.circe.CursorOp._
import io.circe.tests.CirceSuite

trait GenCursorOps {
  val arrayMoves: Gen[List[CursorOp]] =
    Gen.listOf(Gen.oneOf(
      Gen.const(MoveLeft),
      Gen.const(MoveRight),
      Gen.choose(1, 10000) map LeftN,
      Gen.choose(1, 10000) map RightN
    ))

  val downFields: Gen[List[CursorOp]] =
    Gen.listOf(Gen.identifier map DownField)
}

class ShowDecodingFailureSuite extends CirceSuite with GenCursorOps {

  val show = Show[DecodingFailure].show _

  test("simple example") {
    val ops = List(MoveRight, MoveRight, DownArray, DownField("bar"), DownField("foo")) map HistoryOp.ok

    assert(show(DecodingFailure("failure", ops)) === "failure, at .foo.bar[2]")
  }

  test("advanced example") {
    val ops = List(
      DeleteGoFirst,
      MoveLeft, LeftN(2), RightN(5),
      DownArray, DownArray, DownField("bar"),
      MoveUp, MoveRight, MoveRight,
      DownArray, DownField("foo")) map HistoryOp.ok

    assert(show(DecodingFailure("failure", ops)) === "failure, at .foo.bar[0][2]{|<-!}")
  }

  test("field selection") {
    check(forAll(downFields) { moves =>
      val ops = moves map HistoryOp.ok
      val selection = moves.foldRight("") {
        case (DownField(f), s) => s"$s.$f"
        case (_, s)            => s
      }

      show(DecodingFailure("failure", ops)) === s"failure, at $selection"
    })
  }

  test("array indexing") {
    check(forAll(arrayMoves) { moves =>
      val ops = (moves ++ List(DownArray)) map HistoryOp.ok
      val index = moves.foldLeft(0) {
        case (i, MoveLeft)  => i - 1
        case (i, MoveRight) => i + 1
        case (i, LeftN(n))  => i - n
        case (i, RightN(n)) => i + n
        case (i, _)         => i
      }

      show(DecodingFailure("failure", ops)) === s"failure, at [$index]"
    })
  }
}
