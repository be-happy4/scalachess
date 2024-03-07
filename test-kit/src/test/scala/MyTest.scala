package chess

import chess.Square.*
import chess.format.{Uci, Visual}
import chess.format.pgn.{Parser, PgnStr, Reader, SanStr}

import java.util.Scanner
import scala.util.Either
import chess.MoveOrDrop.applyGame

class MyTest extends ChessTest:
  test("Italian Game: Two Knights Defense Opening"):
    val game = Game(Board init chess.variant.Standard, White)
      .playMoves(E2 -> E4)
      .get
      .playMoves(E7 -> E5)
      .get
      .playMoves(G1 -> F3)
      .get
      .playMoves(B8 -> C6)
      .get
      .playMoves(F1 -> C4)
      .get
      .playMoves(G8 -> F6)
      .get
    println(game.board.minors)
//    println(Fen.write(game))
    println(Visual.addNewLines(Visual.>>(game.board)))

  test("Full Path Test"):
    val sit = Reader.full(PgnStr("e4 e5 Nf3 Nc6 Bc4 Nf6")).flatMap(_.valid).get
    //    println(Fen.write(sit.state))
    println(Visual.addNewLines(Visual.>>(sit.state.board)))

object Main extends ChessTest:
  def interleaveWithTwoPositions(): Unit =
    val in   = new Scanner(System.in)
    var game = Game(Board init chess.variant.Standard, White)
    var flag = true
    println(Visual.addNewLines(game.board.visual))
    while flag
    do
      val (from, to) = (allKeys(in.next()), allKeys(in.next()))
      game = game.playMove(from, to).get
      println(Visual.addNewLines(game.board.visual))
      flag = in.hasNext

  def moveOnGame(now: Game, sanStrArray: String*): Either[ErrorStr, Game] =
    var game: Either[ErrorStr, Game] = Right(now)
    for sanStr <- sanStrArray do
      game = game
        .flatMap: old =>
          Parser.sanOnly(SanStr(sanStr))
            .flatMap: san =>
              san.apply(old.situation)
                .map: move =>
                  move.applyGame(old)
    game

  def getGame(sanStrArray: String*): Either[ErrorStr, Game] =
    moveOnGame(Game(Board init chess.variant.Standard, White), sanStrArray*)

  def processEnd(game: Game): Boolean =
    val sit = game.situation
    if sit.end then
      if sit.checkMate then println(s"${sit.winner.get} win.")
      else if sit.staleMate || sit.autoDraw then println("Draw.")
      return false
    true

  def interleaveWithSan(initSanStrArray: String*): Unit =
    val in   = new Scanner(System.in)
    var game: Game = getGame(initSanStrArray*) match
      case Left(err) => println(err); getGame().get
      case Right(newGame) => newGame
    def printInfo(): Unit =
      println(Visual.addNewLines(game.board.visual))
    printInfo()
    var flag = processEnd(game)
    while flag do
      var wrongInput = true
      while wrongInput do
        print("Next move: ")
        moveOnGame(game, in.next().split(" ")*) match
          case Left(err) => println(err)
          case Right(newGame) =>
            game = newGame
            wrongInput = false
      printInfo()
      flag = processEnd(game)

  def main(args: Array[String]): Unit =
//    interleaveWithSan("f3 e5 g4 Qh4".split(" ")*)
    interleaveWithSan("e4 e5 Nc3 Nc6 Ne2".split(" ")*)
//    interleaveWithSan()
