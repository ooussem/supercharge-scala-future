package exercises.action.fp.search

import exercises.action.DateGenerator._
import exercises.action.fp.IO
import exercises.action.fp.search.Airport._
import exercises.action.fp.search.SearchFlightGenerator._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant, LocalDate}
import scala.concurrent.ExecutionContext
import scala.util.Random

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.fp.search.SearchFlightServiceTest
class SearchFlightServiceTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  import ExecutionContext.Implicits.global

  test("fromTwoClients example") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight2 = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight3, flight1)))
    val client2 = SearchFlightClient.constant(IO(List(flight2, flight4)))

    val service = SearchFlightService.bestFromTwoClients(client1, client2)(ExecutionContext.global)
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1, flight2, flight3, flight4)))
  }

  test("fromTwoClients example cheapest") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight2 = Flight("1", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight3, flight1)))
    val client2 = SearchFlightClient.constant(IO(List(flight2, flight4)))

    val service = SearchFlightService.bestFromTwoClients(client1, client2)(ExecutionContext.global)
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1, flight3, flight4)))
  }

  test("fromTwoClients should throw error") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight2 = Flight("1", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO.fail(new Exception("No Flights")))
    val client2 = SearchFlightClient.constant(IO(List(flight2, flight4)))

    val service = SearchFlightService.bestFromTwoClients(client1, client2)(ExecutionContext.global)
    val result  = service.search(parisOrly, londonGatwick, today).attempt.unsafeRun()

    for {
      resultR <- result
    } yield {
      assert(result.isSuccess)
      assert(resultR == SearchResult(List(flight2, flight4)))
    }
  }

  test("fromTwoClients should throw error PBT") {
    forAll(airportGen, airportGen, dateGen, clientGen, clientGen) { (from, to, today, client1, client2) =>
      val service = SearchFlightService.bestFromTwoClients(client1, client2)(ExecutionContext.global)
      val result = service.search(from, to, today).attempt.unsafeRun()

      assert(result.isSuccess)
    }
  }

  test("fromClients") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight2 = Flight("1", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")
    val flight5 = Flight("5", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(223), 2, 100, "")

    val client1 = SearchFlightClient.constant(IO(List(flight3, flight1)))
    val client2 = SearchFlightClient.constant(IO(List(flight2)))
    val client3 = SearchFlightClient.constant(IO(List(flight4)))
    val client4 = SearchFlightClient.constant(IO(List(flight5)))

    val service = SearchFlightService.fromClients(List(client1, client2, client3, client4))
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1, flight3, flight4, flight5)))
  }

  test("fromClients should throw error PBT") {
    forAll(airportGen, airportGen, dateGen, Gen.listOf(clientGen)) { (from, to, today, clients) =>
      val service1 = SearchFlightService.fromClients(clients)
      val service2 = SearchFlightService.fromClients(Random.shuffle(clients))

      val result1 = service1.search(from, to, today).attempt.unsafeRun()
      val result2 = service2.search(from, to, today).attempt.unsafeRun()

      assert(result1.isSuccess)
      assert(result1 == result2)
    }
  }
}
