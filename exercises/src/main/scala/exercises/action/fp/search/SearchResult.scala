package exercises.action.fp.search

// `flights` must be ordered using `SearchResult.bestOrdering`
case class SearchResult private (flights: List[Flight]) {
  val cheapest: Option[Flight] = flights.minByOption(_.unitPrice)
  val fastest: Option[Flight]  = flights.minByOption(_.duration)
  val best: Option[Flight]     = flights.minOption(SearchResult.bestOrdering)
}

object SearchResult {
  def apply(listFlight: List[Flight]): SearchResult = {
    new SearchResult(
      listFlight
        .groupBy(flight => flight.flightId)
        .view
        .mapValues(flights => flights.minBy(_.unitPrice))
        .values
        .toList
        .sorted(bestOrdering)
    )
  }
  
  // Order by number of stops (0, 1, 2, ...) and then by price.
  // For example, sorting the following flights
  // flight A: 2 stops, 100$
  // flight B: 0 stop , 120$
  // flight C: 1 stop ,  50$
  // flight D: 0 stop ,  80$
  // produces: flight D, flight B, flight C, flight A
  val bestOrdering: Ordering[Flight] =
    Ordering.by(flight => (flight.numberOfStops, flight.unitPrice))

}
