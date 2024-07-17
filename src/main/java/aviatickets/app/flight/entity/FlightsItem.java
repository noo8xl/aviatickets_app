package aviatickets.app.flight.entity;

import java.util.List;

public record FlightsItem(
		Integer id,
		String flightNumber,
		String airline,

		// Itinerary with Transfer: the legs of the journey are detailed,
		// showing the transfer at some Airport.
		List<Leg> itinerary,
		Aircraft aircraft,

		Integer totalDistance, // => leg distance += leg distance
		String totalDuration,
		Price price,
		Short passengerCount,
		Short availableSits
) {
}
