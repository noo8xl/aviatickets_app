package aviatickets.app.flight.entity;

import jakarta.validation.constraints.Positive;

import java.util.List;

public record FlightsItem(
		@Positive
		Integer id,
		String flightNumber,
		String airline,

		// Itinerary with Transfer: the legs of the journey are detailed,
		// showing the transfer at some Airport.
		List<Leg> itinerary,
		Aircraft aircraft,

		@Positive
		Short totalDistance, // => leg distance += leg distance
		String totalDuration,
		Price price,
		@Positive
		Short passengerCount,
		@Positive
		Short availableSits
) {
}
