package aviatickets.app.flight.dto.response;

import jakarta.validation.constraints.Positive;

public record ShortFlightDto(
		@Positive
		Integer flight_id,
		String flightNumber,
		String duration,
		Float price
) {
}
