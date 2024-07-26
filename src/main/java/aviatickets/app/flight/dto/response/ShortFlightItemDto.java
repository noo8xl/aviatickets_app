package aviatickets.app.flight.dto.response;

import jakarta.validation.constraints.Positive;

public record ShortFlightItemDto(
		@Positive
		Integer id,
		String flightNumber,
		String duration,
		Float price
) {
}
