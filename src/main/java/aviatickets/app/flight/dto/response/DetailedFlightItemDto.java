package aviatickets.app.flight.dto.response;

import aviatickets.app.flight.entity.Aircraft;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Positive;

public record DetailedFlightItemDto(
		@Positive
		Integer id,
		@NotBlank
		String flightNumber,
		String airline,
		Aircraft aircraft,





		Float price
) {
}
