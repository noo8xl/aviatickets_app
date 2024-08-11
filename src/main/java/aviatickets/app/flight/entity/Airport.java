package aviatickets.app.flight.entity;

import jakarta.validation.constraints.Positive;

public record Airport(
		@Positive
		Integer id,
		String code,
		String airportName,
		String city,
		String country,
		String terminal,
		String timezone,
		Location location,
		AirportContacts contacts
) {
}
