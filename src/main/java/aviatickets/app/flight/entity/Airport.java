package aviatickets.app.flight.entity;

public record Airport(
		Integer id,
		String code,
		String airportName,
		String city,
		String country,
		Character terminal,
		String timezone,
		Location location,
		AirportContacts contacts
) {
}
