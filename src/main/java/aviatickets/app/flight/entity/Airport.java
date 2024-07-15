package aviatickets.app.flight.entity;

public record Airport(
		String code,
		String airportName,
		String city,
		String country,
		Character terminal,
		String timezone) {

}
