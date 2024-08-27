package aviatickets.app.flight.dto.response;

import java.sql.Date;

public record ShortFlightDto(
		String flightNumber,

		String departureAirportName,
		String departureAirportCode,
		String departureAirportCity,

		String arrivalAirportName,
		String arrivalAirportCode,
		String arrivalAirportCity,

		String duration,
		Date departureDate,
		Float price
) {

}
