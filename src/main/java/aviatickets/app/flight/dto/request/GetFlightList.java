package aviatickets.app.flight.dto.request;

import java.time.LocalDateTime;

// GetTicketList -> can contain null fields if shouldn't be filtred *
public record GetFlightList(
		LocalDateTime flightDate,
		String departureAirport,
		String arrivalAirport,
		LocalDateTime departureDate,
		LocalDateTime returnDate, // null or Date
		Integer passengerCount, // passenger * price
		String cabinClass,
		Boolean transfer // ??
) {
}

// passenger -> age and price will be a details at order page *
// transfer -> not so good. should be enum with smthg like (SLOWEST, CHEEPEST,
// FASTEST, etc)