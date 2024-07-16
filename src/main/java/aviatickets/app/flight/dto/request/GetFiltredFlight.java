package aviatickets.app.flight.dto.request;

import java.time.LocalDateTime;

import aviatickets.app.flight.entity.CabinClass;
import aviatickets.app.flight.entity.Location;

// GetTicketList -> can contain null fields if shouldn't be filtred *
public record GetFiltredFlight(
		String departureAirport,
		String arrivalAirport,
		Location location,
		LocalDateTime departureDate, // null or Date
		LocalDateTime returnDate, // null or Date
		Short passengerCount, // -> passenger * price
		CabinClass cabinClass, // as string "Economy", "Business", "First"
		FilterOptions filterOptions // as enum CHEAPEST, FASTEST, DIRECT
) {
}

// passenger -> age and price will be a details at order page *