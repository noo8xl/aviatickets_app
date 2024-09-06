package aviatickets.app.flight.dto.request;

import aviatickets.app.flight.entity.CabinClass;
import aviatickets.app.flight.entity.Location;

import java.time.LocalDateTime;

// GetTicketList -> can contain null fields if it shouldn't be filtered *
public record GetFilteredFlight(
		String departureAirport,
		String arrivalAirport,
		Location location,
		Short offset,
		LocalDateTime departureDate, // null or Date
		LocalDateTime returnDate, // null or Date
		Short passengerCount, // -> passenger * price
		CabinClass cabinClass, // as string "Economy", "Business", "First"
		FilterOptions filterOptions // as enum CHEAPEST, FASTEST, DIRECT
) {
}

// passenger -> age and price will be a details at order page *


// -> should be updated