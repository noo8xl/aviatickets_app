package aviatickets.app.flight.entity;

import java.time.LocalDateTime;

public record Leg(
		Short leg,
		Airport departureAirport,
		Airport arrivalAirport,
		LocalDateTime departureTime,
		LocalDateTime arrivalTime,
		Integer distance,
		FlightStatus status) {

}
