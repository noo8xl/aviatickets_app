package aviatickets.app.flight.entity;

import java.time.LocalDateTime;

public record Leg(
		Short leg,
		Airport departureAirport,
		Airport arrivalAirport,
		LocalDateTime departureTime,
		LocalDateTime arrivalTime,
		String status // scheduled, active, landed, cancelled, incident, diverted
) {

}
