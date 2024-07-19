package aviatickets.app.flight.entity;

import jakarta.validation.constraints.Positive;

import java.sql.Date;

public record Leg(
		@Positive
		Integer id,
		@Positive
		Short legNumber,
		Airport departureAirport,
		Airport arrivalAirport,
		Date departureTime,
		Date arrivalTime,
		String duration,
		@Positive
		Integer distance,
		String status

) {}