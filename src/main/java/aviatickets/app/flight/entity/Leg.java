package aviatickets.app.flight.entity;

import java.time.LocalDateTime;

public record Leg(
		Integer id,
		Short leg,
		Airport departureAirport,
		Airport arrivalAirport,
		LocalDateTime departureTime,
		LocalDateTime arrivalTime,
		Integer distance,
		String status) {

}


//                "departureTime": "2024-07-16T09:00:00Z",
//										"arrivalTime": "2024-07-16T10:00:00Z",
//										"duration": "1h 00m",
//										"distance": 2000,
//										"status": "On Time"
