package aviatickets.app.flight.entity;

import java.sql.Date;

public record Leg(
		Integer id,
		Short legNumber,
		Airport departureAirport,
		Airport arrivalAirport,
		Date departureTime,
		Date arrivalTime,
		String duration,
		Integer distance,
		String status) {

}


//                "departureTime": "2024-07-16T09:00:00Z",
//										"arrivalTime": "2024-07-16T10:00:00Z",
//										"duration": "1h 00m",
//										"distance": 2000,
//										"status": "On Time"
