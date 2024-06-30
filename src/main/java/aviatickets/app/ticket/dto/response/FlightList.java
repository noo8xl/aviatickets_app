package aviatickets.app.ticket.dto.response;

import java.util.List;

import aviatickets.app.ticket.entity.flight.FlightsItem;
import jakarta.validation.constraints.NotEmpty;

public record FlightList(
    @NotEmpty List<FlightsItem> flightList) {
}

// [
// {
// "flightNumber": "AA123",
// "airline": "American Airlines",
// "departureAirport": "JFK",
// "arrivalAirport": "LAX",
// "departureTime": "2024-07-01T08:00:00",
// "arrivalTime": "2024-07-01T11:00:00",
// "duration": "6h",
// "price": 350.00,
// "status": "On Time"
// },
// {
// "flightNumber": "DL456",
// "airline": "Delta Airlines",
// "departureAirport": "ATL",
// "arrivalAirport": "ORD",
// "departureTime": "2024-07-01T09:30:00",
// "arrivalTime": "2024-07-01T11:00:00",
// "duration": "2h 30m",
// "price": 200.00,
// "status": "Delayed"
// },
// {
// "flightNumber": "UA789",
// "airline": "United Airlines",
// "departureAirport": "SFO",
// "arrivalAirport": "SEA",
// "departureTime": "2024-07-01T07:45:00",
// "arrivalTime": "2024-07-01T09:45:00",
// "duration": "2h",
// "price": 150.00,
// "status": "Cancelled"
// },
// {
// "flightNumber": "SW234",
// "airline": "Southwest Airlines",
// "departureAirport": "LAS",
// "arrivalAirport": "DEN",
// "departureTime": "2024-07-01T12:00:00",
// "arrivalTime": "2024-07-01T14:30:00",
// "duration": "2h 30m",
// "price": 180.00,
// "status": "On Time"
// },
// {
// "flightNumber": "BA987",
// "airline": "British Airways",
// "departureAirport": "LHR",
// "arrivalAirport": "JFK",
// "departureTime": "2024-07-01T10:00:00",
// "arrivalTime": "2024-07-01T13:00:00",
// "duration": "8h",
// "price": 500.00,
// "status": "On Time"
// },
// {
// "flightNumber": "LH345",
// "airline": "Lufthansa",
// "departureAirport": "FRA",
// "arrivalAirport": "DXB",
// "departureTime": "2024-07-01T14:00:00",
// "arrivalTime": "2024-07-01T22:00:00",
// "duration": "6h",
// "price": 450.00,
// "status": "On Time"
// }
// ]
