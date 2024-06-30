package aviatickets.app.ticket.entity.flight;

import java.time.LocalDateTime;

public record Departure(
    String airport,
    String timezone,
    String iata,
    String icao,
    Integer terminal,
    String gate,
    Integer delay,
    LocalDateTime scheduled,
    LocalDateTime estimated,
    LocalDateTime actual,
    LocalDateTime estimatedRunway,
    LocalDateTime actualRunway) {
}
