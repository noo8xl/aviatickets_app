package aviatickets.app.ticket.entity.flight;

import java.time.LocalDateTime;

import jakarta.validation.constraints.Positive;

public record Arrival(
    String airport,
    String timezone,
    String iata,
    String icao,
    Character terminal,
    String gate,
    @Positive String baggage,
    Integer delay,
    LocalDateTime scheduled,
    LocalDateTime estimated,
    LocalDateTime actual,
    LocalDateTime estimatedRunway,
    LocalDateTime actualRunway) {

}
