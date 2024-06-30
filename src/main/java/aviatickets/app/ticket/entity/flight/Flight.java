package aviatickets.app.ticket.entity.flight;

public record Flight(
    Integer number,
    String iata,
    String icao,
    String icao24) {
}
