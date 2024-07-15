package aviatickets.app.flight.entity;

public record Price(
    String currency,
    Float amount,
    String baggageAllowance) {
}
