package aviatickets.app.flight.entity;

public record Price(
    String currency,
    Float amount,
		Short discount,
    String baggageAllowance
) {
}
