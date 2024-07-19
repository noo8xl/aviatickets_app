package aviatickets.app.flight.entity;

import jakarta.validation.constraints.Positive;

public record Price(
    String currency,
		@Positive
    Float amount,
		@Positive
		Short discount,
    String baggageAllowance
) {
}
