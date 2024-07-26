package aviatickets.app.flight.entity;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

public record Aircraft(
		@Positive
		Integer id,
		@NotBlank
		String model,
		@NotEmpty
		String registration,
		@Positive
		Short seatingCapacity,
		@Positive
		Short yearOfManufacture,
		AircraftFeatures features) {
}
