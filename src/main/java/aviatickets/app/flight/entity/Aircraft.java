package aviatickets.app.flight.entity;

import jakarta.validation.constraints.Positive;

public record Aircraft(
		@Positive
		Integer id,
		String model,
		String registration,
		@Positive
		Short seatingCapacity,
		@Positive
		Short yearOfManufacture,
		AircraftFeatures features) {
}
