package aviatickets.app.flight.entity;

public record Aitcraft(
		String model,
		String registration,
		Short seatingCapacity,
		Short yearOfManufacture,
		AircraftFeatures features) {
}
