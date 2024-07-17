package aviatickets.app.flight.entity;

public record Aircraft(
		String model,
		String registration,
		Short seatingCapacity,
		Short yearOfManufacture,
		AircraftFeatures features) {
}
