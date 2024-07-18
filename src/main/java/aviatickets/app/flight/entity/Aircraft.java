package aviatickets.app.flight.entity;

public record Aircraft(
		Integer id,
		String model,
		String registration,
		Short seatingCapacity,
		Short yearOfManufacture,
		AircraftFeatures features) {
}
