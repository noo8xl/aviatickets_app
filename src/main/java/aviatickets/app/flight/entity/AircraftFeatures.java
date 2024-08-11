package aviatickets.app.flight.entity;

public record AircraftFeatures(
		Integer id,
		Boolean wifi,
		Boolean inFlightEntertainment,
		Boolean powerOutlets,
		CabinClass cabinClass // "Economy", "Business", "First"
) {}
