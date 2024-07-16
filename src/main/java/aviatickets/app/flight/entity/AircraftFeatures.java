package aviatickets.app.flight.entity;


public record AircraftFeatures(
		Boolean wifi,
		Boolean inFlightEntertaiment,
		Boolean powerOutlets,
		CabinClass cabinClass // "Economy", "Business", "First"
) {

}
