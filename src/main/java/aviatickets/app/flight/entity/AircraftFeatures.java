package aviatickets.app.flight.entity;


public record AircraftFeatures(
		Boolean wifi,
		Boolean inFlightEntertainment,
		Boolean powerOutlets,
		CabinClass cabinClass // "Economy", "Business", "First"
) {

}
