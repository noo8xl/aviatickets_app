package aviatickets.app.flight.entity;


import jakarta.validation.constraints.Positive;

public record AircraftFeatures(
		Boolean wifi,
		Boolean inFlightEntertainment,
		Boolean powerOutlets,
		CabinClass cabinClass // "Economy", "Business", "First"
) {

}
