package aviatickets.app.flight.entity;

import java.util.List;

public record AircraftFeatures(
		Boolean wifi,
		Boolean inFlightEntertaiment,
		Boolean powerOutlets,
		List<String> cabinClass // "Economy", "Business", "First"
) {

}
