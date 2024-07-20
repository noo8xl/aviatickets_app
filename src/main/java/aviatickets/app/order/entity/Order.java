package aviatickets.app.order.entity;

import aviatickets.app.flight.entity.FlightsItem;

public record Order(
	Integer id,
	FlightsItem flight
	// some other details ..

	) {
}
