package aviatickets.app.util.entity;

import aviatickets.app.flight.entity.FlightsItem;

import java.util.List;

public record Flights(List<FlightsItem> flights) {
}
