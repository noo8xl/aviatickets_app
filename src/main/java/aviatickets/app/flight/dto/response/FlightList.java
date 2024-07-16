package aviatickets.app.flight.dto.response;

import java.util.List;

import aviatickets.app.flight.entity.FlightsItem;
import jakarta.validation.constraints.NotEmpty;

public record FlightList(
    @NotEmpty List<FlightsItem> flightList) {
}
