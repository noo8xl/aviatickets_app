package aviatickets.app.flight;

import java.util.List;

import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.entity.FlightsItem;

public interface FlightInteraction {

  // getHotFlightList -> get list of cheapest today flights
  List<FlightsItem> getHotFlightList();

  // findFlightByFilter -> get list of flights by filter
  List<FlightsItem> findFlightByFilter(GetFilteredFlight filter);

	void deleteFlight(FlightsItem flight);

	void updateFlight(FlightsItem flight);

	void createFlight(FlightsItem flight);

}
