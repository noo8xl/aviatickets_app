package aviatickets.app.flight;

import java.util.List;

import aviatickets.app.flight.dto.request.GetFiltredFlight;
import aviatickets.app.flight.entity.FlightsItem;

public interface FlightInteraction {

  // getHotFlightList -> get list of cheapest today flights
  public List<FlightsItem> getHotFlightList();

  // findFlightByFilter -> get list of flights by filter
  public List<FlightsItem> findFlightByFilter(GetFiltredFlight filter);

}
