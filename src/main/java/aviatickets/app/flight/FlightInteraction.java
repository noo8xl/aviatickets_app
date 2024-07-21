package aviatickets.app.flight;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightItem;
import aviatickets.app.flight.entity.FlightsItem;

public interface FlightInteraction {

  // getHotFlightList -> get list of cheapest today flights
  List<ShortFlightItem> getHotFlightList(Short offset) throws SQLException, ClassNotFoundException;

  // findFlightByFilter -> get list of flights by filter
  List<ShortFlightItem> findFlightByFilter(GetFilteredFlight filter) throws SQLException, ClassNotFoundException;

	void deleteFlight(FlightsItem flight);

	void updateFlight(FlightsItem flight);

	void createFlight(FlightsItem flight) throws Exception;

}
