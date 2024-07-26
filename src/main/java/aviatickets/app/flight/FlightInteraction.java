package aviatickets.app.flight;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightItemDto;
import aviatickets.app.flight.entity.FlightsItem;

interface FlightInteraction {

  // getHotFlightList -> get list of cheapest today flights
  List<ShortFlightItemDto> getHotFlightList(Short offset) throws SQLException, ClassNotFoundException;

  // findFlightByFilter -> get list of flights by filter
  List<ShortFlightItemDto> findFlightByFilter(GetFilteredFlight filter) throws SQLException, ClassNotFoundException;


	// getFlightDetails -> get flight detailed data (FULL_FLIGHT_INFO)
	FlightsItem getFlightDetails(String flightNumber) throws SQLException, ClassNotFoundException;

	void deleteFlight(FlightsItem flight);

	void updateFlight(FlightsItem flight);

	void createFlight(FlightsItem flight) throws Exception;

}
