package aviatickets.app.flight;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightDto;
import aviatickets.app.flight.entity.FlightsItem;

public interface FlightInterface {

  // getHotFlightList -> get a list of nearest today flights
  List<ShortFlightDto> getHotFlightsList() throws SQLException, ClassNotFoundException;

  // findFlightByFilter -> get list of flights by filter
  List<ShortFlightDto> findFlightsByFilter(GetFilteredFlight filter) throws SQLException, ClassNotFoundException;

	// getFlightDetails -> get flight detailed data
	FlightsItem getFlightDetails(String flightNumber) throws SQLException, ClassNotFoundException;
	FlightsItem getFlightDetails(Integer id) throws SQLException, ClassNotFoundException;

// ##########################################################################################################
// ##################################### ADMIN permission only ##############################################
// ##########################################################################################################

	void deleteFlight(Integer flightId, Integer customerId) throws SQLException, ClassNotFoundException;

	void updateFlight(FlightsItem flight) throws SQLException, ClassNotFoundException;

	void createFlight(FlightsItem flight) throws SQLException, ClassNotFoundException;

}
