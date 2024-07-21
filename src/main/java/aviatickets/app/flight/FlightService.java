package aviatickets.app.flight;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightItem;
import org.springframework.stereotype.Service;

import aviatickets.app.flight.entity.FlightsItem;

@Service
public class FlightService implements FlightInteraction {

  private final FlightRepository flightRepository;

  public FlightService(FlightRepository flightRepository) {
    this.flightRepository = flightRepository;
  }

  public List<ShortFlightItem> getHotFlightList(Short offset) throws SQLException, ClassNotFoundException {
    return flightRepository.getHotFlights(offset);
  }

  public List<ShortFlightItem> findFlightByFilter(GetFilteredFlight filter) throws SQLException, ClassNotFoundException {

    // LocalDateTime flightDate,
    // String departureAirport,
    // String arrivalAirport,
    // LocalDateTime departureDate, // null or Date
    // LocalDateTime returnDate, // null or Date
    // Short passengerCount, // -> passenger * price
    // String cabinClass, // as string "Economy", "Business", "First"
    // FilterOptions filterOptions // as enum SLOWEST, CHEAPEST, FASTEST, DIRECT

    return flightRepository.findFlightsByFilter(filter);
  }

	public FlightsItem getFlightDetails(String flightNumber) throws SQLException, ClassNotFoundException {

		return null;
	}


	@Override
	public void createFlight(FlightsItem flight) throws RuntimeException, SQLException, ClassNotFoundException {
		flightRepository.createNewFlight(flight);
	}

	@Override
	public void deleteFlight(FlightsItem flight) {

	}

	@Override
	public void updateFlight(FlightsItem flight) {

	}


}
