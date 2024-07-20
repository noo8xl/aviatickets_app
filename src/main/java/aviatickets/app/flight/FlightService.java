package aviatickets.app.flight;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.flight.dto.request.GetFilteredFlight;
import org.springframework.stereotype.Service;

import aviatickets.app.flight.entity.FlightsItem;

@Service
public class FlightService implements FlightInteraction {

  private final FlightRepository flightRepository;

  public FlightService(FlightRepository flightRepository) {
    this.flightRepository = flightRepository;
  }

  public List<FlightsItem> getHotFlightList(Short offset) throws SQLException {
    return flightRepository.getHotFlights(offset);
  }

  public List<FlightsItem> findFlightByFilter(GetFilteredFlight filter) {

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
