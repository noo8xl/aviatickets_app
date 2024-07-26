package aviatickets.app.flight;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightItemDto;
import org.springframework.stereotype.Service;

import aviatickets.app.flight.entity.FlightsItem;

@Service
public class FlightService implements FlightInteraction {

  private final FlightRepository flightRepository;

  public FlightService(FlightRepository flightRepository) {
    this.flightRepository = flightRepository;
  }

	@Override
  public List<ShortFlightItemDto> getHotFlightList(Short offset) throws SQLException, ClassNotFoundException {
    return flightRepository.getHotFlights(offset);
  }

	@Override
  public List<ShortFlightItemDto> findFlightByFilter(GetFilteredFlight filter) throws SQLException, ClassNotFoundException {
    return flightRepository.findFlightsByFilter(filter);
  }

	@Override
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
