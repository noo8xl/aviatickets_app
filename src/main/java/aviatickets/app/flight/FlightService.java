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
  public List<ShortFlightItemDto> getHotFlightsList(Short offset) throws SQLException, ClassNotFoundException {
    return this.flightRepository.getHotFlightsList(offset);
  }

	@Override
  public List<ShortFlightItemDto> findFlightsByFilter(GetFilteredFlight filter) throws SQLException, ClassNotFoundException {
    return this.flightRepository.findFlightsByFilter(filter);
  }

	@Override
	public FlightsItem getFlightDetails(String flightNumber) throws SQLException, ClassNotFoundException {

		return null;
	}

	@Override
	public FlightsItem getFlightDetails(Integer id) throws SQLException, ClassNotFoundException {
		return null;
	}


	// ########################################################################################################

	@Override
	public void createFlight(FlightsItem flight) throws RuntimeException, SQLException, ClassNotFoundException {
		this.flightRepository.createFlight(flight);
	}

	@Override
	public void deleteFlight(Integer id) {

	}

	@Override
	public void updateFlight(FlightsItem flight) {

	}


}
