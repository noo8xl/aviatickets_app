package aviatickets.app.flight;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightItemDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import aviatickets.app.flight.entity.FlightsItem;

@RequiredArgsConstructor
@Service
public class FlightService implements FlightInterface {

  private final FlightInterface flightRepository;

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
		return this.flightRepository.getFlightDetails(flightNumber);
	}

	@Override
	public FlightsItem getFlightDetails(Integer id) throws SQLException, ClassNotFoundException {
		return this.flightRepository.getFlightDetails(id);
	}


	// ########################################################################################################

	@Override
	public void createFlight(FlightsItem flight) throws RuntimeException, SQLException, ClassNotFoundException {
		this.flightRepository.createFlight(flight);
	}

	@Override
	public void deleteFlight(Integer id) throws SQLException, ClassNotFoundException {
		this.flightRepository.deleteFlight(id);
	}

	@Override
	public void updateFlight(FlightsItem flight) throws SQLException, ClassNotFoundException {
		this.flightRepository.updateFlight(flight);
	}


}
