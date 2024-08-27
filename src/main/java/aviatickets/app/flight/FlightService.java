package aviatickets.app.flight;

import java.util.List;

import aviatickets.app.exception.ServerErrorException;
import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import aviatickets.app.flight.entity.FlightsItem;

@RequiredArgsConstructor
@Service
public class FlightService implements FlightInterface {

  private final FlightInterface flightRepository;

	@Override
  public List<ShortFlightDto> getHotFlightsList() {
		try {
			return this.flightRepository.getHotFlightsList();
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
  }

	@Override
  public List<ShortFlightDto> findFlightsByFilter(GetFilteredFlight filter) {
		try {
			return this.flightRepository.findFlightsByFilter(filter);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
  }

	@Override
	public FlightsItem getFlightDetails(String flightNumber) {
		try {
			return this.flightRepository.getFlightDetails(flightNumber);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
	public FlightsItem getFlightDetails(Integer id) {
		try {
			return this.flightRepository.getFlightDetails(id);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}


	// ########################################################################################################

	@Override
	public void createFlight(FlightsItem flight) {
		try {
			this.flightRepository.createFlight(flight);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
	public void deleteFlight(Integer flightId, Integer customerId) {
		try {
			this.flightRepository.deleteFlight(flightId, customerId);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
	public void updateFlight(FlightsItem flight) {
		try {
			this.flightRepository.updateFlight(flight);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}


}
