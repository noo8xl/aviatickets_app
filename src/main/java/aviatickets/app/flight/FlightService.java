package aviatickets.app.flight;

import java.util.List;

import org.springframework.stereotype.Service;

import aviatickets.app.flight.dto.request.GetFiltredFlight;
import aviatickets.app.flight.entity.FlightsItem;

@Service
public class FlightService implements FlightInteraction {

  private final FlightRepository flightRepository;

  public FlightService(FlightRepository flightRepository) {
    this.flightRepository = flightRepository;
  }

  public List<FlightsItem> getHotFlightList() {
    return flightRepository.getHotFlights();
  }

  public List<FlightsItem> findFlightByFilter(GetFiltredFlight filter) {

    // LocalDateTime flightDate,
    // String departureAirport,
    // String arrivalAirport,
    // LocalDateTime departureDate, // null or Date
    // LocalDateTime returnDate, // null or Date
    // Short passengerCount, // -> passenger * price
    // String cabinClass, // as string "Economy", "Business", "First"
    // FilterOptions filterOptions // as enum SLOWEST, CHEAPEST, FASTEST, DIRECT
    
    return null;
  }

}
