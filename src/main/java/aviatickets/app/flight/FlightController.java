package aviatickets.app.flight;

import org.springframework.cache.annotation.Cacheable;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import aviatickets.app.exception.NotFoundException;
import aviatickets.app.exception.ServerErrorException;
import aviatickets.app.flight.dto.response.FlightList;

@RestController
@RequestMapping("/flight")
public class FlightController {

  public final FlightService flightService;

  public FlightController(FlightService flightService) {
    this.flightService = flightService;
  }

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get-hot/")
  @Cacheable
  FlightList getHotList() {
    try {
      FlightList flights = flightService.getHotFlightList();
      if (flights.isEmpty()) {
        throw new NotFoundException("Empty set.");
      }
      return flights;
    } catch (Exception e) {
      throw new ServerErrorException();
    }
  }

  @ResponseStatus(HttpStatus.OK)
  @PostMapping("/find-flight/")
  FlightList findFlight() {
    try {
      FlightList flight = flightService.findFlightByFilter();
      if (flight.isEmpty()) {
        throw new NotFoundException("Can't find any data.");
      }
      return flight;
    } catch (Exception e) {
      throw new ServerErrorException();
    }
  }

}
