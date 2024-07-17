package aviatickets.app.flight;

import java.util.List;

import aviatickets.app.flight.dto.request.GetFilteredFlight;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import aviatickets.app.exception.NotFoundException;
import aviatickets.app.exception.ServerErrorException;
import aviatickets.app.flight.entity.FlightsItem;

@RestController
@RequestMapping("/flights")
public class FlightController {

  public final FlightService flightService;

  public FlightController(FlightService flightService) {
    this.flightService = flightService;
  }

  // getHotList -> get a list of a cheapest flights of the day by request
  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get-hot/")
  List<FlightsItem> getHotList() {
    try {
      List<FlightsItem> flights = flightService.getHotFlightList();
      if (flights.isEmpty()) {
        throw new NotFoundException("Empty set.");
      }
      return flights;
    } catch (Exception e) {
      throw new ServerErrorException();
    }
  }

  // findFlight -> find flights by request body filter, and return list of it
  @ResponseStatus(HttpStatus.OK)
  @PostMapping("/find-filtered-flight/")
  List<FlightsItem> findFlight(@RequestBody GetFilteredFlight filter) {
    try {
      List<FlightsItem> flights = flightService.findFlightByFilter(filter);
      if (flights.isEmpty()) {
        throw new NotFoundException("Can't find any data.");
      }
      return flights;
    } catch (Exception e) {
      throw new ServerErrorException();
    }
  }

//	admin handlers only *

  @ResponseStatus(HttpStatus.OK)
  @PostMapping("/create-new-flight/")
  void createNewFlight(@RequestBody FlightsItem flight) {
		System.out.println(flight);
		flightService.createFlight(flight);
	}


}
