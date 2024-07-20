package aviatickets.app.flight;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.exception.BadRequestException;
import aviatickets.app.flight.dto.request.GetFilteredFlight;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

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
  @GetMapping("/get-hot/{offset}/")
  List<FlightsItem> getHotList(@PathVariable Short offset) {
		System.out.println("test routes");
    try {
      List<FlightsItem> flights = flightService.getHotFlightList(offset);
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

	// #############################################################################
	// ########################## admin handlers only ##############################
	// #############################################################################

  @ResponseStatus(HttpStatus.CREATED)
  @PostMapping("/create-new-flight/")
  void createNewFlight(@RequestBody FlightsItem flight) throws BadRequestException, SQLException, ClassNotFoundException {
		System.out.println(flight);
		flightService.createFlight(flight);
	}


}
