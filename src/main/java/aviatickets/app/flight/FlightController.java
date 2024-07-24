package aviatickets.app.flight;

import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.List;

import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightItem;
import jakarta.validation.Valid;
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
  List<ShortFlightItem> getHotList(@PathVariable Short offset) {
		System.out.println("test routes");
    try {
      List<ShortFlightItem> flights = flightService.getHotFlightList(offset);
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
	List<ShortFlightItem> findFlight(@Valid @RequestBody GetFilteredFlight filter) {
    try {
      List<ShortFlightItem> flights = flightService.findFlightByFilter(filter);
      if (flights.isEmpty()) {
        throw new NotFoundException("Can't find any data.");
      }
      return flights;
    } catch (Exception e) {
      throw new ServerErrorException();
    }
  }

	// findFlight -> find flights by request body filter, and return list of it
	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/find-filtered-flight/{flightNumber}")
	FlightsItem findFlight(@PathVariable String flightNumber ) {
		try {
			return flightService.getFlightDetails(flightNumber);
		} catch (Exception e) {
			throw new ServerErrorException();
		}
	}


	// #############################################################################
	// ########################## admin handlers only ##############################
	// #############################################################################

  @ResponseStatus(HttpStatus.CREATED)
  @PostMapping("/create-new-flight/")
  void createNewFlight(@Valid @RequestBody FlightsItem flight) throws BadRequestException, SQLException, ClassNotFoundException {
		System.out.println(flight);
		flightService.createFlight(flight);
	}


}
