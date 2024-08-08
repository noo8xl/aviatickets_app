package aviatickets.app.flight;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.exception.BadRequestException;
import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightItemDto;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get-hot/{offset}/")
	ResponseEntity<List<ShortFlightItemDto>> getHotList(@PathVariable Short offset) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.flightService.getHotFlightsList(offset));
  }

  @ResponseStatus(HttpStatus.OK)
  @PostMapping("/find-filtered-flight/")
	ResponseEntity<List<ShortFlightItemDto>> findFlight(@Valid @RequestBody GetFilteredFlight filter) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.flightService.findFlightsByFilter(filter));
  }

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/find-filtered-flight/{flightNumber}/")
	ResponseEntity<FlightsItem> findFlightByNumber(@PathVariable String flightNumber ) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.flightService.getFlightDetails(flightNumber));
	}

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/find-filtered-flight/{id}/")
	ResponseEntity<FlightsItem> findFlightById(@PathVariable Integer id ) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.flightService.getFlightDetails(id));
	}



	// #############################################################################
	// ########################## admin handlers only ##############################
	// #############################################################################

  @ResponseStatus(HttpStatus.CREATED)
  @PostMapping("/create-new-flight/")
  void createNewFlight(@Valid @RequestBody FlightsItem flight) throws BadRequestException, SQLException, ClassNotFoundException {
		System.out.println(flight);
		this.flightService.createFlight(flight);
	}

	@ResponseStatus(HttpStatus.ACCEPTED)
	@PatchMapping("/update-flight/{id}/")
	void updateFlightById(
			@PathVariable Integer id, @Valid @RequestBody FlightsItem flight
	) throws SQLException, ClassNotFoundException {
		this.flightService.updateFlight(flight);
	}

	@ResponseStatus(HttpStatus.ACCEPTED)
	@PatchMapping("/delete-flight/{id}/")
	void updateFlightById(@PathVariable Integer id) throws SQLException, ClassNotFoundException {
		this.flightService.deleteFlight(id);
	}


}
