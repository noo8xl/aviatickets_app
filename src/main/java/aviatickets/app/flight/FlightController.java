package aviatickets.app.flight;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.exception.BadRequestException;
import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightDto;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import aviatickets.app.flight.entity.FlightsItem;

@RequiredArgsConstructor
@RestController
@RequestMapping("/flights")
public class FlightController {

  public final FlightInterface flightService;

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get-hot/{offset}/")
	ResponseEntity<List<ShortFlightDto>> getHotList(@PathVariable Short offset) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.flightService.getHotFlightsList(offset));
  }

  @ResponseStatus(HttpStatus.OK)
  @PostMapping("/find-filtered-flight/")
	ResponseEntity<List<ShortFlightDto>> findFlight(@Valid @RequestBody GetFilteredFlight filter) throws SQLException, ClassNotFoundException {
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
	// ########################## ADMIN handlers only ##############################
	// #############################################################################

  @ResponseStatus(HttpStatus.CREATED)
  @PostMapping("/create-new-flight/")
  void createNewFlight(@Valid @RequestBody FlightsItem flight) throws BadRequestException, SQLException, ClassNotFoundException {
//		System.out.println("flight ->" + flight);
		this.flightService.createFlight(flight);
	}

	@ResponseStatus(HttpStatus.ACCEPTED)
	@PutMapping("/update-flight/{id}/")
	void updateFlightById(
			@PathVariable Integer id,
			@Valid @RequestBody FlightsItem flight
	) throws SQLException, ClassNotFoundException {
		this.flightService.updateFlight(flight);
	}

	@ResponseStatus(HttpStatus.ACCEPTED)
	@PatchMapping("/delete-flight/{flightId}/{customerId}/")
	void deleteFlightById(
			@PathVariable Integer flightId, @PathVariable Integer customerId
	) throws SQLException, ClassNotFoundException {
		this.flightService.deleteFlight(flightId, customerId);
	}


}
