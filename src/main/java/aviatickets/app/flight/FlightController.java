package aviatickets.app.flight;

import aviatickets.app.exception.BadRequestException;
import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightDto;
import aviatickets.app.flight.entity.FlightsItem;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.sql.SQLException;
import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping("/flights")
public class FlightController {

  public final FlightInterface flightService;

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get-hot/")
	public ResponseEntity<List<ShortFlightDto>> getHotList() throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.flightService.getHotFlightsList());
  }

  @ResponseStatus(HttpStatus.OK)
  @PostMapping("/find-filtered-flight/")
	@Cacheable(value = "hotFlights", key = "#filter.toString()")
	public ResponseEntity<List<ShortFlightDto>> findFlight(@Valid @RequestBody GetFilteredFlight filter) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.flightService.findFlightsByFilter(filter));
  }

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/find-filtered-flight/{flightNumber}/")
	public ResponseEntity<FlightsItem> findFlightByNumber(@PathVariable String flightNumber ) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.flightService.getFlightDetails(flightNumber));
	}

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/find-filtered-flight/{id}/")
	public ResponseEntity<FlightsItem> findFlightById(@PathVariable Integer id ) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.flightService.getFlightDetails(id));
	}

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-flights-details/{flightNumber}/")
	@Cacheable(value = "flightDetails", key = "#flightNumber")
	public FlightsItem getFlightDetails(@PathVariable String flightNumber) throws SQLException, ClassNotFoundException {
		return this.flightService.getFlightDetails(flightNumber);
	}

	// #############################################################################
	// ########################## ADMIN handlers only ##############################
	// #############################################################################

  @ResponseStatus(HttpStatus.CREATED)
  @PostMapping("/create-new-flight/")
	public void createNewFlight(@Valid @RequestBody FlightsItem flight) throws BadRequestException, SQLException, ClassNotFoundException {
		this.flightService.createFlight(flight);
	}

	@ResponseStatus(HttpStatus.ACCEPTED)
	@PutMapping("/update-flight/")
	public void updateFlightById(@Valid @RequestBody FlightsItem flight) throws SQLException, ClassNotFoundException {
		this.flightService.updateFlight(flight);
	}

	@ResponseStatus(HttpStatus.ACCEPTED)
	@DeleteMapping("/delete-flight/{flightId}/{customerId}/")
	public void deleteFlightById(
			@PathVariable Integer flightId,
			@PathVariable Integer customerId
	) throws SQLException, ClassNotFoundException {
		this.flightService.deleteFlight(flightId, customerId);
	}

}
