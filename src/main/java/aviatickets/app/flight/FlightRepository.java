package aviatickets.app.flight;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.entity.Aircraft;
import aviatickets.app.flight.entity.Airport;
import aviatickets.app.flight.entity.Leg;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;
import org.jetbrains.annotations.NotNull;

import aviatickets.app.flight.entity.FlightsItem;
import org.springframework.util.Assert;

@Repository
public class FlightRepository {

  private final JdbcClient jdbcClient;

  public FlightRepository(JdbcClient jdbcClient) {
    this.jdbcClient = jdbcClient;
  }

  @Cacheable("hotFlight")
  public List<FlightsItem> getHotFlights() {
    String sqlStr = "SELECT * FROM flights WHERE departure_time=?"; // as example

    return jdbcClient.sql(sqlStr)
			.param(LocalDateTime.now())
			.query(FlightsItem.class)
			.list();
  }

  public List<FlightsItem> findFlightsByFilter(GetFilteredFlight filter) {

		// --> get flights list here ->
		// --> get sold ticket with the same flights numbers

		// then get free seats value by multiplying flight total seats on bought ticket to this flight
		// with the same flights number

		// -> then update flight.availableSeats = total / bought
		// -> return list to user

		System.out.println(filter);
    String sqlStr = "SELECT ... ";

    return jdbcClient.sql(sqlStr)
			.params()
			.query(FlightsItem.class)
			.list();
  }

	// --------------------------------------------
	// ------------ admin area only ---------------
	// --------------------------------------------

	public void createNewFlight(@NotNull FlightsItem flight) {

		String selectFlight = "SELECT id FROM flights WHERE flight_number=?";

		Optional<FlightsItem> storedFlight = jdbcClient
			.sql(selectFlight)
			.param(flight.flightNumber())
			.query(FlightsItem.class)
			.optional();

		// ?????????? -> should create db ready leg ojb ** 
		Leg tempLeg = new Leg(
				null,
				0,

		)

		Assert.state(storedFlight.isPresent(), "Flight already exists");

		if(flight.itinerary().size() == 1) {
			this.saveAirport(flight.itinerary().getLast().departureAirport());
			this.saveAirport(flight.itinerary().getFirst().arrivalAirport());
		} else {
			for (int i = 0; i <= flight.itinerary().size(); i++) {
				Integer depAirport = this.saveAirport(flight.itinerary().get(i).departureAirport());
				Integer ariAirport = this.saveAirport(flight.itinerary().get(i).arrivalAirport());

				this.saveLeg(flight.itinerary().get(i), [depAirport, ariAirport])
			}
		}


		// if aircraft.registration == flight.aircraft.registration -> continue without saving aircraft
		this.saveAircraft(flight.aircraft());

		// if flightNumber == flight.flightNumber -> continue without saving flight
		this.saveFlight(flight);

	}


	private void saveAircraft(@NotNull Aircraft aircraft) {

		Integer aircraftId = 0;
		String getAircraftIdStr = "SELECT id FROM aircraft WHERE registration=?";
		String airStr = "INSERT INTO aircraft VALUES (?,?,?,?)";
		String featuresStr = "INSERT INTO aircraft_features VALUES (?,?,?,?)";
		String cabinStr = "INSERT INTO cabin_class VALUES (?,?,?,?)";

		Optional<Aircraft> storedAircraft = jdbcClient
			.sql(getAircraftIdStr)
			.param(aircraft.registration())
			.query(Aircraft.class)
			.optional();

		Assert.state(storedAircraft.isPresent(), "Aircraft already exists");

		var updated = jdbcClient
			.sql(airStr)
			.params(List.of(
				aircraft.model(),
				aircraft.registration(),
				aircraft.seatingCapacity(),
				aircraft.yearOfManufacture()
			))
			.update();

		Optional<Aircraft> savedAircraft = jdbcClient
			.sql(getAircraftIdStr)
			.param(aircraft.registration())
			.query(Aircraft.class)
			.optional();

		if(savedAircraft.isPresent()) {
			aircraftId = savedAircraft.get().id();
		}

		Assert.state(aircraftId == 0, "Failed to get saved aircraft id.");

		updated += jdbcClient
			.sql(cabinStr)
			.params(List.of(
				aircraft.features().cabinClass().economy(),
				aircraft.features().cabinClass().business(),
				aircraft.features().cabinClass().first(),
				aircraftId
			))
			.update();

		updated += jdbcClient
			.sql(featuresStr)
			.params(List.of(
				aircraft.features().wifi(),
				aircraft.features().inFlightEntertainment(),
				aircraft.features().powerOutlets(),
				aircraftId
			))
			.update();


		System.out.println("updated count -> " + updated);
		Assert.state(updated != 3, "Failed to create new aircraft.");
	}

	private Integer saveAirport(@NotNull Airport airport) {

		Integer airportId = 0;
		String getAirportIdStr = "SELECT id FROM airport WHERE name=?";
		String airStr = "INSERT INTO airport VALUES (?,?,?,?,?)";
		String locationStr = "INSERT INTO airport_location VALUES (?,?,?,?)";
		String contactStr = "INSERT INTO airport_contact VALUES (?,?,?,?)";

		Optional<Airport> storedAirport = jdbcClient
			.sql(getAirportIdStr)
			.param(airport.airportName())
			.query(Airport.class)
			.optional();

		Assert.state(storedAirport.isPresent(), "Airport already exists");

		var updated = jdbcClient
			.sql(airStr)
			.params(List.of(
				airport.code(),
					airport.airportName(),
					airport.city(),
					airport.country(),
					airport.terminal(),
					airport.timezone()
			))
			.update();

		Optional<Airport> savedAirport = jdbcClient
			.sql(getAirportIdStr)
			.param(airport.airportName())
			.query(Airport.class)
			.optional();

		if(savedAirport.isPresent()) {
			airportId = savedAirport.get().id();
		}

		Assert.state(airportId == 0, "Failed to get saved airport id.");


		updated += jdbcClient
			.sql(locationStr)
			.params(List.of(
				airport.location().longitude(),
				airport.location().latitude(),
				airport.location().altitude(),
				airportId
			))
			.update();


		updated += jdbcClient
			.sql(contactStr)
			.params(List.of(
				airport.contacts().phone(),
				airport.contacts().email(),
				airport.contacts().website(),
				airportId
			))
			.update();


		System.out.println("updated count -> " + updated);
		Assert.state(updated != 3, "Failed to create new aircraft.");

		return airportId;
	}

	private void saveLeg(Leg leg, Integer[] ids) {
		String sqlStr = "INSERT INTO leg_details VALUES (?,?,?,?,?,?,?)";

		var updated = jdbcClient
			.sql(sqlStr)
			.params(List.of(

			))
			.update();

	}

	private void saveFlight(FlightsItem flight) {

	}

}
