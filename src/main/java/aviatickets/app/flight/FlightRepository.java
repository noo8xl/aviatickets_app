package aviatickets.app.flight;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

import aviatickets.app.database.DatabaseInterface;
import aviatickets.app.database.dto.DBConnectionDto;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.exception.ServerErrorException;
import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightDto;
import aviatickets.app.flight.entity.Aircraft;
import aviatickets.app.flight.entity.Airport;
import aviatickets.app.flight.entity.FlightsItem;
import aviatickets.app.flight.entity.Leg;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Repository;


@RequiredArgsConstructor
@Repository
class FlightRepository implements FlightInterface {

	private Connection connection = null;
	private Statement statement = null;
	private ResultSet resultSet = null;

	private final DatabaseInterface database;
	private final Logger log = LoggerFactory.getLogger(FlightRepository.class);



// ########################################################################################
// ################################# customer area  #######################################
// ########################################################################################

	@Override
	@Cacheable("hotFlights")
  public List<ShortFlightDto> getHotFlightsList(Short offset) throws SQLException, ClassNotFoundException {

		List<ShortFlightDto> flights = new ArrayList<>();
		String sql = "SELECT * FROM SHORT_FLIGHT_DATA " +
				"WHERE leg_details.departure_time " +
				"BETWEEN CURRENT_TIMESTAMP() " +
				"AND get_departure_time_filter() " +
				"LIMIT 10 " +
				"OFFSET ?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedFlight = this.connection.prepareStatement(sql);
			preparedFlight.setShort(1, offset);

			this.resultSet = preparedFlight.executeQuery();
//			log.info("result set is \n->" + resultSet);

			while(this.resultSet.next()) {
				ShortFlightDto item = new ShortFlightDto(
					this.resultSet.getInt("id"),
					this.resultSet.getString("flight_number"),
					this.resultSet.getString("total_duration"),
					this.resultSet.getFloat("price")
				);
				flights.add(item);
			}
//			log.info(flights.size());

		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
		return flights;
  }

	@Override
	@Cacheable("filteredFlights")
	public List<ShortFlightDto> findFlightsByFilter(GetFilteredFlight filter) throws SQLException, ClassNotFoundException {

		List<ShortFlightDto> flights = new ArrayList<>();
		String sql = "";
		System.out.println(filter);

		// select * from leg_details where flight_number=3; ---<

		// --> get flights list here ->
		// --> get sold ticket with the same flights numbers

		// then get free seats value by multiplying flight total seats on bought ticket to this flight
		// with the same flights number

		// -> then update flight.availableSeats = total / bought
		// -> return list to user
		try {
			this.initConnection((byte) 1);

			// check filtered fields for not null value **
			// -> set filter ..

			PreparedStatement preparedFlight = this.connection.prepareStatement(sql);
//			preparedFlight.setShort(1, offset);

			this.resultSet = preparedFlight.executeQuery();

			while (this.resultSet.next()) {
				ShortFlightDto item = new ShortFlightDto(
					this.resultSet.getInt("id"),
					this.resultSet.getString("flight_number"),
					this.resultSet.getString("total_duration"),
					this.resultSet.getFloat("price")
				);
				flights.add(item);
			}

		}	catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}

    return flights;
  }

	@Override
	public FlightsItem getFlightDetails(String flightNumber) throws SQLException, ClassNotFoundException {
		return null;
	}

	@Override
	public FlightsItem getFlightDetails(Integer id) throws SQLException, ClassNotFoundException {
		return null;
	}


// ########################################################################################
// ################################## admin area only #####################################
// ########################################################################################


	// createNewFlight -> add new flight and its details to db as admin
	@Override
	public void createFlight(FlightsItem flight) throws RuntimeException, SQLException, ClassNotFoundException {


		log.info("flight item is -> {}", flight.toString());

		Integer flightId = this.getFlightId(flight.getFlightNumber());
		if(flightId != 0) {
			throw new BadRequestException("Flight already exists");
		}

		if(flight.getItinerary().size() == 1) {
			this.saveAirport(flight.getItinerary().getFirst().departureAirport());
			this.saveAirport(flight.getItinerary().getFirst().arrivalAirport());
			this.saveLegItem(flight.getItinerary().getFirst(), flight.getFlightNumber());
		} else {
			for (int i = 0; i < flight.getItinerary().size(); i++) {
				// run through the itinerary list to save each
				// leg and airport details
				Leg item = flight.getItinerary().get(i);
				log.info("Itinerary item:  {}", item);
				this.saveAirport(item.departureAirport());
				this.saveAirport(item.arrivalAirport());
				this.saveLegItem(item, flight.getFlightNumber());
			}
		}
		this.saveFlight(flight);
	}


	@Override
	public void deleteFlight(Integer id) throws SQLException, ClassNotFoundException {

	}

	@Override
	public void updateFlight(FlightsItem flight) throws SQLException, ClassNotFoundException {

	}

	// saveAircraft -> save new aircraft to db if it doesn't exist by:
	// -> check if aircraft exists by get an aircraft ID
	// -> if id != 0 --> continue, else --> stop
	// -> then create a new aircraft
	// get an id again and validate it
	// -> save aircraft details data such as features and cabin class
	// -> check updated counter before end
	private void saveAircraft(Aircraft aircraft) throws RuntimeException, SQLException, ClassNotFoundException {

		int updated = 0;
		int aircraftId = 0;
		String[] returnedId = {"id"};

		String airStr = "INSERT INTO aircraft (model, registration, seating_capacity, year_of_manufacture) VALUES (?,?,?,?)";
		String featuresStr = "INSERT INTO aircraft_features (wifi, entertainment, power_outlets, aircraft_id) VALUES (?,?,?,?)";
		String cabinStr = "INSERT INTO cabin_class (economy, business, first, aircraft_id) VALUES (?,?,?,?)";

		try {
			aircraftId = this.getAircraftId(aircraft.registration());
			if(aircraftId != 0) {
				log.info("Flight already exists");
				return;
			}

			this.initConnection((byte) 0);

			PreparedStatement preparedAircraft = this.connection.prepareStatement(airStr, returnedId);
			PreparedStatement preparedFeatures = this.connection.prepareStatement(featuresStr);
			PreparedStatement preparedCabinClass = this.connection.prepareStatement(cabinStr);

			preparedAircraft.setString(1, aircraft.model());
			preparedAircraft.setString(2, aircraft.registration());
			preparedAircraft.setShort(3, aircraft.seatingCapacity());
			preparedAircraft.setShort(4, aircraft.yearOfManufacture());

			updated += preparedAircraft.executeUpdate();

			this.resultSet = preparedAircraft.getGeneratedKeys();
			while (this.resultSet.next()) {
				aircraftId = this.resultSet.getInt(1);
			}

			log.info("saved aircraft id is -> {}", aircraftId);

			preparedFeatures.setBoolean(1, aircraft.features().wifi());
			preparedFeatures.setBoolean(2, aircraft.features().inFlightEntertainment());
			preparedFeatures.setBoolean(3, aircraft.features().powerOutlets());
			preparedFeatures.setInt(4, aircraftId);

			preparedCabinClass.setBoolean(1, aircraft.features().cabinClass().economy());
			preparedCabinClass.setBoolean(2, aircraft.features().cabinClass().business());
			preparedCabinClass.setBoolean(3, aircraft.features().cabinClass().first());
			preparedCabinClass.setInt(4, aircraftId);

			updated += preparedFeatures.executeUpdate();
			updated += preparedCabinClass.executeUpdate();

			if(updated != 3) {
				throw new ServerErrorException("Failed to create new aircraft.");
			}

		}	catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		};
	}


	// saveAirport -> save new airport to db if it doesn't exist by:
	// -> check if airport exists by get an airport ID
	// -> if id != 0 --> continue, else --> stop
	// -> then create a new airport
	// get an id again and validate it
	// -> save airport details data such as location and contacts
	// -> check updated counter before end
	private void saveAirport(Airport airport) throws RuntimeException, SQLException, ClassNotFoundException {

		String[] returnedId = {"id"};
		int updated = 0;
		Integer airportId = 0;

		String airStr = "INSERT INTO airport (code, airport_name, city, country, terminal, timezone) VALUES (?,?,?,?,?,?)";
		String locationStr = "INSERT INTO airport_location (longitude, latitude, altitude, airport_id) VALUES (?,?,?,?)";
		String contactStr = "INSERT INTO airport_contacts (phone, email, website, airport_id) VALUES (?,?,?,?)";

		try {

			airportId = this.getAirportId(airport.airportName());
			if(airportId != 0) {
				System.out.println("Flight already exists");
				return;
			}

			this.initConnection((byte) 0);
			PreparedStatement preparedAirport = this.connection.prepareStatement(airStr, returnedId);
			PreparedStatement preparedLocation = this.connection.prepareStatement(locationStr);
			PreparedStatement preparedContacts = this.connection.prepareStatement(contactStr);

			preparedAirport.setString(1, airport.code());
			preparedAirport.setString(2, airport.airportName());
			preparedAirport.setString(3, airport.city());
			preparedAirport.setString(4, airport.country());
			preparedAirport.setString(5, airport.terminal());
			preparedAirport.setString(6, airport.timezone());


			log.info("prep airport -> {}", preparedAirport);

			updated += preparedAirport.executeUpdate();
			this.resultSet = preparedAirport.getGeneratedKeys();
			while (this.resultSet.next()) {
				airportId = this.resultSet.getInt(1);
			}

			log.info("saved airport id is -> {}", airportId);

			preparedLocation.setString(1, airport.location().longitude());
			preparedLocation.setString(2, airport.location().latitude());
			preparedLocation.setString(3, airport.location().altitude());
			preparedLocation.setInt(4, airportId);

			preparedContacts.setString(1, airport.contacts().phone());
			preparedContacts.setString(2, airport.contacts().email());
			preparedContacts.setString(3, airport.contacts().website());
			preparedContacts.setInt(4, airportId);

			updated += preparedLocation.executeUpdate();
			updated += preparedContacts.executeUpdate();

			if(updated != 3) {
				throw new ServerErrorException("Failed to create new airport.");
			}
		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
	}

	// saveLegItem -> call it to save each leg in flight
	private void saveLegItem(Leg leg, String flightNumber) throws SQLException, ClassNotFoundException {

		String sql = "INSERT INTO leg_details " +
				"(flight_number, departure_airport, arrival_airport, departure_time, arrival_time, duration, distance, status) " +
				"VALUES (?,?,?,?,?,?,?,?)";

		try {
			Integer arrivalAirportId = this.getAirportId(leg.arrivalAirport().airportName());
			Integer departureAirportId = this.getAirportId(leg.departureAirport().airportName());

			log.info("arrival arp -> {}, departure arp -> {}", arrivalAirportId, departureAirportId);

			this.initConnection((byte) 0);
			PreparedStatement preparedLeg = this.connection.prepareStatement(sql);
			preparedLeg.setString(1, flightNumber);
			preparedLeg.setInt(2, arrivalAirportId);
			preparedLeg.setInt(3, departureAirportId);
			preparedLeg.setDate(4, leg.departureTime());
			preparedLeg.setDate(5, leg.arrivalTime());
			preparedLeg.setString(6, leg.duration());
			preparedLeg.setInt(7, leg.distance());
			preparedLeg.setString(8, leg.status());

			int updated = preparedLeg.executeUpdate();
			if(updated < 1) {
				throw new ServerErrorException("Failed to create new leg.");
			}
		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
	}

	// saveFlight -> save flight obj
	// if flightNumber == flight.flightNumber -> continue without saving flight
	private void saveFlight(FlightsItem flight) throws SQLException, ClassNotFoundException {

		String[] returnedId = {"id"};
		Integer aircraftId;
		int priceId = 0;
		int updated = 0;

		String priceSql = "INSERT INTO price_details (flight_number, currency, amount, discount, baggage) VALUES (?,?,?,?,?)";
		String flightSql = "INSERT INTO flights " +
				"(flight_number, airline, aircraft_id, distance, total_duration, price, passenger_count, available_sits) " +
				"VALUES(?,?,?,?,?,?,?,?)";

		try {
			aircraftId = this.getAircraftId(flight.getAircraft().registration());
			if(aircraftId != 0) {
				log.info("Aircraft already exists");
			} else {
				this.saveAircraft(flight.getAircraft());
				aircraftId = this.getAircraftId(flight.getAircraft().registration());
				if(aircraftId == 0) {
					throw new ServerErrorException("Failed to save aircraft data.");
				}
			}

			log.info("aircraftId -> {}", aircraftId);

			this.initConnection((byte) 0);

			PreparedStatement preparedPrice = this.connection.prepareStatement(priceSql, returnedId);
			PreparedStatement preparedFlight = this.connection.prepareStatement(flightSql);

			preparedPrice.setString(1, flight.getFlightNumber());
			preparedPrice.setString(2, flight.getPrice().currency());
			preparedPrice.setFloat(3, flight.getPrice().amount());
			preparedPrice.setInt(4, flight.getPrice().discount());
			preparedPrice.setString(5, flight.getPrice().baggageAllowance());



			updated += preparedPrice.executeUpdate();

			this.resultSet = preparedPrice.getGeneratedKeys();
			while (this.resultSet.next()) {
				priceId = this.resultSet.getInt(1);
			}
			log.info("price id is -> {}", priceId);

			preparedFlight.setString(1, flight.getFlightNumber());
			preparedFlight.setString(2, flight.getAirline());
			preparedFlight.setInt(3, aircraftId);
			preparedFlight.setShort(4, flight.getTotalDistance());
			preparedFlight.setString(5, flight.getTotalDuration());
			preparedFlight.setInt(6, priceId);
			preparedFlight.setShort(7,flight.getPassengerCount());
			preparedFlight.setShort(8, flight.getAvailableSits());

			updated += preparedFlight.executeUpdate();
			if(updated < 2) {
				throw new ServerErrorException("Failed to create new flight.");
			}

		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
	}

// ########################################################################################
// ############################## get item id by filter ###################################
// ########################################################################################

	private Integer getAirportId(String airportName) throws SQLException, ClassNotFoundException {
		String sql = "SELECT id FROM airport WHERE airport_name=?";
		return this.getItemId(sql, airportName);
	}

	private Integer getAircraftId(String registrationNumber) throws SQLException, ClassNotFoundException {
		String sql = "SELECT id FROM aircraft WHERE registration=?";
		return this.getItemId(sql, registrationNumber);
	}

	private Integer getFlightId(String flightNumber) throws SQLException, ClassNotFoundException {
		String sql = "SELECT id FROM flights WHERE flight_number=?";
		return this.getItemId(sql, flightNumber);
	}

//	// getLegId -> return leg id by flight number
//	// if it exists and return 0 if not
//	private Integer getPriceId(String flightNumber) throws SQLException, ClassNotFoundException {
//		String sql = "SELECT id FROM price_details WHERE flight_number=?";
//		return this.getItemId(sql, flightNumber);
//	}

// getItemId -> get item id from db (airport, aircraft, flight) or 0 if it doesn't exists
private Integer getItemId(String sql, String filter) throws SQLException, ClassNotFoundException {
	int itemId = 0;
	String[] returnedId = {"id"};

	try {
		this.initConnection((byte) 1);

		PreparedStatement statement = this.connection.prepareStatement(sql, returnedId);
		statement.setString(1, filter);

		this.resultSet = statement.executeQuery();
		while(this.resultSet.next()) {
			itemId = this.resultSet.getInt("id");
		}

		log.info("returned value from -getItemId- func -> {}", itemId);
	} catch (Exception e ) {
		throw e;
	} finally {
		this.closeAndStopDBInteraction();
	}
	return itemId;
}

// ########################################################################################
// ########################## end of get item id by filter ################################
// ########################################################################################


// ########################################################################################
// ############################# database interaction area ################################
// ########################################################################################

	private void initConnection(Byte type) throws ClassNotFoundException, SQLException {
		DBConnectionDto dto = this.database.initConnection(type);
		this.connection = dto.connection();
		this.statement = dto.statement();
		this.resultSet = dto.resultSet();
	}

	private void closeAndStopDBInteraction() throws SQLException, ClassNotFoundException {
		DBConnectionDto dto = new DBConnectionDto(this.connection, this.statement, this.resultSet);
		this.database.closeAndStopDBInteraction(dto);
	}

}
