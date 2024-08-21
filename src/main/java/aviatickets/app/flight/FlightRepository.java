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
import aviatickets.app.flight.entity.*;
import aviatickets.app.util.SerializationInterface;
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
	private final SerializationInterface serializationService;
	private final Logger log = LoggerFactory.getLogger(FlightRepository.class);

// ########################################################################################
// ################################# customer area  #######################################
// ########################################################################################

	@Override
	@Cacheable("hotFlights")
  public List<ShortFlightDto> getHotFlightsList(Short offset) throws SQLException, ClassNotFoundException {

//		struct temp {
//			String flightNum
//			int departureAirport
//			int arrivalAirport
//		}


//		List<temp> baseData = push here cur result set;
//		-> this.resultSet = new sql query execution;


		List<ShortFlightDto> flights = new ArrayList<>();
		String baseSql = "SELECT flights.flight_number, "
				+ "leg_details.departure_airport AS departure_airport, "
				+ "leg_details.arrival_airport AS arrival_airport, "
				+ "FROM flights "
				+ "JOIN leg_details "
				+ "ON flights.flight_number = leg_details.flight_number "
				+ "WHERE leg_details.departure_time = get_departure_time_filter() "
				+ "DESC "
				+ "LIMIT 10 "
				+ "OFFSET ?";

			String funcSql = "CALL get_short_flight_data("
				+ "?,?,?,?, @flight_id, @legs, @departure_airport, @arrival_airport "
				+ "@distance, @available_sits, @price)";

		try {

			this.initConnection((byte) 1);

			PreparedStatement baseStatement = this.connection.prepareStatement(baseSql);
			baseStatement.setInt(1, offset);

			this.resultSet = baseStatement.executeQuery();
			log.info("result set is \n->{}", this.resultSet.toString());

			PreparedStatement funcStatement = this.connection.prepareStatement(funcSql);

			while(this.resultSet.next()) {
//				String flightNum = this.resultSet.getString("flight_number");
//				int departureAirport = this.resultSet.getInt("departure_airport");
//				int arrivalAirport = this.resultSet.getInt("arrival_airport");



//				temp item = new temp(
//						this.resultSet.getInt("id"),
//						this.resultSet.getString("distance"),
//						this.resultSet.getString("flight_number"),
//						this.resultSet.getFloat("price")
//				);
//				baseData.add(temp);
//			}
//
//			for (int i = 0; i < baseData.size(); i++) {
//				funcStatement.setString(1, baseData.get(i).flightNum); ....
//				funcStatement.setBoolean(2, true);
//				funcStatement.setInt(3, departureAirport);
//				funcStatement.setInt(4, arrivalAirport);
//
//
//
//
//				ShortFlightDto item = new ShortFlightDto(
//						this.resultSet.getInt("id"),
//						this.resultSet.getString("distance"),
//						this.resultSet.getString("flight_number"),
//						this.resultSet.getFloat("price")
//				);
//				flights.add(item);
//
			}


//
//			this.resultSet = statement.executeQuery();
//			log.info("result set is \n->{}", this.resultSet.toString());
//
//			while(this.resultSet.next()) {
//				ShortFlightDto item = new ShortFlightDto(
//					this.resultSet.getInt("id"),
//					this.resultSet.getString("distance"),
//					this.resultSet.getString("flight_number"),
//					this.resultSet.getFloat("price")
//				);
//				flights.add(item);
//			}
			log.info("flight size is -> {}", flights.size());

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

		// ####################################### variables ##############################################

		boolean isProcedureExecute;
		boolean isDropped;

		int aircraftId = 0;
		int priceId = 0;
		int departureAirportId = 0;
		int arrivalAirportId = 0;

		FlightsItem flightItem = null;
		Leg legItem = null;
		List<Leg> legs = new ArrayList<>();

		Aircraft aircraft = null;
		CabinClass cabinClass = null;
		AircraftFeatures aircraftFeatures = null;

		Airport airport = null;
		AirportContacts contacts = null;
		Location location = null;

		// ################################## sql strings area ##############################################

		String getIds = "SELECT "
			+ "flights.aircraft_id, flights.price, "
			+ "leg_details.departure_airport, leg_details.arrival_airport "
			+	"FROM flights "
			+ "JOIN leg_details "
			+ "ON flights.flight_number=leg_details.flight_number "
			+ "WHERE flights.flight_number=?";

		String flightSql = "SELECT "
			+ "flight_number, airline, aircraft_id, departure_time, "
			+ "distance=(SELECT calculate_total_distance(?) AS distance), "
			+ "departure_time, distance, price, passenger_count, "
			+ "available_sits=(SELECT count_available_sits(?) AS available_sits) "
			+ "FROM flights "
			+ "WHERE flight_number = ?";

		String getCabinClass = "SELECT * from cabin_class WHERE aircraft_id=?";
		String getFeatures = "SELECT * from features WHERE aircraft_id=?";
		String getAircraft = "SELECT * from aircraft WHERE aircraft_id=?";

		String getContacts = "SELECT * from airport_contacts WHERE airport_id=?";
		String getLocation = "SELECT * from airport_location WHERE location_id=?";
		String getAirport = "SELECT * from airport WHERE airport_id=?";


		// ################################## the main logic ##############################################

		try {
			this.initConnection((byte) 0);
			String[] returnedIds = {"aircraft_id, price, departure_airport, arrival_airport"};

			PreparedStatement getIDsStatement = this.connection.prepareStatement(getIds, returnedIds);

			PreparedStatement getAircraftStatement = this.connection.prepareStatement(getAircraft);
			PreparedStatement getCabinClassStatement = this.connection.prepareStatement(getCabinClass);
			PreparedStatement getFeaturesStatement = this.connection.prepareStatement(getFeatures);

			PreparedStatement getAirportStatement = this.connection.prepareStatement(getAirport);
			PreparedStatement getLocationStatement = this.connection.prepareStatement(getLocation);
			PreparedStatement getContactsStatement = this.connection.prepareStatement(getContacts);

			PreparedStatement getFlightStatement = this.connection.prepareStatement(flightSql);




			// count_available_sits(flightNumber)
			// calculate_current_price(flightNumber)



			getIDsStatement.setString(1, flightNumber);
			this.resultSet = getIDsStatement.executeQuery();
			while (this.resultSet.next()) {
				aircraftId = this.resultSet.getInt("aircraft_id");
				priceId = this.resultSet.getInt("price");
				departureAirportId = this.resultSet.getInt("departure_airport");
				arrivalAirportId = this.resultSet.getInt("arrival_airport");
			}




//				flightItem = this.serializationService.getFlightItemEntityFromResultSet(
//						this.resultSet,
//						null,
//						legs
//				);

		} finally {
//			PreparedStatement dropStatement = this.connection.prepareStatement(dropSql);
//			isDropped = dropStatement.execute();
//			log.info("drop view exec is  -> {}", isDropped);
//			log.info("drop view count is  -> {}", dropStatement.getUpdateCount());
//
//			if (dropStatement.getUpdateCount() < 1) {
//				log.info("-----------> Failed to drop view <GET_DETAILED_FLIGHT>.");
//			}
			this.closeAndStopDBInteraction();
		}

		return flightItem;
	}

	@Override
	public FlightsItem getFlightDetails(Integer id) throws SQLException, ClassNotFoundException {
		return null;
	}


// ########################################################################################
// ################################## ADMIN area only #####################################
// ########################################################################################


	// createNewFlight -> add new flight and its details to db as ADMIN
	@Override
	public void createFlight(FlightsItem flight) throws RuntimeException, SQLException, ClassNotFoundException {

		log.info("flight item is -> {}", flight.toString());
		Leg legItem = new Leg();
		
		Integer flightId = this.getFlightId(flight.getFlightNumber());
		if(flightId != 0) {
			throw new BadRequestException("Flight already exists");
		}

		if(flight.getItinerary().size() == 1) {
			this.saveAirport(flight.getItinerary().getFirst().getDepartureAirport().getAirport());
			this.saveAirport(flight.getItinerary().getFirst().getArrivalAirport().getAirport());
			this.saveLegItem(flight.getItinerary().getFirst().getLegItem(), flight.getFlightNumber());
		} else {
			for (int i = 0; i < flight.getItinerary().size(); i++) {
				// run through the itinerary list to save each
				// leg and airport details
				legItem.setLeg(
						flight.getItinerary().get(i).getId(),
						flight.getItinerary().get(i).getLegNumber(),
						flight.getItinerary().get(i).getDepartureTime(),
						flight.getItinerary().get(i).getArrivalTime(),
						flight.getItinerary().get(i).getDuration(),
						flight.getItinerary().get(i).getDistance(),
						flight.getItinerary().get(i).getStatus()
				);
				legItem.setArrivalAirport(flight.getItinerary().get(i).getArrivalAirport().getAirport());
				legItem.setDepartureAirport(flight.getItinerary().get(i).getDepartureAirport().getAirport());
				
				log.info("Itinerary item:  {}", legItem);
				this.saveAirport(legItem.getDepartureAirport().getAirport());
				this.saveAirport(legItem.getArrivalAirport().getAirport());
				this.saveLegItem(legItem.getLegItem(), flight.getFlightNumber());
			}
		}
		this.saveFlight(flight);
	}


	@Override
	public void deleteFlight(Integer flightId, Integer customerId) throws SQLException, ClassNotFoundException {

		String sql = "CALL delete_flight(?,?)";

		try {
			this.initConnection((byte) 0);

			PreparedStatement statement = this.connection.prepareStatement(sql);
			statement.setInt(1, flightId);
			statement.setInt(2, customerId);

			statement.execute();
			log.info("updated count -> {}", statement.getUpdateCount());

			if (statement.getUpdateCount() < 1) {
				throw new SQLException("Failed to delete flight with id _" + flightId + "_.");
			}
		} finally {
			this.closeAndStopDBInteraction();
		}

	}

	@Override
	public void updateFlight(FlightsItem flight) throws SQLException, ClassNotFoundException {

		Integer isExist = this.getFlightId(flight.getFlightNumber());
		if(isExist == 0) {
			throw new BadRequestException("Flight not found.");
		}

		if(flight.getItinerary().size() == 1) {
			this.updateAirport(flight.getItinerary().getFirst().getDepartureAirport());
			this.updateAirport(flight.getItinerary().getFirst().getArrivalAirport());
			this.updateLegItem(flight.getItinerary().getFirst(), flight.getFlightNumber());
		} else {
			for (int i = 0; i < flight.getItinerary().size(); i++) {
				// run through the itinerary list to save each
				// leg and airport details
				Leg item = flight.getItinerary().get(i);
				log.info("Itinerary item is -> {}", item);
				this.updateAirport(item.getDepartureAirport());
				this.updateAirport(item.getArrivalAirport());
				this.updateLegItem(item, flight.getFlightNumber());
			}
		}
		this.updateFlightItem(flight);
	}

	// saveAircraft -> save new aircraft to db if it doesn't exist by:
	// -> check if aircraft exists by get an aircraft ID
	// -> if id != 0 --> continue, else --> stop
	// -> then create a new aircraft,
	// get an id again and validate it
	// -> save aircraft details data such as features and cabin class
	// -> check updated counter before the end
	private void saveAircraft(Aircraft aircraft) throws RuntimeException, SQLException, ClassNotFoundException {

		int updated = 0;
		int aircraftId = 0;
		String[] returnedId = {"id"};

		String airStr = "INSERT INTO aircraft (model, registration, seating_capacity, year_of_manufacture) VALUES (?,?,?,?)";
		String featuresStr = "INSERT INTO aircraft_features (wifi, entertainment, power_outlets, aircraft_id) VALUES (?,?,?,?)";
		String cabinStr = "INSERT INTO cabin_class (economy, business, first, aircraft_id) VALUES (?,?,?,?)";

		try {
			aircraftId = this.getAircraftId(aircraft.getRegistration());
			if(aircraftId != 0) {
				log.info("Flight already exists");
				return;
			}

			this.initConnection((byte) 0);

			PreparedStatement preparedAircraft = this.connection.prepareStatement(airStr, returnedId);
			PreparedStatement preparedFeatures = this.connection.prepareStatement(featuresStr);
			PreparedStatement preparedCabinClass = this.connection.prepareStatement(cabinStr);

			preparedAircraft.setString(1, aircraft.getModel());
			preparedAircraft.setString(2, aircraft.getRegistration());
			preparedAircraft.setShort(3, aircraft.getSeatingCapacity());
			preparedAircraft.setShort(4, aircraft.getYearOfManufacture());

			updated += preparedAircraft.executeUpdate();

			this.resultSet = preparedAircraft.getGeneratedKeys();
			while (this.resultSet.next()) {
				aircraftId = this.resultSet.getInt(1);
			}

			log.info("saved aircraft id is -> {}", aircraftId);

			preparedFeatures.setBoolean(1, aircraft.getFeatures().getWifi());
			preparedFeatures.setBoolean(2, aircraft.getFeatures().getInFlightEntertainment());
			preparedFeatures.setBoolean(3, aircraft.getFeatures().getPowerOutlets());
			preparedFeatures.setInt(4, aircraftId);

			preparedCabinClass.setBoolean(1, aircraft.getFeatures().getCabinClass().getEconomy());
			preparedCabinClass.setBoolean(2, aircraft.getFeatures().getCabinClass().getBusiness());
			preparedCabinClass.setBoolean(3, aircraft.getFeatures().getCabinClass().getFirst());
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
	// -> then create a new airport,
	// get an id again and validate it
	// -> save airport details data such as location and contacts
	// -> check updated counter before the end
	private void saveAirport(Airport airport) throws RuntimeException, SQLException, ClassNotFoundException {

		String[] returnedId = {"id"};
		int updated = 0;
		Integer airportId = 0;

		String airStr = "INSERT INTO airport (code, airport_name, city, country, terminal, timezone) VALUES (?,?,?,?,?,?)";
		String locationStr = "INSERT INTO airport_location (longitude, latitude, altitude, airport_id) VALUES (?,?,?,?)";
		String contactStr = "INSERT INTO airport_contacts (phone, email, website, airport_id) VALUES (?,?,?,?)";

		try {

			airportId = this.getAirportId(airport.getAirportName());
			if(airportId != 0) {
				System.out.println("Flight already exists");
				return;
			}

			this.initConnection((byte) 0);
			PreparedStatement preparedAirport = this.connection.prepareStatement(airStr, returnedId);
			PreparedStatement preparedLocation = this.connection.prepareStatement(locationStr);
			PreparedStatement preparedContacts = this.connection.prepareStatement(contactStr);

			preparedAirport.setString(1, airport.getCode());
			preparedAirport.setString(2, airport.getAirportName());
			preparedAirport.setString(3, airport.getCity());
			preparedAirport.setString(4, airport.getCountry());
			preparedAirport.setString(5, airport.getTerminal());
			preparedAirport.setString(6, airport.getTimezone());


			log.info("prep airport -> {}", preparedAirport);

			updated += preparedAirport.executeUpdate();
			this.resultSet = preparedAirport.getGeneratedKeys();
			while (this.resultSet.next()) {
				airportId = this.resultSet.getInt(1);
			}

			log.info("saved airport id is -> {}", airportId);

			preparedLocation.setString(1, airport.getLocation().getLongitude());
			preparedLocation.setString(2, airport.getLocation().getLatitude());
			preparedLocation.setString(3, airport.getLocation().getAltitude());
			preparedLocation.setInt(4, airportId);

			preparedContacts.setString(1, airport.getContacts().getPhone());
			preparedContacts.setString(2, airport.getContacts().getEmail());
			preparedContacts.setString(3, airport.getContacts().getWebsite());
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
			Integer arrivalAirportId = this.getAirportId(leg.getArrivalAirport().getAirportName());
			Integer departureAirportId = this.getAirportId(leg.getDepartureAirport().getAirportName());

			log.info("arrival arp -> {}, departure arp -> {}", arrivalAirportId, departureAirportId);

			this.initConnection((byte) 0);
			PreparedStatement preparedLeg = this.connection.prepareStatement(sql);
			preparedLeg.setString(1, flightNumber);
			preparedLeg.setInt(2, arrivalAirportId);
			preparedLeg.setInt(3, departureAirportId);
			preparedLeg.setDate(4, leg.getDepartureTime());
			preparedLeg.setDate(5, leg.getArrivalTime());
			preparedLeg.setString(6, leg.getDuration());
			preparedLeg.setShort(7, leg.getDistance());
			preparedLeg.setString(8, leg.getStatus());

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
				"(flight_number, airline, aircraft_id, departure_time, distance, total_duration, price, passenger_count, available_sits) " +
				"VALUES(?,?,?,?,?,?,?,?,?)";

		try {
			aircraftId = this.getAircraftId(flight.getAircraft().getRegistration());
			if(aircraftId != 0) {
				log.info("Aircraft already exists");
			} else {
				this.saveAircraft(flight.getAircraft());
				aircraftId = this.getAircraftId(flight.getAircraft().getRegistration());
				if(aircraftId == 0) {
					throw new ServerErrorException("Failed to save aircraft data.");
				}
			}

			log.info("aircraftId -> {}", aircraftId);

			this.initConnection((byte) 0);

			PreparedStatement preparedPrice = this.connection.prepareStatement(priceSql, returnedId);
			PreparedStatement preparedFlight = this.connection.prepareStatement(flightSql);

			preparedPrice.setString(1, flight.getFlightNumber());
			preparedPrice.setString(2, flight.getPrice().getCurrency());
			preparedPrice.setFloat(3, flight.getPrice().getAmount());
			preparedPrice.setInt(4, flight.getPrice().getDiscount());
			preparedPrice.setString(5, flight.getPrice().getBaggageAllowance());



			updated += preparedPrice.executeUpdate();

			this.resultSet = preparedPrice.getGeneratedKeys();
			while (this.resultSet.next()) {
				priceId = this.resultSet.getInt(1);
			}
			log.info("price id is -> {}", priceId);

			preparedFlight.setString(1, flight.getFlightNumber());
			preparedFlight.setString(2, flight.getAirline());
			preparedFlight.setInt(3, aircraftId);
			preparedFlight.setDate(4, flight.getItinerary().getFirst().departureTime());
			preparedFlight.setShort(5, flight.getTotalDistance());
			preparedFlight.setString(6, flight.getTotalDuration());
			preparedFlight.setInt(7, priceId);
			preparedFlight.setShort(8,flight.getPassengerCount());
			preparedFlight.setShort(9, flight.getAvailableSits());

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


	private void updateFlightItem(FlightsItem item) {

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
