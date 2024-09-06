package aviatickets.app.flight;

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

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

@RequiredArgsConstructor
@Repository
class FlightRepository extends AbstractFlight implements FlightInterface {

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
  public List<ShortFlightDto> getHotFlightsList() throws SQLException, ClassNotFoundException {

		String flightNumber;
		String duration;
		Date departureDate;
		Float price = 0.0F;
		String airportName = null;
		String city = null;
		String code = null;

		ShortFlightDto dtoItem = new ShortFlightDto();
		List<ShortFlightDto> hotFlights = new ArrayList<>();

		String getArrivalAirportDataSql = "SELECT code, airport_name, city "
				+ "FROM airport "
				+ "WHERE id=("
					+ "SELECT arrival_airport "
					+ "FROM leg_details "
					+ "WHERE flight_number=?"
					+ "AND leg_number=("
						+ "SELECT MAX(leg_number) "
						+ "FROM leg_details "
						+ "WHERE flight_number=?))";

		String getDepartureAirportDataSql = "SELECT code, airport_name, city "
				+ "FROM airport "
				+ "WHERE id=("
					+ "SELECT arrival_airport "
					+ "FROM leg_details "
					+ "WHERE flight_number=?"
					+ "AND leg_number=("
						+ "SELECT MIN(leg_number) "
						+ "FROM leg_details "
						+ "WHERE flight_number=?))";


		String baseSql = "SELECT flight_number, total_duration, departure_time "
				+ "FROM flights "
				+ "WHERE departure_time "
				+ "BETWEEN CURRENT_TIMESTAMP() AND NOW() + INTERVAL 1 DAY "
				+ "ORDER BY departure_time DESC "
				+ "LIMIT 20 ";

		String getPriceSql = "SELECT calculate_current_price(?) AS price";

		try {
			this.initConnection((byte) 1);

			PreparedStatement baseStatement = this.connection.prepareStatement(baseSql);
			PreparedStatement getDepartureAirportDataStatement = this.connection.prepareStatement(getDepartureAirportDataSql);
			PreparedStatement getArrivalAirportDataStatement = this.connection.prepareStatement(getArrivalAirportDataSql);

			PreparedStatement getPriceStatement = this.connection.prepareStatement(getPriceSql);

			this.resultSet = baseStatement.executeQuery();
			while (this.resultSet.next()) {
				flightNumber = this.resultSet.getString("flight_number");
				duration = this.resultSet.getString("total_duration");
				departureDate = this.resultSet.getDate("departure_time");

				dtoItem.setFlightNumber(flightNumber);
				dtoItem.setDuration(duration);
				dtoItem.setDepartureDate(departureDate);

				hotFlights.add(dtoItem.getShortFlightDto());
			}
			baseStatement.close();

			for (ShortFlightDto item : hotFlights) {

				flightNumber = item.getFlightNumber();

				getPriceStatement.setString(1, flightNumber);
				this.resultSet = getPriceStatement.executeQuery();
				while (this.resultSet.next()) {
					price = this.resultSet.getFloat("price");
				}
				getPriceStatement.close();

				item.setPrice(price);

				getDepartureAirportDataStatement.setString(1, flightNumber);
				this.resultSet = getDepartureAirportDataStatement.executeQuery();
				while (this.resultSet.next()) {
					airportName = this.resultSet.getString("airport_name");
					city = this.resultSet.getString("city");
					code = this.resultSet.getString("code");
				}
				getDepartureAirportDataStatement.close();

				item.setDepartureAirportCity(city);
				item.setDepartureAirportName(airportName);
				item.setDepartureAirportCode(code);

				getArrivalAirportDataStatement.setString(1, flightNumber);
				this.resultSet = getArrivalAirportDataStatement.executeQuery();
				while (this.resultSet.next()) {
					airportName = this.resultSet.getString("airport_name");
					city = this.resultSet.getString("city");
					code = this.resultSet.getString("code");
				}
				getArrivalAirportDataStatement.close();

				item.setArrivalAirportCity(city);
				item.setArrivalAirportName(airportName);
				item.setArrivalAirportCode(code);

				log.info("current dto state is => {}", item);
			}


		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
		return hotFlights;
  }

	@Override
	@Cacheable("filteredFlights")
	public List<ShortFlightDto> findFlightsByFilter(GetFilteredFlight filter) throws SQLException, ClassNotFoundException {

		List<ShortFlightDto> flights = new ArrayList<>();
		String sql = "";

		try {
			this.initConnection((byte) 1);


		}	catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}

    return flights;
  }

	@Override
	public FlightsItem getFlightDetails(String flightNumber) throws SQLException, ClassNotFoundException {

		int aircraftId = 0;
		int departureAirportId = 0;
		int arrivalAirportId = 0;
		short availableSits = 0;

		FlightsItem flightItem = new FlightsItem();
		Leg legItem;
		List<Leg> legs = new ArrayList<>();

		Airport departureAirport;
		Airport arrivalAirport;

		Aircraft aircraft;
		Price price;

		String flightSql = "SELECT * FROM flights WHERE flight_number=?";
		String getLegsList = "SELECT * FROM leg_details WHERE flight_number=? ORDER BY leg_number";
		String getAvailableSits = "SELECT count_available_sits(?) AS available_sits";

		try {
			aircraftId = this.getAircraftIdToDetailedMethod(flightNumber);
			aircraft = this.getAircraft(aircraftId);
			price = this.getPrice(flightNumber);

			this.initConnection((byte) 0);

			PreparedStatement getLegsListStatement = this.connection.prepareStatement(getLegsList);
			PreparedStatement getAvailableSitsStatement = this.connection.prepareStatement(getAvailableSits);
			PreparedStatement getFlightStatement = this.connection.prepareStatement(flightSql);

			// ################################# perform queries ############################################

			getLegsListStatement.setString(1, flightNumber);
			this.resultSet = getLegsListStatement.executeQuery();
			while (this.resultSet.next()) {

				departureAirportId = this.resultSet.getInt("departure_airport");
				arrivalAirportId = this.resultSet.getInt("arrival_airport");

				departureAirport = this.getAirport(departureAirportId);
				arrivalAirport = this.getAirport(arrivalAirportId);

				legItem = this.serializationService.getLegEntityFromResultSet(
					this.resultSet,
					departureAirport,
					arrivalAirport
				);

				legs.add(legItem);
			}

			getFlightStatement.setString(1, flightNumber);
			this.resultSet = getFlightStatement.executeQuery();
			while (this.resultSet.next()) {
				flightItem = this.serializationService.getFlightItemEntityFromResultSet(
					this.resultSet,
					aircraft,
					legs,
					price
				);
			}

			getAvailableSitsStatement.setString(1, flightNumber);
			this.resultSet = getAvailableSitsStatement.executeQuery();
			while (this.resultSet.next()) {
				availableSits = this.resultSet.getShort("available_sits");
				flightItem.setAvailableSits(availableSits);
			}

		} finally {
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
//
//		Integer isExist = this.getFlightId(flight.getFlightNumber());
//		if(isExist == 0) {
//			throw new BadRequestException("Flight not found.");
//		}
//
//		if(flight.getItinerary().size() == 1) {
//			this.updateAirport(flight.getItinerary().getFirst().getDepartureAirport());
//			this.updateAirport(flight.getItinerary().getFirst().getArrivalAirport());
//			this.updateLegItem(flight.getItinerary().getFirst(), flight.getFlightNumber());
//		} else {
//			for (int i = 0; i < flight.getItinerary().size(); i++) {
//				// run through the itinerary list to save each
//				// leg and airport details
//				Leg item = flight.getItinerary().get(i);
//				log.info("Itinerary item is -> {}", item);
//				this.updateAirport(item.getDepartureAirport());
//				this.updateAirport(item.getArrivalAirport());
//				this.updateLegItem(item, flight.getFlightNumber());
//			}
//		}
//		this.updateFlightItem(flight);
	}

	// saveAircraft -> save new aircraft to db if it doesn't exist by:
	// -> check if aircraft exists by get an aircraft ID
	// -> if id != 0 --> continue, else --> stop
	// -> then create a new aircraft,
	// get an id again and validate it
	// -> save aircraft details data such as features and cabin class
	// -> check updated counter before the end
	protected void saveAircraft(Aircraft aircraft) throws RuntimeException, SQLException, ClassNotFoundException {

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
	protected void saveAirport(Airport airport) throws RuntimeException, SQLException, ClassNotFoundException {

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
	protected void saveLegItem(Leg leg, String flightNumber) throws SQLException, ClassNotFoundException {

		String sql = "INSERT INTO leg_details " +
				"(leg_number, flight_number, departure_airport, arrival_airport, departure_time, arrival_time, duration, distance, status) " +
				"VALUES (?,?,?,?,?,?,?,?,?)";

		try {
			Integer arrivalAirportId = this.getAirportId(leg.getArrivalAirport().getAirportName());
			Integer departureAirportId = this.getAirportId(leg.getDepartureAirport().getAirportName());

			log.info("arrival arp -> {}, departure arp -> {}", arrivalAirportId, departureAirportId);

			this.initConnection((byte) 0);
			PreparedStatement preparedLeg = this.connection.prepareStatement(sql);

			preparedLeg.setShort(1, leg.getLegNumber());
			preparedLeg.setString(2, flightNumber);
			preparedLeg.setInt(3, departureAirportId);
			preparedLeg.setInt(4, arrivalAirportId);
			preparedLeg.setDate(5, leg.getDepartureTime());
			preparedLeg.setDate(6, leg.getArrivalTime());
			preparedLeg.setString(7, leg.getDuration());
			preparedLeg.setShort(8, leg.getDistance());
			preparedLeg.setString(9, leg.getStatus());

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
	protected void saveFlight(FlightsItem flight) throws SQLException, ClassNotFoundException {

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
			preparedFlight.setDate(4, flight.getItinerary().getFirst().getDepartureTime());
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

// ########################################################################################
// ############################## get item id by filter ###################################
// ########################################################################################

protected Integer getAirportId(String airportName) throws SQLException, ClassNotFoundException {
		String sql = "SELECT id FROM airport WHERE airport_name=?";
		return this.getItemId(sql, airportName);
	}

protected Integer getAircraftId(String registrationNumber) throws SQLException, ClassNotFoundException {
		String sql = "SELECT id FROM aircraft WHERE registration=?";
		return this.getItemId(sql, registrationNumber);
	}

protected Integer getAircraftIdToDetailedMethod(String flightNumber) throws SQLException, ClassNotFoundException {
		String sql = "SELECT id FROM flights WHERE flight_number=?";
		return this.getItemId(sql, flightNumber);
	}

protected Integer getFlightId(String flightNumber) throws SQLException, ClassNotFoundException {
		String sql = "SELECT id FROM flights WHERE flight_number=?";
		return this.getItemId(sql, flightNumber);
	}

protected Airport getAirport(Integer id) throws SQLException {

		ResultSet tempRs;
		Airport airport = new Airport();
		AirportContacts airportContacts = new AirportContacts();
		Location location = new Location();

		String getContacts = "SELECT * from airport_contacts WHERE airport_id=?";
		String getLocation = "SELECT * from airport_location WHERE airport_id=?";
		String getAirport = "SELECT * from airport WHERE id=?";

		PreparedStatement getAirportStatement = this.connection.prepareStatement(getAirport);
		PreparedStatement getLocationStatement = this.connection.prepareStatement(getLocation);
		PreparedStatement getContactsStatement = this.connection.prepareStatement(getContacts);


		getContactsStatement.setInt(1, id);
		tempRs = getContactsStatement.executeQuery();
		while (tempRs.next()) {
			airportContacts = this.serializationService.getAirportContactsEntityFromResultSet(tempRs);
		}

		getLocationStatement.setInt(1, id);
		tempRs = getLocationStatement.executeQuery();
		while (tempRs.next()) {
			location = this.serializationService.getLocationEntityFromResultSet(tempRs);
		}

		getAirportStatement.setInt(1, id);
		tempRs = getAirportStatement.executeQuery();
		while (tempRs.next()) {
			airport = this.serializationService.getAirportEntityFromResultSet(tempRs, airportContacts, location);
		}

		return airport;
	}

protected Aircraft getAircraft(Integer id) throws SQLException, ClassNotFoundException {

		Aircraft aircraft = new Aircraft();
		CabinClass cabinClass = new CabinClass();
		AircraftFeatures aircraftFeatures = new AircraftFeatures();

		String getCabinClass = "SELECT * from cabin_class WHERE aircraft_id=?";
		String getFeatures = "SELECT * from aircraft_features WHERE aircraft_id=?";
		String getAircraft = "SELECT * from aircraft WHERE id=?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement getAircraftStatement = this.connection.prepareStatement(getAircraft);
			PreparedStatement getCabinClassStatement = this.connection.prepareStatement(getCabinClass);
			PreparedStatement getFeaturesStatement = this.connection.prepareStatement(getFeatures);

			getCabinClassStatement.setInt(1, id);
			this.resultSet = getCabinClassStatement.executeQuery();
			while (this.resultSet.next()) {
				cabinClass = this.serializationService.getCabinClassEntityFromResultSet(this.resultSet);
			}

			getFeaturesStatement.setInt(1, id);
			this.resultSet = getFeaturesStatement.executeQuery();
			while (this.resultSet.next()) {
				aircraftFeatures = this.serializationService.
						getAircraftFeaturesEntityFromResultSet(this.resultSet, cabinClass);
			}

			getAircraftStatement.setInt(1, id);
			this.resultSet = getAircraftStatement.executeQuery();
			while (this.resultSet.next()) {
				aircraft = this.serializationService.getAircraftEntityFromResultSet(this.resultSet, aircraftFeatures);
			}

		} finally {
			this.closeAndStopDBInteraction();
		}

		return aircraft;
	}

protected Price getPrice(String flightNumber) throws SQLException, ClassNotFoundException {

		Price price = new Price();
		String getPrice = "SELECT * FROM price_details WHERE flight_number=?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement getPriceStatement = this.connection.prepareStatement(getPrice);

			getPriceStatement.setString(1, flightNumber);
			this.resultSet = getPriceStatement.executeQuery();
			while (this.resultSet.next()) {
				price = this.serializationService.getPriceEntityFromResultSet(this.resultSet);
			}

		} finally {
			this.closeAndStopDBInteraction();
		}

		return price;
	}

// getItemId -> get item id from db (airport, aircraft, flight) or 0 if it doesn't exists
protected Integer getItemId(String sql, String filter) throws SQLException, ClassNotFoundException {
	int itemId = 0;
	String[] returnedId = {"id"};

	try {
		this.initConnection((byte) 1);

		PreparedStatement statement = this.connection.prepareStatement(sql, returnedId);
		statement.setString(1, filter);

		this.resultSet = statement.executeQuery();
		while (this.resultSet.next()) {
			itemId = this.resultSet.getInt("id");
		}

		log.info("returned value from -getItemId- func -> {}", itemId);
	} catch (Exception e) {
		throw e;
	} finally {
		this.closeAndStopDBInteraction();
	}
	return itemId;
}

protected void updateFlightItem(FlightsItem item) {

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
