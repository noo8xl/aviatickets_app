package aviatickets.app.flight;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

import aviatickets.app.databaseInit.DatabaseInit;
import aviatickets.app.databaseInit.dto.DatabaseDto;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.exception.ServerErrorException;
import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.dto.response.ShortFlightItem;
import aviatickets.app.flight.entity.Aircraft;
import aviatickets.app.flight.entity.Airport;
import aviatickets.app.flight.entity.FlightsItem;
import aviatickets.app.flight.entity.Leg;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Repository;

@Repository
class FlightRepository {

	@Value("${spring.datasource.url}")
	private String dbUrl;

	@Value("${spring.datasource.username}")
	private String dbUsername;

	@Value("${spring.datasource.password}")
	private String dbPassword;

	private Connection connection = null;
	private Statement statement = null;
	private ResultSet resultSet = null;

	private DatabaseInit databaseInit;

  FlightRepository(DatabaseInit databaseInit) {
		this.databaseInit = databaseInit;
	}

// ########################################################################################
// ################################# customer area  #######################################
// ########################################################################################


	@Cacheable("hotFlights")
  public List<ShortFlightItem> getHotFlights(Short offset) throws SQLException, ClassNotFoundException {

		List<ShortFlightItem> flights = new ArrayList<>();
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
			System.out.println("result set is \n->" + resultSet);

			while(this.resultSet.next()) {
				ShortFlightItem item = new ShortFlightItem(
					this.resultSet.getInt("id"),
					this.resultSet.getString("flight_number"),
					this.resultSet.getString("total_duration"),
					this.resultSet.getFloat("price")
				);
				flights.add(item);
			}
			System.out.println(flights.size());

		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
		return flights;
  }

	@Cacheable("filteredFlights")
	public List<ShortFlightItem> findFlightsByFilter(GetFilteredFlight filter) throws SQLException, ClassNotFoundException {

		List<ShortFlightItem> flights = new ArrayList<>();
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
//			System.out.println("result set is \n->" + resultSet);

			while (this.resultSet.next()) {
				ShortFlightItem item = new ShortFlightItem(
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

// ########################################################################################
// ################################## admin area only #####################################
// ########################################################################################

	// createNewFlight -> add new flight and its details to db as admin
	public void createNewFlight(FlightsItem flight) throws RuntimeException, SQLException, ClassNotFoundException {

		Integer flightId = this.getFlightId(flight.flightNumber());
		if(flightId != 0) {
			throw new BadRequestException("Flight already exists");
		}

		if(flight.itinerary().size() == 1) {
			this.saveAirport(flight.itinerary().getFirst().departureAirport());
			this.saveAirport(flight.itinerary().getFirst().arrivalAirport());
			this.saveLegItem(flight.itinerary().getFirst(), flight.flightNumber());
		} else {
			for (int i = 0; i < flight.itinerary().size(); i++) {
				// run through the itinerary list to save each
				// leg and airport details
				Leg item = flight.itinerary().get(i);
				System.out.println("Itinerary item: " + item);
				this.saveAirport(item.departureAirport());
				this.saveAirport(item.arrivalAirport());
				this.saveLegItem(item, flight.flightNumber());
			}
		}
		this.saveFlight(flight);

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
		Integer aircraftId = 0;

		String airStr = "INSERT INTO aircraft (model, registration, seating_capacity, year_of_manufacture) VALUES (?,?,?,?)";
		String featuresStr = "INSERT INTO aircraft_features (wifi, entertainment, power_outlets, aircraft_id) VALUES (?,?,?,?)";
		String cabinStr = "INSERT INTO cabin_class (economy, business, first, aircraft_id) VALUES (?,?,?,?)";

		try {

			aircraftId = this.getAircraftId(aircraft.registration());
			if(aircraftId != 0) {
				System.out.println("Flight aircraft exists");
				return;
			}

			this.initConnection((byte) 0);
			PreparedStatement preparedAircraft = this.connection.prepareStatement(airStr);
			PreparedStatement preparedFeatures = this.connection.prepareStatement(featuresStr);
			PreparedStatement preparedCabinClass = this.connection.prepareStatement(cabinStr);

			preparedAircraft.setString(1, aircraft.model());
			preparedAircraft.setString(2, aircraft.registration());
			preparedAircraft.setShort(3, aircraft.seatingCapacity());
			preparedAircraft.setShort(4, aircraft.yearOfManufacture());

			this.resultSet = preparedAircraft.executeQuery();
			while (this.resultSet.next()) {
				aircraftId = this.resultSet.getInt("id");
			}

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

			System.out.println("updated count -> " + updated);
			if(updated != 2) {
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

			System.out.println("airportId is -> " + airportId);

			this.initConnection((byte) 0);
			PreparedStatement preparedAirport = this.connection.prepareStatement(airStr);
			PreparedStatement preparedLocation = this.connection.prepareStatement(locationStr);
			PreparedStatement preparedContacts = this.connection.prepareStatement(contactStr);

			preparedAirport.setString(1, airport.code());
			preparedAirport.setString(2, airport.airportName());
			preparedAirport.setString(3, airport.city());
			preparedAirport.setString(4, airport.country());
			preparedAirport.setString(5, String.valueOf(airport.terminal()));
			preparedAirport.setString(6, airport.timezone());

			this.resultSet = preparedAirport.executeQuery();
			while (this.resultSet.next()) {
				airportId = this.resultSet.getInt("id");
			}

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

			System.out.println("updated count -> " + updated);
			if(updated != 2) {
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

		Integer aircraftId;
		Integer priceId = 0;
		int updated = 0;

		String priceSql = "INSERT INTO price_details (flight_number, currency, amount, discount, baggage) VALUES (?,?,?,?,?)";
		String flightSql = "INSERT INTO flights " +
				"(flight_number, airline, aircraft_id, distance, total_duration, price, passenger_count, available_sits) " +
				"VALUES(?,?,?,?,?,?,?,?)";

		try {
			aircraftId = this.getAircraftId(flight.aircraft().registration());
			if(aircraftId != 0) {
				System.out.println("Aircraft already exists");
			} else {
				this.saveAircraft(flight.aircraft());
				aircraftId = this.getAircraftId(flight.aircraft().registration());
				if(aircraftId == 0) {
					throw new ServerErrorException("Failed to save aircraft data.");
				}
			}

			this.initConnection((byte) 0);
			PreparedStatement preparedPrice = this.connection.prepareStatement(priceSql);
			PreparedStatement preparedFlight = this.connection.prepareStatement(flightSql);

			preparedPrice.setString(1, flight.flightNumber());
			preparedPrice.setString(2, flight.price().currency());
			preparedPrice.setFloat(3, flight.price().amount());
			preparedPrice.setInt(4, flight.price().discount());
			preparedPrice.setString(5, flight.price().baggageAllowance());

			this.resultSet = preparedPrice.executeQuery();
			while (this.resultSet.next()) {
				priceId = this.resultSet.getInt("id");
			}

			preparedFlight.setString(1, flight.flightNumber());
			preparedFlight.setString(2, flight.airline());
			preparedFlight.setInt(3, aircraftId);
			preparedFlight.setShort(4, flight.totalDistance());
			preparedFlight.setString(5, flight.totalDuration());
			preparedFlight.setInt(6, priceId);
			preparedFlight.setShort(7,flight.passengerCount());
			preparedFlight.setShort(8, flight.availableSits());

			updated += preparedFlight.executeUpdate();
			if(updated < 1) {
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

	// getAirportId -> return airport id by airport name if it exists
	// and return 0 if not
	private Integer getAirportId(String airportName) throws SQLException, ClassNotFoundException {
		String sql = "SELECT id FROM airport WHERE airport_name=?";
		return this.getItemId(sql, airportName);
	}

	// getAircraftId -> return aircraft id by registration name if it exists
	// and return 0 if not
	private Integer getAircraftId(String registrationNumber) throws SQLException, ClassNotFoundException {
		String sql = "SELECT id FROM aircraft WHERE registration=?";
		return this.getItemId(sql, registrationNumber);
	}

	// getFlightId -> return flight id by flight number if it exists
	// and return 0 if not
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

	// getItemId -> get item id from db (airport, aircraft, flight)
	private Integer getItemId(String sql, String filter) throws SQLException, ClassNotFoundException {
		int itemId = 0;
		try {
			this.initConnection((byte) 0);

			PreparedStatement preparedStatement = this.connection.prepareStatement(sql);
			preparedStatement.setString(1, filter);

			this.resultSet = preparedStatement.executeQuery();
			while(this.resultSet.next()) {
				itemId = this.resultSet.getInt("id");
			}
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

	// initConnection -> init database connection before use any repo method
	private void initConnection(Byte type) throws ClassNotFoundException, SQLException {
		DatabaseDto dto = this.databaseInit.initConnection(type);
		this.connection = dto.connection();
		this.statement = dto.statement();
		this.resultSet = dto.resultSet();
	}

	// closeAndStopDBInteraction -> close any active connection before end interaction with each repository method
	private void closeAndStopDBInteraction() throws SQLException {
		DatabaseDto dto = new DatabaseDto(this.connection, this.statement, this.resultSet);
		this.databaseInit.closeAndStopDBInteraction(dto);
	}

}
