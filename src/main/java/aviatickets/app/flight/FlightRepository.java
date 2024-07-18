package aviatickets.app.flight;

import java.sql.*;
import java.time.LocalDateTime;
import java.util.List;

import aviatickets.app.exception.BadRequestException;
import aviatickets.app.exception.ServerErrorException;
import aviatickets.app.flight.dto.request.GetFilteredFlight;
import aviatickets.app.flight.entity.Aircraft;
import aviatickets.app.flight.entity.Airport;
import aviatickets.app.flight.entity.FlightsItem;
import aviatickets.app.flight.entity.Leg;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Repository;

@Repository
public class FlightRepository {

	@Value("${spring.datasource.url}")
	private String dbUrl;

	@Value("${spring.datasource.username}")
	private String dbUsername;

	@Value("${spring.datasource.password}")
	private String dbPassword;

	private Connection connection = null;
	private Statement statement = null;
	private ResultSet resultSet = null;

  public FlightRepository() {	}

// ########################################################################################
// ################################# customer area  #######################################
// ########################################################################################


@Cacheable("hotFlight")
  public List<FlightsItem> getHotFlights() {
    String sqlStr = "SELECT * FROM flights WHERE departure_time=?"; // as example

    return null;
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

    return null;
  }

// ########################################################################################
// ################################## admin area only #####################################
// ########################################################################################

	public void createNewFlight(FlightsItem flight) throws RuntimeException, SQLException, ClassNotFoundException {

		Integer flightId = this.getFlightId(flight.flightNumber());
		if(flightId != 0) {
			throw new BadRequestException("Flight already exists");
		}

		if(flight.itinerary().size() == 1) {
			this.saveAirport(flight.itinerary().getFirst().departureAirport());
			this.saveAirport(flight.itinerary().getFirst().arrivalAirport());
			this.saveLegItem(flight.itinerary().getFirst());
		} else {
			for (int i = 0; i <= flight.itinerary().size(); i++) {
				Leg item = flight.itinerary().get(i);
				this.saveAirport(item.departureAirport());
				this.saveAirport(item.arrivalAirport());
				this.saveLegItem(item);
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

		String airStr = "INSERT INTO aircraft (model, registration, seating_capacity, year_of_manufacture) VALUES (?,?,?,?)";
		String featuresStr = "INSERT INTO aircraft_features (wifi, entertainment, power_outlets, aircraft) VALUES (?,?,?,?)";
		String cabinStr = "INSERT INTO cabin_class (economy, business, first, aircraft) VALUES (?,?,?,?)";

		try {

			Integer aircraftId = this.getAircraftId(aircraft.registration());
			if(aircraftId != 0) {
				System.out.println("Flight aircraft exists");
				return;
			}

			this.initConnection();
			PreparedStatement preparedAircraft = this.connection.prepareStatement(airStr);
			PreparedStatement preparedFeatures = this.connection.prepareStatement(featuresStr);
			PreparedStatement preparedCabinClass = this.connection.prepareStatement(cabinStr);

			preparedAircraft.setString(1, aircraft.model());
			preparedAircraft.setString(2, aircraft.registration());
			preparedAircraft.setInt(3, aircraft.seatingCapacity());
			preparedAircraft.setInt(4, aircraft.yearOfManufacture());

			preparedFeatures.setBoolean(1, aircraft.features().wifi());
			preparedFeatures.setBoolean(2, aircraft.features().inFlightEntertainment());
			preparedFeatures.setBoolean(3, aircraft.features().powerOutlets());
			preparedFeatures.setInt(4, aircraftId);

			preparedCabinClass.setBoolean(1, aircraft.features().cabinClass().economy());
			preparedCabinClass.setBoolean(2, aircraft.features().cabinClass().business());
			preparedCabinClass.setBoolean(3, aircraft.features().cabinClass().first());
			preparedCabinClass.setInt(1, aircraftId);

			updated += preparedAircraft.executeUpdate();
			updated += preparedFeatures.executeUpdate();
			updated += preparedCabinClass.executeUpdate();

			System.out.println("updated count -> " + updated);
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

		int updated = 0;

		String airStr = "INSERT INTO airport (code, airport_name, city, country, terminal, timezone) VALUES (?,?,?,?,?,?)";
		String locationStr = "INSERT INTO airport_location (longitude, latitude, altitude, airport) VALUES (?,?,?,?)";
		String contactStr = "INSERT INTO airport_contact (phone, email, website, airport) VALUES (?,?,?,?)";

		try {

			Integer airportId = this.getAirportId(airport.airportName());
			if(airportId != 0) {
				System.out.println("Flight already exists");
				return;
			}

			this.initConnection();
			PreparedStatement preparedAirport = this.connection.prepareStatement(airStr);
			PreparedStatement preparedLocation = this.connection.prepareStatement(locationStr);
			PreparedStatement preparedContacts = this.connection.prepareStatement(contactStr);

			preparedAirport.setString(1, airport.code());
			preparedAirport.setString(2, airport.airportName());
			preparedAirport.setString(3, airport.city());
			preparedAirport.setString(4, airport.country());
			preparedAirport.setString(5, String.valueOf(airport.terminal()));
			preparedAirport.setString(6, airport.timezone());

			preparedLocation.setString(1, airport.location().longitude());
			preparedLocation.setString(2, airport.location().latitude());
			preparedLocation.setString(3, airport.location().altitude());
			preparedLocation.setInt(4, airportId);

			preparedContacts.setString(1, airport.contacts().phone());
			preparedContacts.setString(2, airport.contacts().email());
			preparedContacts.setString(3, airport.contacts().website());
			preparedContacts.setInt(4, airportId);

			updated += preparedAirport.executeUpdate();
			updated += preparedLocation.executeUpdate();
			updated += preparedContacts.executeUpdate();

			System.out.println("updated count -> " + updated);
			if(updated != 3) {
				throw new ServerErrorException("Failed to create new airport.");
			}
		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
	}

	private void saveLegItem(Leg leg) throws SQLException, ClassNotFoundException {

		String sql = "INSERT INTO leg_details " +
				"(departure_airport, arrival_airport, departure_time, arrival_time, duration, distance, status) " +
				"VALUES (?,?,?,?,?,?,?)";

		try {
			Integer arrivalAirportId = this.getAirportId(leg.arrivalAirport().airportName());
			Integer departureAirportId = this.getAirportId(leg.departureAirport().airportName());

			this.initConnection();
			PreparedStatement preparedLeg = this.connection.prepareStatement(sql);
			preparedLeg.setInt(1, arrivalAirportId);
			preparedLeg.setInt(2, departureAirportId);
			preparedLeg.setDate(3, leg.departureTime());
			preparedLeg.setDate(4, leg.arrivalTime());
			preparedLeg.setString(5, leg.duration());
			preparedLeg.setInt(6, leg.distance());
			preparedLeg.setString(7, leg.status());

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
		Integer flightId = 0;

		this.saveAircraft(flight.aircraft());
		Integer savedAircraftId = this.getAircraftId(flight.aircraft().registration());

		flightId = this.getFlightId(flight.flightNumber());
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

	// getItemId -> get item id from db (airport, aircraft, flight)
	private Integer getItemId(String sql, String filter) throws SQLException, ClassNotFoundException {
		int itemId = 0;
		try {
			this.initConnection();

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
	private void initConnection() throws ClassNotFoundException, SQLException {
		try {
			Class.forName("com.mysql.jdbc.Driver");
			this.connection = DriverManager.getConnection(this.dbUrl, this.dbUsername, this.dbPassword);
			this.statement = this.connection.createStatement();
		} catch (ClassNotFoundException | SQLException e) {
			throw e;
		}
	}

	// closeAndStopDBInteraction -> close any active connection before end interaction with each repository method
	private void closeAndStopDBInteraction() throws SQLException {
		try {
			if (this.resultSet != null) {
				this.resultSet.close();
			}

			if (this.statement != null) {
				this.statement.close();
			}

			if (this.connection != null) {
				this.connection.close();
			}
		} catch (Exception e) {
			throw e;
		}
	}

}
