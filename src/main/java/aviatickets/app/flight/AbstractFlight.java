package aviatickets.app.flight;

import aviatickets.app.flight.entity.*;

import java.sql.SQLException;

// AbstractFlight -> describe internal flightRepo.class methods
abstract class AbstractFlight {

// save Aircraft entity included its features and cabin class entities.
abstract protected void saveAircraft(Aircraft aircraft) throws RuntimeException, SQLException, ClassNotFoundException;

// save Airport entity includes the airport contacts and location.
abstract protected void saveAirport(Airport airport) throws RuntimeException, SQLException, ClassNotFoundException;

// save an itinerary item called leg with all of the Airport and Aircraft entities data.
abstract protected void saveLegItem(Leg leg, String flightNumber) throws SQLException, ClassNotFoundException;

//  ------------------ > get an id of entity.
abstract protected Integer getItemId(String sql, String filter) throws SQLException, ClassNotFoundException;

abstract protected Integer getAirportId(String airportName) throws SQLException, ClassNotFoundException;

abstract protected Integer getAircraftId(String registrationNumber) throws SQLException, ClassNotFoundException;

abstract protected Integer getAircraftIdToDetailedMethod(String registrationNumber) throws SQLException, ClassNotFoundException;

abstract protected Integer getFlightId(String registrationNumber) throws SQLException, ClassNotFoundException;

// get the Airport entity included internal entities such as contacts and location.
abstract protected Airport getAirport(Integer id) throws SQLException;

// get the Aircraft entity included internal entities such as cabin class and features.
abstract protected Aircraft getAircraft(Integer id) throws SQLException, ClassNotFoundException;

// get price data
abstract protected Price getPrice(String flightNumber) throws SQLException, ClassNotFoundException;

// save flight item with all the included date and entities.
abstract protected void saveFlight(FlightsItem flight) throws SQLException, ClassNotFoundException;

// update each part of the flight entity.
abstract protected void updateFlightItem(FlightsItem item) throws SQLException, ClassNotFoundException;

}
