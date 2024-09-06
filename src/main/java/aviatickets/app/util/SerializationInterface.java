package aviatickets.app.util;

import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.flight.entity.*;
import aviatickets.app.purchase.entity.Purchase;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

// SerializationInterface -> get entity from sql.resultSet
public interface SerializationInterface {

	ActionLog getActionEntityFromResultSet(ResultSet rs) throws SQLException;

	Customer getCustomerEntityFromResultSet(ResultSet rs) throws SQLException;

	Purchase getPurchaseEntityFromResultSet(ResultSet rs) throws SQLException;

	// #################################################################################################
	// ################################### flight entity area ##########################################

	FlightsItem getFlightItemEntityFromResultSet(
			ResultSet rs, Aircraft aircraft, List<Leg> legs, Price price
	) throws SQLException;

	Airport getAirportEntityFromResultSet(ResultSet rs, AirportContacts contacts, Location location) throws SQLException;
	AirportContacts getAirportContactsEntityFromResultSet(ResultSet rs) throws SQLException;
	Location getLocationEntityFromResultSet(ResultSet rs) throws SQLException;

	Aircraft getAircraftEntityFromResultSet(ResultSet rs, AircraftFeatures aircraftFeatures) throws SQLException;
	AircraftFeatures getAircraftFeaturesEntityFromResultSet(ResultSet rs, CabinClass cabinClass) throws SQLException;
	CabinClass getCabinClassEntityFromResultSet(ResultSet rs) throws SQLException;

	Price getPriceEntityFromResultSet(ResultSet rs) throws SQLException;

	Leg getLegEntityFromResultSet(ResultSet rs, Airport departureAirport, Airport arrivalAirport) throws SQLException;

	// ################################ end of flight entity area ######################################
	// #################################################################################################

}
