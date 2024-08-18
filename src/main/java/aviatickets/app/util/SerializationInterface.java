package aviatickets.app.util;

import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.flight.entity.FlightsItem;
import aviatickets.app.purchase.entity.Purchase;

import java.sql.ResultSet;
import java.sql.SQLException;
// ####################################################################################################
// ################################ get entity from sql.resultSet area ################################
// ####################################################################################################


// SerializationInterface -> get entity from sql.resultSet
public interface SerializationInterface {

	ActionLog getActionEntityFromResultSet(ResultSet rs) throws SQLException;

	Customer getCustomerEntityFromResultSet(ResultSet rs) throws SQLException;

	Purchase getPurchaseEntityFromResultSet(ResultSet rs) throws SQLException;

	FlightsItem getFlightItemEntityFromResultSet(ResultSet rs) throws SQLException;

}
