package aviatickets.app.actions;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.actions.entity.ActionLog;


// ActionInteraction -> describe an interaction methods 
interface ActionInteraction {

  // saveCustomerAction -> save customer actions log
  void saveCustomerAction(ActionLog a) throws SQLException, ClassNotFoundException;

  // getCustomerLog -> get a list of logs
	List<ActionLog> getCustomerLog(
			Integer skip, Integer limit, Integer customerId
	) throws SQLException, ClassNotFoundException;
} 