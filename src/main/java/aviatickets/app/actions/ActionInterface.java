package aviatickets.app.actions;

import aviatickets.app.actions.entity.ActionLog;

import java.sql.SQLException;
import java.util.List;

public interface ActionInterface {
// save customer actions log
  void saveLog(ActionLog a) throws SQLException, ClassNotFoundException;

// get a list of logs for the ADMIN
	List<ActionLog> getLog(
			Integer skip, Integer limit, Integer customerId
	) throws SQLException, ClassNotFoundException;
} 