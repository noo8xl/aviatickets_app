package aviatickets.app.actions;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.actions.entity.ActionLog;

public interface ActionInterface {

  // -> save customer actions log
  void saveLog(ActionLog a) throws SQLException, ClassNotFoundException;

  // get a list of logs for admin
	List<ActionLog> getLog(
			Integer skip, Integer limit, Integer customerId
	) throws SQLException, ClassNotFoundException;
} 