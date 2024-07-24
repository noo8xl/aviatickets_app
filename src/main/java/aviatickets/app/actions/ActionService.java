package aviatickets.app.actions;

import java.sql.SQLException;
import java.util.List;

import org.springframework.stereotype.Service;

import aviatickets.app.actions.entity.ActionLog;

@Service
public class ActionService implements ActionInteraction {
  
  private final ActionRepository actionRepository;

  ActionService(ActionRepository actionRepository){
    this.actionRepository = actionRepository;
  }

	@Override
  public void saveCustomerAction(ActionLog a) throws SQLException, ClassNotFoundException {
    this.actionRepository.saveLog(a);
  }

	@Override
  public List<ActionLog> getCustomerLog(
			Integer skip, Integer limit, Integer customerId, Integer adminId
	) throws SQLException, ClassNotFoundException {
    return actionRepository.getLog(skip, limit, customerId, adminId);
  }
}
