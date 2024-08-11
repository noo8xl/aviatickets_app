package aviatickets.app.actions;

import java.sql.SQLException;
import java.util.List;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import aviatickets.app.actions.entity.ActionLog;

@RequiredArgsConstructor
@Service
public class ActionService implements ActionInterface {
  
  private final ActionInterface actionRepository;

	@Override
  public void saveLog(ActionLog a) throws SQLException, ClassNotFoundException {
    this.actionRepository.saveLog(a);
  }

	@Override
  public List<ActionLog> getLog(Integer skip, Integer limit, Integer customerId) throws SQLException, ClassNotFoundException {
    return this.actionRepository.getLog(skip, limit, customerId);
  }
}
