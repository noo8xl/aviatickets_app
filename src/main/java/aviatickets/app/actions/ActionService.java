package aviatickets.app.actions;

import java.util.List;

import org.springframework.stereotype.Service;

import aviatickets.app.actions.entity.ActionLog;

@Service
public class ActionService {
  
  private final ActionRepository actionRepository;

  ActionService(ActionRepository actionRepository){
    this.actionRepository = actionRepository;
  }

  public void saveCustomerAction(ActionLog a) {
    this.actionRepository.saveLog(a);
  }

  public List<ActionLog> getCustomerLog(Integer customerId, Integer skip, Integer lim) {
    return actionRepository.getLog(customerId, skip, lim);
  }
}
