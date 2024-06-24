package aviaTickets.app.actions;

import java.util.List;

import aviaTickets.app.actions.entity.ActionLog;


// ActionInteraction -> describe an interaction methods 
public interface ActionInteraction {
  // saveLog -> save customer actions log
  public void saveLog(ActionLog a);
  // getLog -> get a list of logs 
  public List<ActionLog> getLog(Integer customerId, Integer skip, Integer lim);
} 