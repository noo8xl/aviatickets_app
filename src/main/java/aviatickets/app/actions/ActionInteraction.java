package aviatickets.app.actions;

import java.util.List;

import aviatickets.app.actions.entity.ActionLog;


// ActionInteraction -> describe an interaction methods 
interface ActionInteraction {
  // saveLog -> save customer actions log
  void saveLog(ActionLog a);
  // getLog -> get a list of logs 
  List<ActionLog> getLog(Integer customerId, Integer skip, Integer lim);
} 