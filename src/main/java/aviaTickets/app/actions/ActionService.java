package aviaTickets.app.actions;

import java.util.List;

import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

import aviaTickets.app.actions.entity.ActionLog;

@Service
public class ActionService {
  
  public final JdbcClient jdbcClient;

  public ActionService(JdbcClient jdbcClient){
    this.jdbcClient = jdbcClient;
  }

  public void saveCustomerAction(ActionLog a) {
    this.saveLog(a);
  }

  public List<ActionLog> getCustomerLog(Integer customerId) {
    return this.getLog(customerId);
  }

  // ### ----------------------------------------------------------------------------------- ###

  private void saveLog(ActionLog a) {
    String sqlString = "INSERT INTO actions (email, action_date, action, customer_id) VALUES (?,?,?,?)";

    var updated = jdbcClient.sql(sqlString)
      .params(List.of(a.email(), a.date(), a.action(), a.customerId()))
      .update();

    Assert.state(updated != 1, "Failed to create action log.");
  }

  private List<ActionLog> getLog(Integer customerId) {
    String sqlString = "SELECT * FROM actions WHERE customer_id=?";
    return jdbcClient.sql(sqlString)
      .param(customerId)
      .query(ActionLog.class)
      .list();
  }
}
