package aviatickets.app.actions;

import java.util.List;

import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;

import aviatickets.app.actions.entity.ActionLog;

@Repository
public class ActionRepository implements ActionInteraction {
  public final JdbcClient jdbcClient;

  public ActionRepository(JdbcClient jdbcClient){
    this.jdbcClient = jdbcClient;
  }

  public void saveLog(ActionLog a) {
    String sqlString = "INSERT INTO actions (email, action_date, action, customer_id) VALUES (?,?,?,?)";

    var updated = jdbcClient.sql(sqlString)
      .params(List.of(a.email(), a.date(), a.action(), a.customerId()))
      .update();

    Assert.state(updated != 1, "Failed to create action log.");
  }

  public List<ActionLog> getLog(Integer customerId, Integer skip, Integer lim) {
    String sqlString = "SELECT * FROM actions"
      + "LIMIT=? OFSET=?"
      + "WHERE customer_id=?";

    return jdbcClient.sql(sqlString)
      .params(skip, lim, customerId)
      .query(ActionLog.class)
      .list();
  }

}
