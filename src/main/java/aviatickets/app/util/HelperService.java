package aviatickets.app.util;

import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.entity.Customer;
import org.springframework.stereotype.Component;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Random;

@Component
public class HelperService {

	public HelperService() {}

  // generateUniqueString -> generate new pwd OR 2fa code
  public String generateUniqueString(Integer len) {
		String charList = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
		StringBuilder str = new StringBuilder();
		Random rnd = new Random();
		while (str.length() < len) { // length of the random string.
			int index = (int) (rnd.nextFloat() * charList.length());
			str.append(charList.charAt(index));
		}
		return str.toString();
  }

	// getCustomerEntityFromResultSet -> get Customer entity
	public Customer getCustomerEntityFromResultSet(ResultSet rs) throws SQLException {
		return new Customer(
				rs.getInt("id"),
				rs.getString("name"),
				rs.getString("email"),
				rs.getString("password"),
				rs.getDate("created_at"),
				rs.getString("role"),
				rs.getBoolean("is_banned"),
				rs.getBoolean("two_step_auth_status")
		);
	}

	// getActionEntityFromResultSet -> get ActionLog entity
	public ActionLog getActionEntityFromResultSet(ResultSet rs) throws SQLException {
		return new ActionLog(
				rs.getInt("id"),
				rs.getString("email"),
				rs.getDate("date"),
				rs.getString("action"),
				rs.getInt("customer_id")
		);
	}
}
