package aviatickets.app.util;

import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.flight.entity.FlightsItem;
import aviatickets.app.purchase.entity.Purchase;
import lombok.NoArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.ResultSet;
import java.sql.SQLException;


@NoArgsConstructor
@Service
public class SerializationService implements SerializationInterface {


	@Override
	public Customer getCustomerEntityFromResultSet(ResultSet rs) throws SQLException {
		Customer c = new Customer();
	//		Role r = Objects.equals(rs.getString("role"), "USER") ? Role.USER : Role.ADMIN;
	//		System.out.println("role is -> " + r);

		c.setCustomer(
				rs.getInt("id"),
				rs.getString("name"),
				rs.getString("email"),
				rs.getString("password"),
	//				rs.getDate("created_at"),
	//				rs.getDate("updated_at"),
	//				r,
				rs.getBoolean("is_banned"),
				rs.getBoolean("two_step_auth_status")
		);
		return c.getCustomer();
	}

	@Override
	public ActionLog getActionEntityFromResultSet(ResultSet rs) throws SQLException {
		ActionLog a = new ActionLog();
		a.setAction(
				rs.getInt("id"),
				rs.getString("email"),
				rs.getDate("date"),
				rs.getString("action"),
				rs.getInt("customer_id")
		);
		return a.getActionEntity();
	}

	@Override
	public Purchase getPurchaseEntityFromResultSet(ResultSet rs) throws SQLException {
		Purchase p = new Purchase();
		p.setPurchase(
				rs.getInt("id"),
				rs.getString("flight_number"),
				rs.getInt("customer_id"),
				rs.getShort("quantity"),
				rs.getFloat("price"),
				rs.getDate("created_at"),
				rs.getDate("updated_at"),
				rs.getBoolean("payment_status")
		);
		return p.getPurchase();
	}


	@Override
	public FlightsItem getFlightItemEntityFromResultSet(ResultSet rs) throws SQLException {
		FlightsItem flight = new FlightsItem();
	//		flight.setItinerary();
		return flight;
	}
}
