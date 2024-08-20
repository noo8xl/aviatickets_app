package aviatickets.app.util;

import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.flight.entity.*;
import aviatickets.app.purchase.entity.Purchase;
import lombok.NoArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.ResultSet;
import java.util.List;


@NoArgsConstructor
@Service
public class SerializationService implements SerializationInterface {


	@Override
	public Customer getCustomerEntityFromResultSet(ResultSet rs) {

		Customer c = new Customer();

		try {

			c.setCustomer(
				rs.getInt("id"),
				rs.getString("name"),
				rs.getString("email"),
				rs.getString("password"),
				rs.getBoolean("is_banned"),
				rs.getBoolean("two_step_auth_status")
			);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return c.getCustomer();
	}

	@Override
	public ActionLog getActionEntityFromResultSet(ResultSet rs) {

		ActionLog a = new ActionLog();

		try {

			a.setAction(
				rs.getInt("id"),
				rs.getString("email"),
				rs.getDate("date"),
				rs.getString("action"),
				rs.getInt("customer_id")
			);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return a.getActionEntity();
	}

	@Override
	public Purchase getPurchaseEntityFromResultSet(ResultSet rs) {

		Purchase p = new Purchase();

		try {

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

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return p.getPurchase();
	}


	@Override
	public FlightsItem getFlightItemEntityFromResultSet(ResultSet rs, Aircraft aircraft, List<Leg> legs) {

		short totalFlightDistance = 0;
		FlightsItem flight = new FlightsItem();

		try {

			for (Leg leg : legs) {
				totalFlightDistance = (short) (totalFlightDistance + leg.distance());
			}

			flight.setFlightItem(
					rs.getInt("id"),
					rs.getString("flight_number"),
					rs.getString("airline"),
					totalFlightDistance,
					rs.getString("total_duration"),
					rs.getShort("passenger_count"),
					rs.getShort("available_sits")
			);

			flight.setAircraft(aircraft);
			flight.setItinerary(legs);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return flight;
	}

	@Override
	public Airport getAirportEntityFromResultSet(ResultSet rs) {

		try {

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return null;
	}

	@Override
	public AirportContacts getAirportContactsEntityFromResultSet(ResultSet rs) {
		try {

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return null;
	}

	@Override
	public Location getLocationEntityFromResultSet(ResultSet rs) {

		try {

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return null;
	}

	@Override
	public Aircraft getAircraftEntityFromResultSet(ResultSet rs) {

		try {

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return null;
	}

	@Override
	public AircraftFeatures getAircraftFeaturesEntityFromResultSet(ResultSet rs) {

		try {

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return null;
	}

	@Override
	public CabinClass getCabinClassEntityFromResultSet(ResultSet rs) {

		try {

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return null;
	}

	@Override
	public Price getPriceEntityFromResultSet(ResultSet rs) {

		try {

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return null;
	}
}
