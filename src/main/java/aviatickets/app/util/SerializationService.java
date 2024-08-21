package aviatickets.app.util;

import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.flight.entity.*;
import aviatickets.app.purchase.entity.Purchase;
import lombok.NoArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.ResultSet;
import java.sql.SQLException;
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
	public FlightsItem getFlightItemEntityFromResultSet(ResultSet rs, Aircraft aircraft, List<Leg> legs, Price price) {

		short totalFlightDistance = 0;
		FlightsItem flight = new FlightsItem();

		try {

			for (Leg leg : legs) {
				totalFlightDistance = (short) (totalFlightDistance + leg.getDistance());
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
			flight.setPrice(price);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return flight.getFlightItem();
	}

	@Override
	public Airport getAirportEntityFromResultSet(ResultSet rs, AirportContacts contacts, Location location) {

		Airport airport = new Airport();
		try {

			airport.setLocation(location);
			airport.setContacts(contacts);


		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return airport.getAirport();
	}

	@Override
	public AirportContacts getAirportContactsEntityFromResultSet(ResultSet rs) {
		AirportContacts airportContacts = new AirportContacts();
		try {

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return airportContacts.getAirportContacts();
	}

	@Override
	public Location getLocationEntityFromResultSet(ResultSet rs) {

		Location location = new Location();

		try {


		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return location.getLocation();
	}

	@Override
	public Aircraft getAircraftEntityFromResultSet(ResultSet rs, AircraftFeatures aircraftFeatures) {

		Aircraft aircraft = new Aircraft();

		try {

			aircraft.setFeatures(aircraftFeatures);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return aircraft.getAircraft();
	}

	@Override
	public AircraftFeatures getAircraftFeaturesEntityFromResultSet(ResultSet rs, CabinClass cabinClass) {

		AircraftFeatures aircraftFeatures = new AircraftFeatures();

		try {

			aircraftFeatures.setCabinClass(cabinClass);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return aircraftFeatures.getAircraftFeatures();
	}

	@Override
	public CabinClass getCabinClassEntityFromResultSet(ResultSet rs) {

		CabinClass cabinClass = new CabinClass();
		try {


		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return cabinClass.getCabinClass();
	}

	@Override
	public Price getPriceEntityFromResultSet(ResultSet rs) {

		Price price = new Price();
		try {

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return price.getPrice();
	}

	@Override
	public Leg getLegEntityFromResultSet(ResultSet rs, Airport departureAirport, Airport arrivalAirport) throws SQLException {


		Leg leg = new Leg();
		try {

			leg.setDepartureAirport(departureAirport);
			leg.setArrivalAirport(arrivalAirport);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return leg.getLegItem();
	}
}















