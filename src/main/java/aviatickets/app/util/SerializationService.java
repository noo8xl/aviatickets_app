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
			airport.setAirport(
				rs.getInt("id"),
				rs.getString("code"),
				rs.getString("airport_name"),
				rs.getString("city"),
				rs.getString("country"),
				rs.getString("terminal"),
				rs.getString("timezone")
			);

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
			airportContacts.setAirportContacts(
				rs.getString("phone"),
				rs.getString("email"),
				rs.getString("website")
			);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return airportContacts.getAirportContacts();
	}

	@Override
	public Location getLocationEntityFromResultSet(ResultSet rs) {

		Location location = new Location();
		try {
			location.setLocation(
				rs.getString("longitude"),
				rs.getString("latitude"),
				rs.getString("altitude")
			);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return location.getLocation();
	}

	@Override
	public Aircraft getAircraftEntityFromResultSet(ResultSet rs, AircraftFeatures aircraftFeatures) {

		Aircraft aircraft = new Aircraft();
		try {
			aircraft.setAircraft(
				rs.getInt("id"),
				rs.getString("model"),
				rs.getString("registration"),
				rs.getShort("seating_capacity"),
				rs.getShort("year_of_manufacture")
			);

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
			aircraftFeatures.setAircraftFeatures(
				rs.getInt("id"),
				rs.getBoolean("wifi"),
				rs.getBoolean("entertainment"),
				rs.getBoolean("power_outlets")
			);
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
			cabinClass.setCabinClass(
				rs.getBoolean("economy"),
				rs.getBoolean("business"),
				rs.getBoolean("first")
			);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}

		return cabinClass.getCabinClass();
	}

	@Override
	public Price getPriceEntityFromResultSet(ResultSet rs) {

		Price price = new Price();
		try {
			price.setPrice(
				rs.getInt("id"),
				rs.getString("flight_number"),
				rs.getString("currency"),
				rs.getFloat("amount"),
				rs.getShort("discount"),
				rs.getString("baggage")
			);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return price.getPrice();
	}

	@Override
	public Leg getLegEntityFromResultSet(ResultSet rs, Airport departureAirport, Airport arrivalAirport) {

		Leg leg = new Leg();
		try {

			leg.setLeg(
				rs.getInt("id"),
				rs.getDate("departure_time"),
				rs.getDate("arrival_time"),
				rs.getString("duration"),
				rs.getShort("distance"),
				rs.getString("status")
			);

			leg.setDepartureAirport(departureAirport);
			leg.setArrivalAirport(arrivalAirport);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return leg.getLegItem();
	}
}















