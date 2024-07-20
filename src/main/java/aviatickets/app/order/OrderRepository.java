package aviatickets.app.order;

import aviatickets.app.order.entity.Order;

public class OrderRepository {


	public void save(Order order) {

	}

	public void prepareOrder(Order order) {

		// prepare order and waiting for the payment confirmation *

		// call this procedure after all *
		String sql = String.format("CALL update_available_sits(%s)", order.flight().flightNumber());

	}

	public void updateOrderStatus(Order order) {
		// update status to sold after payment confirmation *
	}
}
