package aviatickets.app.purchase;

import aviatickets.app.purchase.entity.Purchase;
import org.springframework.stereotype.Repository;

@Repository
class PurchaseRepository {

	PurchaseRepository() {}

	void save(Purchase p) {
		System.out.println(p);
	}
//
//	void prepareOrder(Purchase p) {
//		System.out.println(p);
//		// prepare order and waiting for the payment confirmation *
//
//		// call this procedure after all *
////		String sql = String.format("CALL update_available_sits(%s)", order.flight().flightNumber());
//
//	}
//
//	void updateOrderStatus(Purchase p) {
//		System.out.println(p);
//		// update status to sold after payment confirmation *
//	}
}
