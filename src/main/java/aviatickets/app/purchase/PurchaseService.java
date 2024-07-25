package aviatickets.app.purchase;

import aviatickets.app.purchase.entity.Purchase;
import org.springframework.stereotype.Service;

@Service
class PurchaseService {

	private final PurchaseRepository purchaseRepository;

	PurchaseService(PurchaseRepository purchaseRepository) {
		this.purchaseRepository = purchaseRepository;
	}

	void save(Purchase purchase) {
		// should return QR-code here
		this.purchaseRepository.save(purchase);
	}

}
