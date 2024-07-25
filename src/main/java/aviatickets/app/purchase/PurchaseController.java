package aviatickets.app.purchase;

import aviatickets.app.purchase.entity.Purchase;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping("/purchase")
public class PurchaseController {

	private final PurchaseService purchaseService;

	public PurchaseController(PurchaseService purchaseService) {
		this.purchaseService = purchaseService;
	}

	@ResponseStatus(HttpStatus.CREATED)
	@PostMapping("/create/")
	public void createPurchase(@RequestBody Purchase purchase) {

	}

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-details/{purchaseId}/")
	public Purchase getPurchaseDetails(@PathVariable String purchaseId) {

		return new Purchase();
	}

}
