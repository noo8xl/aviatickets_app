package aviatickets.app.purchase;

import aviatickets.app.purchase.dto.request.CreatePurchaseDto;
import aviatickets.app.purchase.entity.Purchase;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping("/purchase")
public class PurchaseController {

	private final PurchaseService purchaseService;

	public PurchaseController(PurchaseService purchaseService) {
		this.purchaseService = purchaseService;
	}

	@ResponseStatus(HttpStatus.CREATED)
	@PostMapping(value = {"/create/"}, produces = MediaType.IMAGE_PNG_VALUE)
	public Purchase createPurchase(@RequestBody CreatePurchaseDto dto) {
		return null;
	}

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-details/{purchaseId}/")
	public Purchase getPurchaseDetails(@PathVariable String purchaseId) {

		return null;
	}


// ##########################################################################################################
// ##################################### ADMIN permission only ##############################################
// ##########################################################################################################


@ResponseStatus(HttpStatus.ACCEPTED)
	@PutMapping("/update-purchase-data/")
	public void updatePurchaseData(@RequestBody Purchase p) {

	}

}
