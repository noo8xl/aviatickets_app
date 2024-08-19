package aviatickets.app.purchase;

import aviatickets.app.purchase.dto.request.CreatePurchaseDto;
import aviatickets.app.purchase.dto.request.UpdatePurchaseDto;
import aviatickets.app.purchase.entity.Purchase;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.awt.image.BufferedImage;
import java.util.List;


@RequiredArgsConstructor
@RestController
@RequestMapping("/purchase")
public class PurchaseController {

	private final PurchaseService purchaseService;

	@ResponseStatus(HttpStatus.CREATED)
	@PostMapping("/create/")
	public void createPurchase(@RequestBody CreatePurchaseDto dto) {
		this.purchaseService.create(dto);
	}

	@ResponseStatus(HttpStatus.ACCEPTED)
	@PostMapping(value = {"/confirm-purchase/{id}/"}, produces = MediaType.IMAGE_PNG_VALUE)
	public ResponseEntity<BufferedImage> confirmPurchase(@PathVariable Integer id) {
		return new ResponseEntity<>(this.purchaseService.confirm(id), HttpStatus.OK);
	}

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-details-by-id/{id}/")
	public ResponseEntity<Purchase> getPurchaseDetails(@PathVariable Integer id) {
		return ResponseEntity.ok(this.purchaseService.getDetails(id));
	}


	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-history/{customerId}/{skip}/{limit}/")
	public ResponseEntity<List<Purchase>> getPurchaseHistory(
			@PathVariable Integer customerId, @PathVariable Short skip, @PathVariable Short limit ) {
				return ResponseEntity.ok(this.purchaseService.getHistory(customerId, skip, limit));
	}

// ##########################################################################################################
// ##################################### ADMIN permission only ##############################################
// ##########################################################################################################


	@ResponseStatus(HttpStatus.ACCEPTED)
	@PutMapping("/update/")
	public void updatePurchaseData(@RequestBody UpdatePurchaseDto dto) {
		this.purchaseService.update(dto);
	}

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-all/{skip}/{limit}/")
	public ResponseEntity<List<Purchase>> getAll(
			@PathVariable Short skip, @PathVariable Short limit) {
		return ResponseEntity.ok(this.purchaseService.getAll(skip, limit));
	}

	@ResponseStatus(HttpStatus.OK)
	@DeleteMapping("/delete/{id}/")
	public void delete(@PathVariable Integer id) {
		this.purchaseService.delete(id);
	}

}
