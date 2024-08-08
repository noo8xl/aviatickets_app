package aviatickets.app.purchase;

import aviatickets.app.purchase.dto.request.CreatePurchaseDto;
import aviatickets.app.purchase.dto.request.UpdatePurchaseDto;
import aviatickets.app.purchase.entity.Purchase;
import com.google.zxing.WriterException;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.awt.image.BufferedImage;
import java.sql.SQLException;
import java.util.List;


@RestController
@RequestMapping("/purchase")
public class PurchaseController {

	private final PurchaseService purchaseService;

	public PurchaseController(PurchaseService purchaseService) {
		this.purchaseService = purchaseService;
	}

	@ResponseStatus(HttpStatus.CREATED)
	@PostMapping("/create/")
	public void createPurchase(
			@RequestBody CreatePurchaseDto dto) throws SQLException, ClassNotFoundException {
		this.purchaseService.create(dto);
	}

	@ResponseStatus(HttpStatus.ACCEPTED)
	@PostMapping(value = {"/confirm-purchase/{id}/"}, produces = MediaType.IMAGE_PNG_VALUE)
	public ResponseEntity<BufferedImage> confirmPurchase(
			@PathVariable Integer id) throws SQLException, ClassNotFoundException, WriterException {
		return new ResponseEntity<>(this.purchaseService.confirm(id), HttpStatus.OK);
	}

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-details-by-id/{id}/")
	public ResponseEntity<Purchase> getPurchaseDetails(
			@PathVariable Integer id) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.purchaseService.getDetails(id));
	}


	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-history/{customerId}/{skip}/{limit}/")
	public ResponseEntity<List<Purchase>> getPurchaseDetails(
			@PathVariable Integer customerId, @PathVariable Short skip,
			@PathVariable Short limit ) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.purchaseService.getHistory(customerId, skip, limit));
	}

// ##########################################################################################################
// ##################################### ADMIN permission only ##############################################
// ##########################################################################################################


	@ResponseStatus(HttpStatus.ACCEPTED)
	@PutMapping("/update/")
	public void updatePurchaseData(@RequestBody UpdatePurchaseDto dto) throws SQLException, ClassNotFoundException {
		this.purchaseService.update(dto);
	}

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-all/{skip}/{limit}/")
	public ResponseEntity<List<Purchase>> getAll(
			@PathVariable Short skip, @PathVariable Short limit) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.purchaseService.getAll(skip, limit));
	}

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/delete/{id}/")
	public void delete(@PathVariable Integer id) throws SQLException, ClassNotFoundException {
		this.purchaseService.delete(id);
	}

}
