package aviatickets.app.purchase;

import aviatickets.app.customer.CustomerInterface;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.exception.ServerErrorException;
import aviatickets.app.notification.NotificationInterface;
import aviatickets.app.notification.dto.NewPurchaseDto;
import aviatickets.app.purchase.dto.request.CreatePurchaseDto;
import aviatickets.app.purchase.dto.request.UpdatePurchaseDto;
import aviatickets.app.purchase.entity.Purchase;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.awt.image.BufferedImage;
import java.sql.Date;
import java.util.List;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;

@RequiredArgsConstructor
@Service
class PurchaseService implements PurchaseInterface {

	private final PurchaseInterface purchaseRepository;
	private final NotificationInterface notificationService;
	private final CustomerInterface customerService;

	@Override
	public void create(CreatePurchaseDto dto) {
		try {
			this.purchaseRepository.create(dto);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	// confirm a purchase and return str
	public BufferedImage confirm(Integer id) {
		this.confirmPurchase(id);
		QRCodeWriter barcodeWriter = new QRCodeWriter();

		try {
			BitMatrix bitMatrix = barcodeWriter.encode("test str", BarcodeFormat.QR_CODE, 200, 200);
			return MatrixToImageWriter.toBufferedImage(bitMatrix);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
	public void confirmPurchase(Integer id) {

		Purchase p;
		Customer c;
		NewPurchaseDto dto;
		
		try {
			this.purchaseRepository.confirmPurchase(id);
			p = this.purchaseRepository.getDetails(id);
			c = this.customerService.findOne(p.getCustomerId());
			dto = new NewPurchaseDto(
				c.getUsername(),
					c.getName(),
					new Date(System.currentTimeMillis()),
					p.getId()
			);

//			this.notificationService.sendNewPurchaseEmail(dto);
			System.out.println("here? ----------------------------------");
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
	public Purchase getDetails(Integer id)  {
		try {
			return this.purchaseRepository.getDetails(id);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
	public List<Purchase> getHistory(Integer customerId, Short skip, Short limit) {
		try {
			return this.purchaseRepository.getHistory(customerId, skip, limit);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}


// ##########################################################################################################
// ##################################### ADMIN permission only ##############################################
// ##########################################################################################################

	@Override
	public List<Purchase> getAll(Short skip, Short limit) {
		try {
			return this.purchaseRepository.getAll(skip, limit);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
	public void update(UpdatePurchaseDto dto) {
		try {
			this.purchaseRepository.update(dto);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
	public void delete(Integer id) {
		try {
			this.purchaseRepository.delete(id);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}
	
}
