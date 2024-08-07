package aviatickets.app.purchase;

import aviatickets.app.customer.CustomerService;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.email.EmailService;
import aviatickets.app.purchase.dto.request.CreatePurchaseDto;
import aviatickets.app.purchase.dto.request.UpdatePurchaseDto;
import aviatickets.app.purchase.entity.Purchase;
import aviatickets.app.util.HelperService;
import org.springframework.stereotype.Service;

import java.sql.SQLException;
import java.util.List;

@Service
class PurchaseService implements PurchaseInteraction {

	private final PurchaseRepository purchaseRepository;
	private final EmailService emailService;
	private final CustomerService customerService;
	private final HelperService helperService;

	PurchaseService(
			PurchaseRepository purchaseRepository, EmailService emailService,
			CustomerService customerService, HelperService helperService) {
		this.purchaseRepository = purchaseRepository;
		this.emailService = emailService;
		this.customerService = customerService;
		this.helperService = helperService;
	}

	@Override
	public void create(CreatePurchaseDto dto) throws SQLException, ClassNotFoundException {
		this.purchaseRepository.create(dto);
	}

	public void confirm(Integer id) throws SQLException, ClassNotFoundException {
		this.confirmPurchase(id);
//		return this.helperService.generateQRCode("123");
	}

	@Override
	public void confirmPurchase(Integer id) throws SQLException, ClassNotFoundException {
		// -> should return QR-code here after confirmation
		this.purchaseRepository.confirmPurchase(id);
		Purchase p = this.purchaseRepository.getDetails(id);
		Customer c = this.customerService.findOne(p.getCustomerId());
		this.emailService.sendNewPurchaseEmail(c.getUsername());
	}

	@Override
	public Purchase getDetails(Integer id) throws SQLException, ClassNotFoundException {
		return this.purchaseRepository.getDetails(id);
	}


	@Override
	public List<Purchase> getHistory(Integer customerId, Short skip, Short limit) throws SQLException, ClassNotFoundException {
		return this.purchaseRepository.getHistory(customerId, skip, limit);
	}


// ##########################################################################################################
// ##################################### ADMIN permission only ##############################################
// ##########################################################################################################

	@Override
	public List<Purchase> getAll(Short skip, Short limit) throws SQLException, ClassNotFoundException {
		return this.purchaseRepository.getAll(skip, limit);
	}

	@Override
	public void update(UpdatePurchaseDto dto) throws SQLException, ClassNotFoundException {
		this.purchaseRepository.update(dto);
	}

	@Override
	public void delete(Integer id) throws SQLException, ClassNotFoundException {
		this.purchaseRepository.delete(id);
	}
}
