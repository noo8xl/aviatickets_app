package aviatickets.app.purchase;

import aviatickets.app.purchase.dto.request.CreatePurchaseDto;
import aviatickets.app.purchase.entity.Purchase;

import java.sql.Date;
import java.sql.SQLException;
import java.util.List;

interface PurchaseInteraction {

	// create a new purchase as customer
	// -> should return a QR-code as response
	// and also send it to customer email
	void create(CreatePurchaseDto dto) throws SQLException, ClassNotFoundException;

	// confirm purchase -> update status, gen QR, send email
	// -> should return a QR-code as response
	void confirmPurchase(Integer id) throws SQLException, ClassNotFoundException;

	// get purchase detail by customer id
	Purchase getDetails(Integer id) throws SQLException, ClassNotFoundException;

	// get purchase history by customer id
	List<Purchase> getHistory(Integer customerId, Short skip, Short limit) throws SQLException, ClassNotFoundException;

// ##########################################################################################################
// ##################################### ADMIN permission only ##############################################
// ##########################################################################################################

	// get a list of purchase if it wasn't expired
	// if date > current stamp
	List<Purchase> getList(Date date, Short skip, Short limit) throws SQLException, ClassNotFoundException;

	// get a list of all purchase sort by desc date
	List<Purchase> getAll(Short skip, Short limit) throws SQLException, ClassNotFoundException;

	// update some data in bought purchase
	void update(Purchase purchase) throws SQLException, ClassNotFoundException;

	// delete current purchase by id
	void delete(Integer id) throws SQLException, ClassNotFoundException;
}
