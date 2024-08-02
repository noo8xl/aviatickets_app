package aviatickets.app.util;

import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.customer.entity.Role;
import org.springframework.stereotype.Component;

import java.awt.image.BufferedImage;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Random;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;

@Component
public class HelperService {

	public HelperService() {}

  // generateUniqueString -> generate new pwd OR 2fa code
  public String generateUniqueString(Integer len) {
		String charList = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
		StringBuilder str = new StringBuilder();
		Random rnd = new Random();
		while (str.length() < len) { // length of the random string.
			int index = (int) (rnd.nextFloat() * charList.length());
			str.append(charList.charAt(index));
		}
		return str.toString();
  }


	// setActionLog -> create new ActionLog entity and save it to db
	public ActionLog setActionLog(String email, String action, Integer customerId) {
		ActionLog a = new  ActionLog();
		a.setAction(null, email, null, action, customerId);
		return a;
	}

	// getCustomerEntityFromResultSet -> get Customer entity
	public Customer getCustomerEntityFromResultSet(ResultSet rs) throws SQLException {
		Customer c = new Customer();
		return c.setCustomer(
				rs.getInt("id"),
				rs.getString("name"),
				rs.getString("email"),
				rs.getString("password"),
				null,
				null,
				rs.getBoolean("is_banned"),
				rs.getBoolean("two_step_auth_status")
		);
	}

	// getActionEntityFromResultSet -> get ActionLog entity
	public ActionLog getActionEntityFromResultSet(ResultSet rs) throws SQLException {
		ActionLog a = new ActionLog();
		return a.setAction(
			rs.getInt("id"),
			rs.getString("email"),
			rs.getDate("date"),
			rs.getString("action"),
			rs.getInt("customer_id")
		);
	}

	public BufferedImage generateQRCode(String barcode)
			throws Exception {

		QRCodeWriter barcodeWriter = new QRCodeWriter();
		BitMatrix bitMatrix =
				barcodeWriter.encode(barcode, BarcodeFormat.QR_CODE, 200, 200);

		return MatrixToImageWriter.toBufferedImage(bitMatrix);
	}
}
