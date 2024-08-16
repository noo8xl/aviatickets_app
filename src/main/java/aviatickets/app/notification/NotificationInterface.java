package aviatickets.app.notification;

import aviatickets.app.notification.dto.NewNotifDto;

import java.net.URISyntaxException;

public interface NotificationInterface {

	// ############################### customer area ##########################

	// -> send forgot password email OR telegram
	void sendNewPwd(NewNotifDto dto) throws URISyntaxException;

	// -> send 2fa code to confirm customer action
	void sendTwoStepCode(NewNotifDto dto) throws URISyntaxException;

//	// -> send mail at signup
//	void sendRegistrationEmail(String email);

	// send email if purchase has been confirmed
	void sendNewPurchaseEmail(String email, Integer purchaseId);

// ############################### admin area ##########################

	// send mail with custom notification
	// which should be created ONLY by ADMIN user
	void sendCustomNotification(NewNotifDto dto) throws URISyntaxException;

	void sendErrorMessage(String message);
}
