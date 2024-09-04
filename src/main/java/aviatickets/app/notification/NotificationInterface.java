package aviatickets.app.notification;

import aviatickets.app.notification.dto.NewPurchaseDto;
import aviatickets.app.notification.dto.NotificationDto;

import java.net.URISyntaxException;

public interface NotificationInterface {

	// ############################### customer area ##########################

	// -> send forgot password email OR telegram
	void sendNewPwd(NotificationDto dto);

	// -> send 2fa code to confirm customer action
	void sendTwoStepCode(NotificationDto dto);

	// -> send mail at signup
	void sendRegistrationEmail(String email);

	// send email if purchase has been confirmed
	void sendNewPurchaseEmail(NewPurchaseDto dto);

// ############################### administrator area ##########################

	// send mail with custom notification
	// which should be created ONLY by ADMIN user
	void sendCustomNotification(NotificationDto dto) throws URISyntaxException;

	void sendErrorMessage(String message);
}
