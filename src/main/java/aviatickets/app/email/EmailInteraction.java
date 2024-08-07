package aviatickets.app.email;

import aviatickets.app.customer.dto.ChangePwdDto;
import aviatickets.app.email.entity.Notification;

interface EmailInteraction {

  // -> send forgot password email 
  void sendForgotPwdEmail(String email, String pwd);

  // -> send new password to user email
  void sendChangePwdEmail(ChangePwdDto dto);

  // -> send 2fa email to confirm customer action 
  void sendTwoStepCode(String email);

  // -> send mail at signup
  void sendRegistrationEmail(String email);

	// send mail if purchase
	// and send QR-code **
	void sendNewPurchaseEmail(String email);

	// send mail with custom notification
	// which should be created ONLY by ADMIN user
	void sendCustomNotificationEmail(Notification n);
} 