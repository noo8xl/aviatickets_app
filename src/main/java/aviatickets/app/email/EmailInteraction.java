package aviatickets.app.email;

import aviatickets.app.customer.dto.ChangePwdDto;

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
} 