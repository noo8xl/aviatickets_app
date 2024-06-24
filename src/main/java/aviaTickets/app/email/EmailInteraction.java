package aviaTickets.app.email;

import aviaTickets.app.customer.dto.ChangePwdDto;

public interface EmailInteraction {
  // -> send forgot password email 
  public void sendForgotPwdEmail(String email);
  // -> send new password to user email
  public void sendChangePwdEmail(ChangePwdDto dto);
  // -> send 2fa email to confirm customer action 
  public void sendTwoStepCode(String email);
  // -> send mail at sign up
  public void sendRegistrationEmail(String email);
} 