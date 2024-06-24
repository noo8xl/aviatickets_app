package aviaTickets.app.auth;

import aviaTickets.app.auth.dto.ForgotPwdDto;
import aviaTickets.app.auth.dto.SignInDto;
import aviaTickets.app.auth.dto.SignUpDto;
import aviaTickets.app.auth.dto.response.SignInResponse;

public interface AuthInteraction {
  // login 
  public SignInResponse signIn(SignInDto dto);
  // registration
  public void signUp(SignUpDto dto);
  // send new password to user 
  public void forgotPassword(ForgotPwdDto dto);
  // activate user account by link
  public void activateByLink(String link);
}
