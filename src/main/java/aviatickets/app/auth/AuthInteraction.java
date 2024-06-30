package aviatickets.app.auth;

import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;

public interface AuthInteraction {
  // login
  public SignInResponse signIn(SignInDto dto);

  // registration
  public void signUp(SignUpDto dto);

  // send new password to user
  public void forgotPassword(String email);
}
